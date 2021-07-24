use std::{collections::HashMap, slice::Iter, sync::Arc, thread, time::Duration};

use anyhow::{anyhow, Context, Result};
use crossbeam::channel::Sender;
use dashmap::DashMap;
use itertools::Itertools;
use lsp_server::{Message, Notification};
use lsp_types::{
	notification::Notification as NotificationTrait, DidChangeTextDocumentParams,
	DidOpenTextDocumentParams, Range, Url,
};
use parser::{
	ast::{Decl, Expr, PostfixExpr, PrimaryExpr, Stmt, Token},
	AstNode, FlatTokens, GetRange, IsWithin, ParentGranularity, ParentOfRange, Rename,
};
use ropey::Rope;
use serde_json as json;

use crate::{
	diagnostics::{self, ErrorKind},
	extensions::{UnreadDependency, UnreadDependencyParams},
};

lazy_static! {
	static ref DOCS: Arc<DashMap<Url, Document>> = Arc::new(DashMap::default());
	static ref PENDING: Arc<DashMap<Url, Document>> = Arc::new(DashMap::default());
}

macro_rules! docs {
	(!$pending:ident ? $docs:ident[$key:ident] $adapter:ident => $func:ident()) => {
		loop {
			if !$pending.contains_key($key) && $docs.contains_key($key) {
				break $docs.get($key).$adapter(|entry| entry.value().$func());
			}
			thread::sleep(Duration::from_millis(10));
		}
	};
}

pub fn open(params: DidOpenTextDocumentParams, tx: Sender<Message>) {
	thread::spawn(move || {
		let uri = &params.text_document.uri;
		if DOCS.contains_key(uri) {
			return;
		}

		let (ready, document) = Document::new(uri, &params.text_document.text, tx);

		if !ready {
			PENDING.insert(uri.clone(), document);
		} else {
			DOCS.insert(uri.clone(), document);

			let done = PENDING
				.iter()
				.filter_map(|entry| {
					let key = entry.key();
					let doc = entry.value();

					if doc
						.deps
						.as_ref()
						.unwrap()
						.iter()
						.all(|uri| DOCS.contains_key(uri))
					{
						Some(key.clone())
					} else {
						None
					}
				})
				.collect_vec();

			for key in done {
				let (_, mut doc) = PENDING.remove(&key).unwrap();
				doc.update_deps();

				DOCS.insert(key, doc);
			}
		}
	});
}

pub fn close(uri: &Url) {
	DOCS.remove(uri);
}

pub fn read(uri: &Url) -> Option<String> {
	docs!(!PENDING ? DOCS[uri] map => read())
}

pub fn parse(uri: &Url) -> Option<Arc<Vec<Decl>>> {
	docs!(!PENDING ? DOCS[uri] and_then => parse())
}

pub fn scopes(uri: &Url) -> Option<Arc<Scopes>> {
	docs!(!PENDING ? DOCS[uri] and_then => scopes())
}

pub fn tokens(uri: &Url) -> Option<Arc<Vec<Token>>> {
	docs!(!PENDING ? DOCS[uri] and_then => tokens())
}

pub fn update(params: &DidChangeTextDocumentParams) -> Result<()> {
	if let Some(mut doc) = DOCS.get_mut(&params.text_document.uri) {
		doc.value_mut().update(&params)?;

		Ok(())
	} else {
		Err(anyhow!(
			"No entry found for uri {:?}",
			params.text_document.uri
		))
	}
}

#[derive(Debug)]
struct Document {
	uri: Url,
	text: Rope,
	deps: Option<Arc<Vec<Url>>>,
	ast: Option<Arc<Vec<Decl>>>,
	scopes: Option<Arc<Scopes>>,
	tokens: Option<Arc<Vec<Token>>>,
}

impl Document {
	fn new(uri: &Url, input: &str, tx: Sender<Message>) -> (bool, Self) {
		let text: Rope = input.into();
		let ast = match parser::parse_ast(input) {
			Ok(ast) => {
				diagnostics::clear_errors(uri, Some(ErrorKind::ParseError));
				Some(Arc::new(ast))
			}
			Err(err) => {
				diagnostics::clear_errors(uri, Some(ErrorKind::ParseError));
				diagnostics::report_error(uri, err, ErrorKind::ParseError);

				None
			}
		};

		let mut ready = true;
		let deps = ast.as_ref().map(|ast| {
			let deps = ast.resolve_deps(uri);

			for dep in deps.iter() {
				if !DOCS.contains_key(&dep) {
					ready = false;

					let params = UnreadDependencyParams {
						dependency: dep.clone(),
						dependant: uri.clone(),
					};

					tx.send(Message::Notification(Notification {
						method: UnreadDependency::METHOD.into(),
						params: json::to_value(&params).unwrap(),
					}))
					.unwrap();
				}
			}

			Arc::new(deps)
		});

		let scopes = ast
			.as_ref()
			.map(|ast| Arc::new(Scopes::new(ast.clone(), uri)));

		let tokens = ast.as_ref().map(|ast| {
			let mut tokens = vec![];
			ast.flat_tokens(&mut tokens);

			Arc::new(tokens)
		});

		let result = Self {
			uri: uri.clone(),
			text,
			ast,
			deps,
			scopes,
			tokens,
		};

		if ready {
			result.validate();
		}

		(ready, result)
	}

	fn read(&self) -> String {
		self.text.clone().into()
	}

	fn parse(&self) -> Option<Arc<Vec<Decl>>> {
		self.ast.as_ref().map(|ast| Arc::clone(&ast))
	}

	fn scopes(&self) -> Option<Arc<Scopes>> {
		self.scopes.as_ref().map(|scopes| Arc::clone(&scopes))
	}

	fn tokens(&self) -> Option<Arc<Vec<Token>>> {
		self.tokens.as_ref().map(|tokens| Arc::clone(&tokens))
	}

	fn update(&mut self, params: &DidChangeTextDocumentParams) -> Result<()> {
		for update in &params.content_changes {
			let range = update.range.unwrap();

			let start_line = self.text.line_to_char(range.start.line as usize);
			let edit_start = start_line + range.start.character as usize;

			let end_line = self.text.line_to_char(range.end.line as usize);
			let edit_end = end_line + range.end.character as usize;

			if edit_end - edit_start > 0 {
				self.text.remove(edit_start..edit_end);
			}
			self.text.insert(edit_start, &update.text);
		}

		self.ast = match parser::parse_ast(&self.text.to_string()) {
			Ok(ast) => {
				diagnostics::clear_errors(&self.uri, Some(ErrorKind::ParseError));
				Some(Arc::new(ast))
			}
			Err(err) => {
				diagnostics::clear_errors(&self.uri, Some(ErrorKind::ParseError));
				diagnostics::report_error(&self.uri, err, ErrorKind::ParseError);

				None
			}
		};

		self.scopes = self
			.ast
			.as_ref()
			.map(|ast| Arc::new(Scopes::new(ast.clone(), &self.uri)));

		self.tokens = self.ast.as_ref().map(|ast| {
			let mut tokens = vec![];
			ast.flat_tokens(&mut tokens);

			Arc::new(tokens)
		});

		self.validate();

		Ok(())
	}

	fn update_deps(&mut self) {
		self.scopes = self
			.ast
			.as_ref()
			.map(|ast| Arc::new(Scopes::new(ast.clone(), &self.uri)));

		self.validate().unwrap();
	}

	fn validate(&self) -> Result<()> {
		if let Some(ref ast) = self.ast {
			if ast.iter().any(|decl| matches!(decl, Decl::Extension(_))) {
				diagnostics::clear_errors(&self.uri, Some(ErrorKind::NagaValidationError));
				diagnostics::clear_errors(&self.uri, Some(ErrorKind::TranspileError));

				match self.transpile() {
					Ok(text) => {
						// diagnostics::validate(self.uri.clone(), text);
						eprintln!("Transpile result:");
						eprintln!("---------------------------------------------------------");
						eprintln!("{}", text);
						eprintln!("---------------------------------------------------------");
					}
					Err(err) => {
						diagnostics::report_error(&self.uri, err, ErrorKind::TranspileError);
					}
				}
			} else {
				diagnostics::clear_errors(&self.uri, Some(ErrorKind::TranspileError));
				diagnostics::validate(self.uri.clone(), self.text.to_string());
			}
		}

		Ok(())
	}

	fn transpile(&self) -> Result<String> {
		let ast = self
			.ast
			.as_ref()
			.context("Cannot transpile an unparsed document")?;

		let scopes = self
			.scopes
			.as_ref()
			.context("Cannot transpile an unparsed document")?;

		let (result, transformed) =
			ast.iter()
				.fold((Ok(()), vec![]), |(result, mut tree), decl| match decl {
					Decl::Extension(_) => (result, tree),
					Decl::Module(module) => {
						let (module_name, _) = module.name.borrow_inner();
						match scopes.namespaces.get(module_name) {
							Some(inner) => {
								let decls = inner.scope.iter().filter_map(|(decl_name, decl)| {
									match decl.as_ref() {
										Decl::Extension(_) => None,
										Decl::Struct(inner) => {
											let new_name = format!("{}_{}", module_name, decl_name);
											Some(Decl::Struct(inner.clone().rename(new_name)))
										}
										Decl::Function(inner) => {
											let new_name = format!("{}_{}", module_name, decl_name);
											Some(Decl::Function(inner.clone().rename(new_name)))
										}
										_ => unreachable!(),
									}
								});
								tree.extend(decls);
								(result, tree)
							}
							None => {
								let err = anyhow!("Failed to resolved module `{}`", module_name);
								(Err(err), tree)
							}
						}
					}
					other => {
						tree.push(other.clone());
						(result, tree)
					}
				});

		result.map(|_| {
			transformed
				.iter()
				.map(|decl| format!("{}", decl))
				.join("\n\n")
		})
	}
}

pub type Scope = HashMap<String, Arc<Decl>>;

#[derive(Debug)]
pub struct NamespaceScope {
	pub uri: Url,
	pub scope: Scope,
}

impl NamespaceScope {
	pub fn get(&self, key: &str) -> Option<ScopeDecl> {
		self.scope
			.get(key)
			.map(|decl| ScopeDecl::namespace(self.uri.clone(), Arc::clone(decl)))
	}
}

// TODO: Consider moving this into the `parser` crate
#[derive(Debug)]
pub struct Scopes {
	ast: Arc<Vec<Decl>>,
	namespaces: HashMap<String, NamespaceScope>,
	inner: Vec<(Range, Arc<Scope>)>,
}

#[derive(Debug)]
pub struct ScopeDecl {
	pub uri: Option<Url>,
	pub decl: Arc<Decl>,
}

impl ScopeDecl {
	pub fn new(decl: Arc<Decl>) -> Self {
		ScopeDecl { uri: None, decl }
	}

	pub fn namespace(uri: Url, decl: Arc<Decl>) -> Self {
		ScopeDecl {
			uri: Some(uri),
			decl,
		}
	}
}

impl Scopes {
	fn new(ast: Arc<Vec<Decl>>, uri: &Url) -> Self {
		if ast.is_empty() {
			return Self {
				ast: Arc::new(vec![]),
				namespaces: HashMap::default(),
				inner: vec![],
			};
		}

		let mut scopes: Vec<(Range, Arc<Scope>)> = vec![];

		let global_start = ast.first().unwrap().range().start;
		let global_end = ast.last().unwrap().range().end;

		let global_range = Range {
			start: global_start,
			end: global_end,
		};

		let mut global_scopes = HashMap::new();
		let mut namespaces = HashMap::new();

		for decl in ast.iter() {
			match decl {
				Decl::Module(decl) => {
					let name = decl.name.to_string();
					let (path, _) = decl.path.borrow_inner();
					let dep_uri = uri.resolve_import(&path[1..path.len() - 1]);

					if let Some(dep_ast) =
						DOCS.get(&dep_uri).and_then(|entry| entry.value().parse())
					{
						let scope = dep_ast
							.exports()
							.into_iter()
							.map(|decl| (decl.ident().to_string(), Arc::new(decl)))
							.collect();

						namespaces.insert(
							name,
							NamespaceScope {
								uri: dep_uri.clone(),
								scope,
							},
						);
					}
				}
				_ => {
					let name = decl.ident().to_string();
					global_scopes.insert(name, Arc::new(decl.clone())); // TODO
				}
			}
		}

		scopes.push((global_range, Arc::new(global_scopes)));

		for decl in ast.iter() {
			if let Decl::Function(inner) = decl {
				let mut current_scopes = HashMap::new();
				let range = inner.range;

				for param in &inner.signature.params {
					let name = param.name.to_string();
					current_scopes.insert(name, Arc::new(Decl::Param(param.clone()))); // TODO
				}

				let (_, stmts) = &inner.body;
				for stmt in stmts {
					if let Stmt::Variable(var_decl) = stmt {
						let name = var_decl.name.to_string();
						current_scopes.insert(name, Arc::new(Decl::Var(var_decl.clone()))); // TODO
					}
				}

				scopes.push((range, Arc::new(current_scopes)));
			}
		}

		scopes.reverse();

		Self {
			ast,
			namespaces,
			inner: scopes,
		}
	}

	pub fn find_decl(&self, token: &Token) -> Option<ScopeDecl> {
		let (name, token_range) = token.borrow_inner();
		let parent = self
			.ast
			.parent_of(&token_range, ParentGranularity::Expr)
			.unwrap();

		// Special handling for dot-accessed fields of variables that bind to structs
		if let AstNode::Expr(Expr::Singular(ref expr)) = &parent {
			if let Some(PostfixExpr::Dot { ident, range, .. }) = &expr.postfix {
				let (postfix_ident, _) = ident.borrow_inner();

				if token_range.is_within(&range) && postfix_ident == name {
					let var_name = if let PrimaryExpr::Identifier(ref token) = expr.expr {
						token
					} else {
						return None;
					};

					let var_decl = self.find_decl(var_name)?;
					let struct_name = match var_decl.decl.as_ref() {
						Decl::Var(decl) | Decl::Const(decl) => {
							decl.type_decl.as_ref().map(|ty| &ty.name)
						}
						Decl::Param(decl) => Some(&decl.type_decl.name),
						_ => None,
					}?;

					return self.find_decl(struct_name).and_then(|decl| {
						let uri = decl.uri;
						let decl = match decl.decl.as_ref() {
							Decl::Struct(struct_decl) => {
								struct_decl.body.iter().find_map(|field| {
									if field.name.borrow_inner().0 == name {
										Some(Arc::new(Decl::Field(field.clone())))
									} else {
										None
									}
								})
							}
							_ => None,
						};

						decl.map(|decl| ScopeDecl { uri, decl })
					});
				}
			}
		}

		// Special handling for namespaced types
		match &parent {
			AstNode::Decl(ref decl) => match decl {
				Decl::Var(inner) => {
					if let Some((namespace, _)) = inner.type_decl.as_ref().and_then(|ty| {
						if ty.name.range() == token_range {
							ty.namespace.as_ref().map(|token| token.borrow_inner())
						} else {
							None
						}
					}) {
						return self
							.namespaces
							.get(namespace)
							.and_then(|scope| scope.get(name));
					}
				}
				Decl::Param(_inner) => {
					// TODO
				}
				_ => {}
			},
			AstNode::Stmt(ref _stmt) => {
				// TODO
			}
			AstNode::Expr(ref _expr) => {
				// TODO
			}
		}

		// Find the inner-most scope that encapsulates the token's range, and look up the
		// declaration of the token's identifier
		self.inner.iter().find_map(|(scope_range, scope)| {
			if token_range.is_within(scope_range) {
				scope.get(name).map(|decl| ScopeDecl::new(Arc::clone(decl)))
			} else {
				None
			}
		})
	}

	pub fn iter(&self) -> Iter<'_, (Range, Arc<Scope>)> {
		self.inner.iter()
	}
}

trait ResolveDeps {
	fn resolve_deps(&self, source: &Url) -> Vec<Url>;
}

impl ResolveDeps for Vec<Decl> {
	fn resolve_deps(&self, source: &Url) -> Vec<Url> {
		self.iter()
			.filter_map(|decl| match decl {
				Decl::Module(inner) => {
					let (quoted, _) = inner.path.borrow_inner();
					let slice = &quoted[1..quoted.len() - 1];
					let uri = source.resolve_import(slice);

					Some(uri)
				}
				_ => None,
			})
			.collect()
	}
}

trait GetExports {
	fn exports(&self) -> Vec<Decl>;
}

impl GetExports for Vec<Decl> {
	fn exports(&self) -> Vec<Decl> {
		self.iter()
			.filter(|decl| match decl {
				Decl::Struct(inner) => inner.storage_modifier.as_ref().map_or(false, |token| {
					let (kw, _) = token.borrow_inner();
					kw == "export"
				}),
				Decl::Function(inner) => inner.storage_modifier.as_ref().map_or(false, |token| {
					let (kw, _) = token.borrow_inner();
					kw == "export"
				}),
				_ => false,
			})
			.cloned()
			.collect()
	}
}

trait ResolveImportPath {
	fn resolve_import(&self, path: &str) -> Url;
}

impl ResolveImportPath for Url {
	fn resolve_import(&self, path: &str) -> Self {
		let mut resolved = self.clone();
		{
			let resolved_segs = &mut resolved.path_segments_mut().unwrap();
			resolved_segs.pop();

			let segments = path.split('/');
			for segment in segments {
				match segment {
					"." => {}
					".." => {
						resolved_segs.pop();
					}
					other => {
						resolved_segs.push(other);
					}
				}
			}
		}

		resolved.set_path(&format!("{}.wgsl", resolved.path()));

		resolved
	}
}
