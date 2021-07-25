use std::{collections::HashMap, sync::Arc, thread, time::Duration};

use anyhow::{anyhow, Result};
use crossbeam::channel::Sender;
use dashmap::DashMap;
use itertools::Itertools;
use lsp_server::{Message, Notification};
use lsp_types::{
	notification::Notification as NotificationTrait, DidChangeTextDocumentParams,
	DidOpenTextDocumentParams, Range, Url,
};
use parser::{
	ast::{Decl, Stmt, Token},
	FlatTokens, GetRange, NamespaceScope, Scope, Scopes,
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

		self.validate();
	}

	fn validate(&self) {
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
	}

	fn transpile(&self) -> Result<String> {
		let ast = self
			.ast
			.as_ref()
			.expect("Cannot transpile an unparsed document");

		let scopes = self
			.scopes
			.as_ref()
			.expect("Cannot transpile an unparsed document");

		wgsl_plus::transpile_parsed(ast.clone(), scopes.as_ref())
	}
}

trait FromAstAndUri {
	fn new(ast: Arc<Vec<Decl>>, uri: &Url) -> Self;
}

impl FromAstAndUri for Scopes {
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
