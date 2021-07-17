use std::{collections::HashMap, slice::Iter, sync::Arc};

use anyhow::{anyhow, Result};
use dashmap::DashMap;
use lsp_types::{DidChangeTextDocumentParams, DidOpenTextDocumentParams, Range, Url};
use parser::{
	ast::{Decl, Stmt, Token},
	FlatTokens, GetRange, IsWithin, ParentGranularity, ParentOfRange,
};
use ropey::Rope;

lazy_static! {
	static ref DOCS: Arc<DashMap<Url, Document>> = Arc::new(DashMap::default());
}

pub fn open(params: &DidOpenTextDocumentParams) -> Result<()> {
	let document = Document::new(&params.text_document.text)?;
	DOCS.insert(params.text_document.uri.clone(), document);

	Ok(())
}

pub fn close(uri: &Url) {
	DOCS.remove(uri);
}

#[allow(dead_code)]
pub fn read(uri: &Url) -> Option<String> {
	DOCS.get(uri).map(|entry| entry.value().read())
}

pub fn parse(uri: &Url) -> Option<Arc<Vec<Decl>>> {
	DOCS.get(uri).map(|entry| entry.value().parse())
}

pub fn scopes(uri: &Url) -> Option<Arc<Scopes>> {
	DOCS.get(uri).map(|entry| entry.value().scopes())
}

pub fn tokens(uri: &Url) -> Option<Arc<Vec<Token>>> {
	DOCS.get(uri).map(|entry| entry.value().tokens())
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
	text: Rope,
	ast: Arc<Vec<Decl>>,
	scopes: Arc<Scopes>,
	tokens: Arc<Vec<Token>>,
}

impl Document {
	fn new(input: &str) -> Result<Self> {
		let text: Rope = input.into();
		let ast = Arc::new(parser::parse_ast(input)?);
		let scopes = Arc::new(Scopes::from(ast.clone()));

		let mut tokens = vec![];
		ast.flat_tokens(&mut tokens);

		let tokens = Arc::new(tokens);

		Ok(Self {
			text,
			ast,
			scopes,
			tokens,
		})
	}

	fn read(&self) -> String {
		self.text.clone().into()
	}

	fn parse(&self) -> Arc<Vec<Decl>> {
		Arc::clone(&self.ast)
	}

	fn scopes(&self) -> Arc<Scopes> {
		Arc::clone(&self.scopes)
	}

	fn tokens(&self) -> Arc<Vec<Token>> {
		Arc::clone(&self.tokens)
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

		// TODO: Incremental parsing
		for range in params
			.content_changes
			.iter()
			.filter_map(|update| update.range)
		{
			if let Some(affected_node) = self.ast.parent_of(&range, ParentGranularity::Expr) {
				eprintln!("Changed node: {:#?}", affected_node);
			}
		}
		self.ast = Arc::new(parser::parse_ast(&self.text.to_string())?);

		let mut tokens = vec![];
		self.ast.flat_tokens(&mut tokens);

		self.tokens = Arc::new(tokens);

		Ok(())
	}
}

pub type Scope = HashMap<String, Arc<Decl>>;

// TODO: Consider moving this into the `parser` crate
#[derive(Debug)]
pub struct Scopes {
	ast: Arc<Vec<Decl>>,
	inner: Vec<(Range, Arc<Scope>)>,
}

impl Scopes {
	pub fn find_decl(&self, token: &Token) -> Option<Arc<Decl>> {
		use parser::{
			ast::{Expr, PostfixExpr, PrimaryExpr},
			AstNode,
		};

		let (name, token_range) = token.borrow_inner();
		let parent = self
			.ast
			.parent_of(&token_range, ParentGranularity::Expr)
			.unwrap();

		if let AstNode::Expr(Expr::Singular(ref expr)) = &parent {
			if let Some(PostfixExpr::Dot { ident, range, .. }) = &expr.postfix {
				let (postfix_ident, _) = ident.borrow_inner();

				if token_range.is_within(&range) && postfix_ident == name {
					let var_name = if let PrimaryExpr::Identifier(ref token) = expr.expr {
						token.borrow_inner().0
					} else {
						return None;
					};

					let var_decl = self.inner.iter().find_map(|(scope_range, scope)| {
						if range.is_within(scope_range) {
							scope.get(var_name).map(|decl| Arc::clone(decl))
						} else {
							None
						}
					})?;

					let struct_name = match *var_decl.as_ref() {
						Decl::Var(ref decl) | Decl::Const(ref decl) => {
							if let Some(ref ty) = decl.type_decl {
								ty.name.borrow_inner().0
							} else {
								return None;
							}
						}
						Decl::Param(ref decl) => decl.type_decl.name.borrow_inner().0,
						_ => return None,
					};

					if let Some(Decl::Struct(struct_decl)) = self
						.ast
						.iter()
						.find(|decl| decl.ident().borrow_inner().0 == struct_name)
					{
						return struct_decl.body.iter().find_map(|field| {
							if field.name.borrow_inner().0 == name {
								Some(Arc::new(Decl::Field(field.clone()))) // TODO
							} else {
								None
							}
						});
					} else {
						return None;
					}
				}
			}
		}

		self.inner.iter().find_map(|(scope_range, scope)| {
			if token_range.is_within(scope_range) {
				scope.get(name).map(|decl| Arc::clone(decl))
			} else {
				None
			}
		})
	}

	pub fn iter(&self) -> Iter<'_, (Range, Arc<Scope>)> {
		self.inner.iter()
	}
}

impl From<Arc<Vec<Decl>>> for Scopes {
	fn from(ast: Arc<Vec<Decl>>) -> Self {
		if ast.is_empty() {
			return Self {
				ast: Arc::new(vec![]),
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

		for decl in ast.iter() {
			let name = decl.ident().to_string();
			global_scopes.insert(name, Arc::new(decl.clone())); // TODO
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

		Self { ast, inner: scopes }
	}
}
