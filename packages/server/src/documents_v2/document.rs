use std::{collections::HashMap, sync::Arc};

use lsp_types::{DidChangeTextDocumentParams as UpdateParams, Url};
use parking_lot::RwLock;
use parser_v2::{scopes::Scope, SyntaxTree, Token};
use ropey::Rope;

use super::Status;

pub struct Document<'a> {
	pub uri: Url,
	pub source: String,
	pub tokens: Vec<Token<'a>>,
	pub ast: Arc<SyntaxTree<'a>>,
	pub deps: Arc<HashMap<&'a str, Url>>,
	pub scopes: Arc<Scope<'a>>,
	pub(super) rope: Arc<RwLock<Rope>>,
	pub(super) status: Status,
}

impl<'a> Document<'a> {
	pub(super) fn new(text: String, uri: Url) -> parser_v2::Result<'a, Self> {
		let (ast, source, tokens) = super::parse(text)?;
		let (scopes, deps) = super::build_scope_tree(&ast, &uri);

		let status = if deps.is_empty() {
			Status::Ready
		} else {
			Status::Pending
		};

		let rope = Arc::new(RwLock::new(Rope::from_str(&source)));

		Ok(Self {
			uri,
			source,
			tokens,
			ast: Arc::new(ast),
			deps,
			scopes,
			rope,
			status,
		})
	}

	pub(super) fn update(&mut self, params: UpdateParams) -> parser_v2::Result<()> {
		let mut source = self.rope.write();
		for update in &params.content_changes {
			let range = update.range.unwrap();

			let start_line = source.line_to_char(range.start.line as usize);
			let edit_start = start_line + range.start.character as usize;

			let end_line = source.line_to_char(range.end.line as usize);
			let edit_end = end_line + range.end.character as usize;

			if edit_end - edit_start > 0 {
				source.remove(edit_start..edit_end);
			}
			source.insert(edit_start, &update.text);
		}

		let (ast, source, tokens) = super::parse(source.to_string())?;
		self.source = source;
		self.tokens = tokens;

		let (scopes, deps) = super::build_scope_tree(&ast, &self.uri);
		self.ast = Arc::new(ast);
		self.deps = deps;
		self.scopes = scopes;

		self.status = if self.deps.is_empty() {
			Status::Ready
		} else {
			Status::Pending
		};

		Ok(())
	}
}
