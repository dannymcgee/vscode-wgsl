use std::{collections::HashMap, sync::Arc};

use gramatika::{ArcStr, Substr};
use lsp_types::{DidChangeTextDocumentParams as UpdateParams, Url};
use parking_lot::RwLock;
use parser_v2::{
	scopes::{self, Scope},
	SyntaxTree, Token,
};
use ropey::Rope;

use super::{DependencyResolver, Status};
use crate::diagnostics::Diagnostics;

#[derive(Debug)]
pub struct Document {
	pub uri: Arc<Url>,
	pub source: ArcStr,
	pub tokens: Arc<[Token]>,
	pub ast: Arc<SyntaxTree>,
	pub deps: Arc<HashMap<Substr, Url>>,
	pub scopes: Arc<Scope>,
	pub(super) rope: Arc<RwLock<Rope>>,
	pub(super) status: Status,
}

impl Document {
	pub(super) fn new(text: String, uri: Url) -> parser_v2::Result<Self> {
		let (ast, source, tokens) = super::parse(text)?;
		let scopes = scopes::build(&ast);
		let deps = DependencyResolver::resolve(&ast, &uri);

		let status = if deps.is_empty() {
			Status::Ready
		} else {
			Status::Pending
		};

		let rope = Arc::new(RwLock::new(Rope::from_str(&source)));

		Ok(Self {
			uri: Arc::new(uri),
			source,
			tokens: tokens.into(),
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
		self.tokens = tokens.into();
		self.deps = DependencyResolver::resolve(&ast, &self.uri);
		self.scopes = scopes::build(&ast);
		self.ast = Arc::new(ast);

		self.status = if self.deps.is_empty() {
			Status::Ready
		} else {
			Status::Pending
		};

		Ok(())
	}

	pub(super) fn validate(&self) {
		Diagnostics::global().validate(self);
	}
}

impl Clone for Document {
	fn clone(&self) -> Self {
		Self {
			uri: Arc::clone(&self.uri),
			source: ArcStr::clone(&self.source),
			tokens: Arc::clone(&self.tokens),
			ast: Arc::clone(&self.ast),
			deps: Arc::clone(&self.deps),
			scopes: Arc::clone(&self.scopes),
			rope: Arc::clone(&self.rope),
			status: self.status,
		}
	}
}
