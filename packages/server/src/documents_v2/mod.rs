use std::{collections::HashMap, mem, sync::Arc};

use crossbeam::channel::{self, Receiver, Sender};
use dashmap::{DashMap, DashSet};
use gramatika::{ParseStream, Token as _};
use itertools::Itertools;
use lsp_server::{Message, Notification};
use lsp_types::{
	notification::Notification as _, DidChangeTextDocumentParams as UpdateParams,
	DidOpenTextDocumentParams as OpenParams, Url,
};
use parser_v2::{
	decl::ModuleDecl,
	scopes::{self, Scope},
	traversal::{Visitor, Walk},
	ParseStreamer, SyntaxTree, Token,
};
use serde_json as json;
use wgsl_plus::ResolveImportPath;

mod document;
mod finder;

use crate::extensions::{UnreadDependency, UnreadDependencyParams};
pub use document::Document;
use finder::{DeclFinder, FindDeclResult};

lazy_static! {
	static ref DOCS: Arc<DashMap<Url, SyntaxTree<'static>>> = Arc::new(DashMap::new());
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Status {
	Ready,
	Pending,
}

pub struct Documents<'a> {
	documents: Arc<DashMap<Url, Document<'a>>>,
	pending: Arc<DashSet<Url>>,
	ipc: Sender<Message>,
	status_tx: Sender<Status>,
}

impl<'a> Documents<'a> {
	pub fn new(ipc: Sender<Message>) -> (Self, Receiver<Status>) {
		let (status_tx, status_rx) = channel::unbounded();
		let this = Documents {
			documents: Arc::new(DashMap::new()),
			pending: Arc::new(DashSet::new()),
			ipc,
			status_tx,
		};

		(this, status_rx)
	}

	pub fn open(&self, params: OpenParams) -> parser_v2::Result<()> {
		self.status_tx.send(Status::Pending).unwrap();

		let uri = params.text_document.uri;
		self.pending.remove(&uri);

		let text = params.text_document.text;
		let doc = Document::new(text, uri.clone())?;

		self.documents.insert(uri, doc);
		self.update_status();

		Ok(())
	}

	pub fn update(&self, params: UpdateParams) -> parser_v2::Result<()> {
		self.status_tx.send(Status::Pending).unwrap();

		let uri = params.text_document.uri.clone();
		if let Some(mut doc) = self.documents.get_mut(&uri) {
			doc.update(params).unwrap();
		}

		self.update_status();

		Ok(())
	}

	pub fn status(&self) -> Status {
		Arc::clone(&self.documents)
			.iter()
			.fold(Status::Ready, |accum, document| {
				if document.status == Status::Pending {
					Status::Pending
				} else {
					accum
				}
			})
	}

	fn update_status(&self) {
		// Could probably refactor this to a `fold`, but given the side effects, I think it
		// makes sense to leave it as an imperative loop.
		let mut accum = Status::Ready;

		// Iterate over documents currently marked pending
		for mut document in Arc::clone(&self.documents)
			.as_ref()
			.iter_mut()
			.filter(|doc| doc.status == Status::Pending)
		{
			// Collect unresolved dependencies
			let unresolved_deps = document
				.deps
				.iter()
				.filter(|(_, uri)| !self.documents.contains_key(*uri))
				.collect_vec();

			if unresolved_deps.is_empty() {
				// Update this document's status if all of its dependencies are resolved
				document.status = Status::Ready;
			} else {
				// Set our "accumulator" to Pending
				accum = Status::Pending;

				for (_, dep_uri) in unresolved_deps {
					// Request this dependency from the client if we haven't already done so
					if !self.pending.contains(dep_uri) {
						let params = UnreadDependencyParams {
							dependency: dep_uri.clone(),
							dependant: document.uri.clone(),
						};

						self.ipc
							.send(Message::Notification(Notification {
								method: UnreadDependency::METHOD.into(),
								params: json::to_value(&params).unwrap(),
							}))
							.unwrap();

						self.pending.insert(dep_uri.clone());
					}
				}
			}
		}

		if accum == Status::Ready {
			self.status_tx.send(Status::Ready).unwrap();
		}
	}

	pub fn get(&self, uri: &Url) -> Option<&'a Document<'a>> {
		self.documents
			.get(uri)
			.map(|entry| unsafe { (entry.value() as *const Document).as_ref().unwrap() })
	}

	pub fn find_decl(&self, uri: Url, token: Token<'a>) -> Option<FindDeclResult<'a>> {
		let document = self.get(&uri)?;

		let tree = document.ast.as_ref();
		let scope = Arc::clone(&document.scopes);
		let deps = Arc::clone(&document.deps)
			.iter()
			.map(|(name, uri)| (*name, self.get(uri).unwrap()))
			.collect();

		let mut finder = DeclFinder::new(scope, deps, token);
		tree.walk(&mut finder);

		finder.result()
	}
}

fn parse<'a>(text: String) -> parser_v2::Result<'a, (SyntaxTree<'a>, String, Vec<Token<'a>>)> {
	let mut parser = ParseStream::from(text);
	let ast = parser.parse::<SyntaxTree>()?;
	let (source, tokens) = parser.into_inner();

	Ok((ast, source, tokens))
}

fn build_scope_tree<'a>(
	ast: &SyntaxTree<'a>,
	uri: &Url,
) -> (Arc<Scope<'a>>, Arc<HashMap<&'a str, Url>>) {
	// FIXME: Seriously regretting going down the lifetime rabbit hole...
	// need to figure out how to do this correctly
	let (scopes, deps) = unsafe {
		let tree = ast as *const SyntaxTree;
		let tree = mem::transmute::<&'a SyntaxTree<'a>, &'static SyntaxTree<'static>>(
			tree.as_ref().unwrap(),
		);

		let mut deps = DependencyResolver::new(uri);
		tree.walk(&mut deps);

		(scopes::build(tree), deps)
	};

	let deps = Arc::new(deps.dependencies);

	// FIXME: This function does two things instead of one purely because both operations
	// need an AST with an unsafely extended lifetime, and doing that once is preferable to
	// doing it twice. When the lifetimes are fixed, this function should probably be split up.
	(scopes, deps)
}

struct DependencyResolver<'a> {
	source: Url,
	dependencies: HashMap<&'a str, Url>,
}

impl<'a> DependencyResolver<'a> {
	fn new(source: &Url) -> Self {
		Self {
			source: source.clone(),
			dependencies: HashMap::new(),
		}
	}
}

impl<'a> Visitor<'a> for DependencyResolver<'a> {
	fn visit_module_decl(&mut self, decl: &'a ModuleDecl<'a>) {
		let name = decl.name.lexeme();
		let path = decl.path.lexeme();
		let slice = &path[1..path.len() - 1];
		let uri = self.source.resolve_import(slice);

		self.dependencies.insert(name, uri);
	}
}
