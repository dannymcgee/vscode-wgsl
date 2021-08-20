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
use parking_lot::RwLock;
use parser_v2::{
	decl::ModuleDecl,
	scopes::{self, Scope},
	traversal::{Visitor, Walk},
	ParseStreamer, SyntaxTree, Token,
};
use ropey::Rope;
use serde_json as json;
use wgsl_plus::ResolveImportPath;

use crate::extensions::{UnreadDependency, UnreadDependencyParams};

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

pub struct Document<'a> {
	pub uri: Url,
	pub source: String,
	pub tokens: Vec<Token<'a>>,
	pub ast: Arc<SyntaxTree<'a>>,
	pub deps: Arc<HashMap<&'a str, Url>>,
	pub scopes: Arc<Scope<'a>>,
	rope: Arc<RwLock<Rope>>,
	status: Status,
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
}

impl<'a> Document<'a> {
	fn new(text: String, uri: Url) -> parser_v2::Result<'a, Self> {
		let (ast, source, tokens) = parse(text)?;
		let (scopes, deps) = build_scope_tree(&ast, &uri);

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

	fn update(&mut self, params: UpdateParams) -> parser_v2::Result<()> {
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

		let (ast, source, tokens) = parse(source.to_string())?;
		self.source = source;
		self.tokens = tokens;

		let (scopes, deps) = build_scope_tree(&ast, &self.uri);
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

		// eprintln!("[Documents] Resolving dependencies : {}", uri);
		let mut deps = DependencyResolver::new(uri);
		tree.walk(&mut deps);

		// eprintln!("[Documents] Building scope tree : {}", uri);
		(scopes::build(tree), deps)
	};

	let deps = Arc::new(deps.dependencies);

	// FIXME: This function does two things instead of one purely because both operations
	// need an AST with an unsafely extended lifetime, and doing that once instead of twice
	// is more sustainable (if you could call it that). When the lifetimes are fixed, this
	// function should probably be split up.
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
