use std::{collections::HashMap, mem, sync::Arc};

use crossbeam::channel::{self, Receiver, Sender};
use dashmap::DashMap;
use gramatika::{ParseStream, Token as _};
use lsp_server::{Message, Notification};
use lsp_types::{notification::Notification as _, DidOpenTextDocumentParams as OpenParams, Url};
use parser_v2::{
	decl::ModuleDecl,
	scopes::{self, Scope},
	traversal::{Visitor, Walk},
	ParseStreamer, SyntaxTree, Token,
};
use serde_json as json;
use wgsl_plus::ResolveImportPath;

use crate::extensions::{UnreadDependency, UnreadDependencyParams};

lazy_static! {
	static ref DOCS: Arc<DashMap<Url, SyntaxTree<'static>>> = Arc::new(DashMap::new());
}

#[derive(Clone, Copy)]
pub enum Status {
	Ready,
	Pending,
}

pub struct Documents<'a> {
	documents: Arc<DashMap<Url, Document<'a>>>,
	ipc: Sender<Message>,
	status_tx: Sender<Status>,
	status: Status,
}

pub struct Document<'a> {
	pub uri: Url,
	pub source: String,
	pub tokens: Vec<Token<'a>>,
	pub ast: Arc<SyntaxTree<'a>>,
	pub deps: Arc<HashMap<&'a str, Url>>,
	pub scopes: Arc<Scope<'a>>,
}

impl<'a> Documents<'a> {
	pub fn new(ipc: Sender<Message>) -> (Self, Receiver<Status>) {
		let (status_tx, status_rx) = channel::unbounded();
		let this = Documents {
			documents: Arc::new(DashMap::new()),
			ipc,
			status_tx,
			status: Status::Ready,
		};

		(this, status_rx)
	}

	pub fn status(&self) -> Status {
		self.status
	}

	pub fn open(&self, params: OpenParams) -> parser_v2::Result<()> {
		self.status_tx.send(Status::Pending).unwrap();

		let uri = params.text_document.uri;
		let text = params.text_document.text;
		let doc = Document::new(text, uri.clone())?;

		let unresolved_deps = doc
			.deps
			.iter()
			.filter(|(_, uri)| !self.documents.contains_key(*uri))
			.collect::<Vec<_>>();

		if unresolved_deps.is_empty() {
			self.documents.insert(uri, doc);
			self.status_tx.send(Status::Ready).unwrap();
		} else {
			for (_, dep) in unresolved_deps {
				let params = UnreadDependencyParams {
					dependency: dep.clone(),
					dependant: uri.clone(),
				};

				self.ipc
					.send(Message::Notification(Notification {
						method: UnreadDependency::METHOD.into(),
						params: json::to_value(&params).unwrap(),
					}))
					.unwrap();
			}

			self.documents.insert(uri, doc);
		}

		Ok(())
	}

	pub fn get(&self, uri: &Url) -> Option<&'a Document<'a>> {
		self.documents
			.get(uri)
			.map(|entry| unsafe { (entry.value() as *const Document).as_ref().unwrap() })
	}
}

impl<'a> Document<'a> {
	fn new(text: String, uri: Url) -> parser_v2::Result<'a, Self> {
		let mut parser = ParseStream::from(text);
		let ast = parser.parse::<SyntaxTree>()?;
		let (source, tokens) = parser.into_inner();

		// FIXME: Seriously regretting going down the lifetime rabbit hole...
		// need to figure out how to do this correctly
		let (scopes, deps) = unsafe {
			let tree = &ast as *const SyntaxTree;
			let tree = mem::transmute::<&'a SyntaxTree<'a>, &'static SyntaxTree<'static>>(
				tree.as_ref().unwrap(),
			);

			let mut deps = DependencyResolver::new(&uri);
			tree.walk(&mut deps);

			(scopes::build(tree), deps)
		};

		Ok(Self {
			uri,
			source,
			tokens,
			ast: Arc::new(ast),
			deps: Arc::new(deps.dependencies),
			scopes,
		})
	}
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
