use std::{collections::HashMap, sync::Arc};

use crossbeam::channel::{self, Receiver, Sender};
use dashmap::{DashMap, DashSet};
use gramatika::{once_cell::sync::OnceCell, ArcStr, ParseStream, Substr, Token as _};
use itertools::Itertools;
use lsp_server::{Message, Notification};
use lsp_types::{
	notification::Notification as _, DidChangeTextDocumentParams as UpdateParams,
	DidOpenTextDocumentParams as OpenParams, Url,
};
use parking_lot::RwLock;
use parser_v2::{
	decl::ModuleDecl,
	traversal::{Visitor, Walk},
	ParseStreamer, SyntaxTree, Token,
};
use serde_json as json;
use wgsl_plus::ResolveImportPath;

mod document;
mod finder;

use crate::{
	configuration::Configuration,
	lsp_extensions::{UnreadDependency, UnreadDependencyParams},
};

pub use document::Document;
use finder::DeclFinder;
pub use finder::FindDeclResult;

static INSTANCE: OnceCell<Documents> = OnceCell::new();

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Status {
	Ready,
	Pending,
}

#[derive(Debug)]
pub struct Documents {
	documents: Arc<DashMap<Url, Document>>,
	pending: Arc<DashSet<Url>>,
	ipc: Sender<Message>,
	status_tx: Sender<Status>,
	config: Arc<RwLock<Option<Configuration>>>,
}

impl Documents {
	pub fn global() -> Self {
		INSTANCE.get().unwrap().clone()
	}

	pub fn new(ipc: Sender<Message>) -> (Self, Receiver<Status>) {
		let (status_tx, status_rx) = channel::unbounded();
		INSTANCE
			.set(Documents {
				documents: Arc::new(DashMap::new()),
				pending: Arc::new(DashSet::new()),
				ipc,
				status_tx,
				config: Arc::new(RwLock::new(None)),
			})
			.unwrap();

		(Self::global(), status_rx)
	}

	pub fn configure(&self, config: Configuration) {
		// TODO
		eprintln!("Received configuration: {:#?}", config);
		*self.config.write() = Some(config);
	}

	pub fn open(&self, params: OpenParams) -> anyhow::Result<()> {
		self.status_tx.send(Status::Pending).unwrap();

		let uri = params.text_document.uri;
		self.pending.remove(&uri);

		let text = params.text_document.text;
		let doc = Document::new(text, uri.clone())?;

		if doc.status == Status::Ready {
			doc.validate();
		}

		self.documents.insert(uri, doc);
		self.update_status();

		Ok(())
	}

	pub fn update(&self, params: UpdateParams) -> parser_v2::Result<()> {
		self.status_tx.send(Status::Pending).unwrap();

		let uri = params.text_document.uri.clone();
		if let Some(mut doc) = self.documents.get_mut(&uri) {
			doc.update(params).unwrap();

			if doc.status == Status::Ready {
				doc.validate();
			}
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
				document.validate();
			} else {
				// Set our "accumulator" to Pending
				accum = Status::Pending;

				for (_, dep_uri) in unresolved_deps {
					// Request this dependency from the client if we haven't already done so
					if !self.pending.contains(dep_uri) {
						let params = UnreadDependencyParams {
							dependency: dep_uri.clone(),
							dependant: (*document.uri).clone(),
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

	pub fn get(&self, uri: &Url) -> Option<Document> {
		self.documents.get(uri).map(|entry| entry.value().clone())
	}

	pub fn get_dependents_of(&self, uri: &Url) -> Vec<Url> {
		self.documents
			.iter()
			.filter_map(|entry| {
				if entry.value().deps.values().contains(uri) {
					Some(entry.key().clone())
				} else {
					None
				}
			})
			.collect()
	}

	pub fn find_decl(&self, uri: &Url, token: &Token) -> Option<FindDeclResult> {
		let document = self.get(uri)?;

		let tree = document.ast.as_ref();
		let scope = Arc::clone(&document.scopes);
		let deps = Arc::clone(&document.deps)
			.iter()
			.map(|(name, uri)| (name.clone(), self.get(uri).unwrap()))
			.collect();

		let mut finder = DeclFinder::new(scope, deps, token.clone());
		tree.walk(&mut finder);

		finder.result()
	}
}

impl Clone for Documents {
	fn clone(&self) -> Self {
		Self {
			documents: Arc::clone(&self.documents),
			pending: Arc::clone(&self.pending),
			ipc: self.ipc.clone(),
			status_tx: self.status_tx.clone(),
			config: Arc::clone(&self.config),
		}
	}
}

fn parse(text: String) -> parser_v2::Result<(SyntaxTree, ArcStr, Vec<Token>)> {
	let mut parser = ParseStream::from(text);
	let ast = parser.parse::<SyntaxTree>()?;
	let (source, tokens) = parser.into_inner();

	Ok((ast, source, tokens))
}

struct DependencyResolver {
	source: Url,
	dependencies: HashMap<Substr, Url>,
}

impl DependencyResolver {
	fn resolve(ast: &SyntaxTree, source: &Url) -> Arc<HashMap<Substr, Url>> {
		let mut resolver = Self::new(source);
		ast.walk(&mut resolver);

		Arc::new(resolver.dependencies)
	}

	fn new(source: &Url) -> Self {
		Self {
			source: source.clone(),
			dependencies: HashMap::new(),
		}
	}
}

impl Visitor for DependencyResolver {
	fn visit_module_decl(&mut self, decl: &ModuleDecl) {
		let name = decl.name.lexeme();
		let uri = self.source.resolve_import(decl.path().as_str());

		self.dependencies.insert(name, uri);
	}
}
