use std::{
	collections::VecDeque,
	convert::{TryFrom, TryInto},
	fmt,
	sync::Arc,
	thread,
};

use crossbeam::channel::{Receiver, Sender};
use lsp_server::{
	Message, Notification as LSPNotification, Request as LSPRequest, RequestId, Response,
};
use lsp_types::{
	notification::{
		DidChangeConfiguration, DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument,
		Notification as _,
	},
	request::{
		CodeLensRequest, DocumentSymbolRequest, GotoDefinition, HoverRequest, References,
		Request as _, SemanticTokensFullRequest,
	},
	CodeLensParams, DidChangeConfigurationParams, DidChangeTextDocumentParams,
	DidCloseTextDocumentParams, DidOpenTextDocumentParams, DocumentSymbolParams,
	GotoDefinitionParams, HoverParams, ReferenceParams, SemanticTokensParams, Url,
};
use parking_lot::Mutex;

use crate::{
	documents::{Documents, Status},
	lsp_extensions::{DebugAst, DebugDocumentParams, DebugTokens},
};
pub use requester::*;

mod code_lens;
mod debug_ast;
mod debug_tokens;
mod definition;
mod document_symbols;
mod hover;
mod references;
mod requester;
pub mod semantic_tokens;

enum Request {
	Hover(RequestId, HoverParams),
	SemanticTokens(RequestId, SemanticTokensParams),
	DocumentSymbols(RequestId, DocumentSymbolParams),
	GotoDefinition(RequestId, GotoDefinitionParams),
	DebugAst(RequestId, DebugDocumentParams),
	DebugTokens(RequestId, DebugDocumentParams),
	References(RequestId, ReferenceParams),
	CodeLens(RequestId, CodeLensParams),
}

enum Notification {
	DocumentOpen(DidOpenTextDocumentParams),
	DocumentChange(DidChangeTextDocumentParams),
	DocumentClose(DidCloseTextDocumentParams),
	ConfigChange(DidChangeConfigurationParams),
}

#[derive(Clone)]
pub struct Dispatcher {
	queue: Arc<Mutex<VecDeque<Request>>>,
	documents: Documents,
	ipc: Sender<Message>,
}

macro_rules! next {
	($self:ident, $queue:ident, { $($req_variant:ident => $handler:path),*$(,)? }) => {
		match $queue.pop_front().unwrap() {
			$(Request::$req_variant(id, params) => {
				let msg = $handler(id, params, &$self.documents);
				$self.ipc.send(msg).unwrap();
			})*
		}
	}
}

impl Dispatcher {
	pub fn new(tx: Sender<Message>) -> Self {
		let (documents, docs_status) = Documents::new(tx.clone());
		let this = Self {
			queue: Arc::new(Mutex::new(VecDeque::new())),
			documents,
			ipc: tx,
		};

		this.clone().subscribe(docs_status);
		this
	}

	pub fn notify(&self, notif: LSPNotification) {
		use Notification::*;

		match notif.try_into() {
			Ok(event) => match event {
				DocumentOpen(params) => {
					self.documents.open(params).unwrap();
				}
				DocumentChange(params) => {
					self.documents.update(params).unwrap();
				}
				DocumentClose(_) => {} // TODO
				ConfigChange(params) => {
					self.documents.configure(params.settings.into());
				}
			},
			Err(err) => eprintln!("{}", err),
		}
	}

	pub fn dispatch(&self, req: LSPRequest) {
		match req.try_into() {
			Ok(req) => self.queue.lock().push_back(req),
			Err(err) => eprintln!("{}", err),
		}

		match self.documents.status() {
			Status::Ready => self.clone().process_queue(),
			Status::Pending => {}
		}
	}

	fn subscribe(self, status: Receiver<Status>) {
		thread::spawn(move || {
			for update in status {
				if update == Status::Ready {
					self.clone().process_queue();
				}
			}
		});
	}

	fn process_queue(self) {
		let mut queue = self.queue.lock();
		while !queue.is_empty() {
			next!(self, queue, {
				Hover => hover::handle,
				SemanticTokens => semantic_tokens::handle,
				DocumentSymbols => document_symbols::handle,
				GotoDefinition => definition::handle,
				DebugAst => debug_ast::handle,
				DebugTokens => debug_tokens::handle,
				References => references::handle,
				CodeLens => code_lens::handle,
			})
		}
	}
}

macro_rules! from_request {
	($request:ident, { $($msg_type:ident => $variant:ident),*$(,)? }) => {
		match &$request.method[..] {
			$($msg_type::METHOD => {
				let (id, params) = $request.extract($msg_type::METHOD)?;
				eprintln!("[Request] {}", $msg_type::METHOD);

				Ok(Request::$variant(id, params))
			})*
			_ => Err($request.into())
		}
	}
}

macro_rules! from_notification {
	($notification:ident, { $($msg_type:ident => $variant:ident),*$(,)? }) => {
		match &$notification.method[..] {
			$($msg_type::METHOD => {
				let params = $notification.extract($msg_type::METHOD)?;
				if let Some(uri) = GetUri::uri(&params) {
					eprintln!(
						"[Notification] {} : {}",
						$msg_type::METHOD,
						uri,
					);
				} else {
					eprintln!("[Notification] {}", $msg_type::METHOD);
				}

				Ok(Notification::$variant(params))
			})*
			_ => Err($notification.into())
		}
	}
}

impl TryFrom<LSPRequest> for Request {
	type Error = DispatchError;

	fn try_from(value: LSPRequest) -> Result<Self, Self::Error> {
		from_request!(value, {
			HoverRequest => Hover,
			SemanticTokensFullRequest => SemanticTokens,
			DocumentSymbolRequest => DocumentSymbols,
			GotoDefinition => GotoDefinition,
			DebugAst => DebugAst,
			DebugTokens => DebugTokens,
			References => References,
			CodeLensRequest => CodeLens,
		})
	}
}

impl TryFrom<LSPNotification> for Notification {
	type Error = DispatchError;

	fn try_from(value: LSPNotification) -> Result<Self, Self::Error> {
		from_notification!(value, {
			DidOpenTextDocument => DocumentOpen,
			DidChangeTextDocument => DocumentChange,
			DidCloseTextDocument => DocumentClose,
			DidChangeConfiguration => ConfigChange,
		})
	}
}

#[derive(Debug)]
pub struct DispatchError {
	message: String,
}

impl From<LSPRequest> for DispatchError {
	fn from(req: LSPRequest) -> Self {
		Self {
			message: std::format!("No event handler for request: {}", req.method),
		}
	}
}

impl From<LSPNotification> for DispatchError {
	fn from(notif: LSPNotification) -> Self {
		Self {
			message: std::format!("No event handler for notification: {}", notif.method),
		}
	}
}

impl From<Response> for DispatchError {
	fn from(res: Response) -> Self {
		Self {
			message: std::format!("No event handler for response: {}", res.id),
		}
	}
}

impl fmt::Display for DispatchError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "DispatchError: {}", self.message)
	}
}

trait GetUri<'a> {
	fn uri(&'a self) -> Option<&'a Url>;
}

impl<'a> GetUri<'a> for DidOpenTextDocumentParams {
	fn uri(&'a self) -> Option<&'a Url> {
		Some(&self.text_document.uri)
	}
}

impl<'a> GetUri<'a> for DidChangeTextDocumentParams {
	fn uri(&'a self) -> Option<&'a Url> {
		Some(&self.text_document.uri)
	}
}

impl<'a> GetUri<'a> for DidCloseTextDocumentParams {
	fn uri(&'a self) -> Option<&'a Url> {
		Some(&self.text_document.uri)
	}
}

impl<'a> GetUri<'a> for DidChangeConfigurationParams {
	fn uri(&'a self) -> Option<&'a Url> {
		None
	}
}

impl std::error::Error for DispatchError {}
