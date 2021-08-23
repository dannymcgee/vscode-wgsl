use std::{
	collections::VecDeque,
	convert::{TryFrom, TryInto},
	fmt,
	sync::Arc,
	thread,
};

use crossbeam::channel::{Receiver, Sender};
use lsp_server::{Message, Notification, Request as LSPRequest, RequestId};
use lsp_types::{
	notification::{
		DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, Notification as _,
	},
	request::{
		DocumentSymbolRequest, GotoDefinition, HoverRequest, Request as _,
		SemanticTokensFullRequest,
	},
	DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
	DocumentSymbolParams, GotoDefinitionParams, HoverParams, SemanticTokensParams, Url,
};
use parking_lot::Mutex;

use crate::{
	debug_ast, debug_tokens, definition, docsym,
	documents_v2::{Documents, Status},
	extensions::{DebugAst, DebugDocumentParams, DebugTokens},
	hover, semantic_tokens,
};

enum Request {
	Hover(RequestId, HoverParams),
	SemanticTokens(RequestId, SemanticTokensParams),
	DocumentSymbols(RequestId, DocumentSymbolParams),
	GotoDefinition(RequestId, GotoDefinitionParams),
	DebugAst(RequestId, DebugDocumentParams),
	DebugTokens(RequestId, DebugDocumentParams),
}

enum DocEvent {
	Open(DidOpenTextDocumentParams),
	Change(DidChangeTextDocumentParams),
	Close(DidCloseTextDocumentParams),
}

pub struct Dispatcher<'a>
where
	'a: 'static,
{
	queue: Arc<Mutex<VecDeque<Request>>>,
	documents: Arc<Documents<'a>>,
	ipc: Sender<Message>,
}

impl<'a> Clone for Dispatcher<'a> {
	fn clone(&self) -> Self {
		Self {
			queue: Arc::clone(&self.queue),
			documents: Arc::clone(&self.documents),
			ipc: self.ipc.clone(),
		}
	}
}

macro_rules! handle {
	($self:ident, $queue:ident, { $($req_variant:ident => $handler:path),*$(,)? }) => {
		match $queue.pop_front().unwrap() {
			$(Request::$req_variant(id, params) => {
				let msg = $handler(id, params, &$self.documents);
				$self.ipc.send(msg).unwrap();
			})*
		}
	}
}

impl<'a> Dispatcher<'a> {
	pub fn new(tx: Sender<Message>) -> Self {
		let (documents, docs_status) = Documents::new(tx.clone());
		let this = Self {
			queue: Arc::new(Mutex::new(VecDeque::new())),
			documents: Arc::new(documents),
			ipc: tx,
		};

		this.clone().subscribe(docs_status);
		this
	}

	pub fn notify(&self, notif: Notification) {
		if let Ok(event) = notif.try_into() {
			match event {
				DocEvent::Open(params) => {
					let _ = self.documents.open(params);
				}
				DocEvent::Change(params) => {
					let _ = self.documents.update(params);
				}
				DocEvent::Close(_) => {} // TODO
			}
		}
	}

	pub fn dispatch(&self, req: LSPRequest) {
		if let Ok(req) = req.try_into() {
			self.queue.lock().push_back(req);
		}

		match self.documents.status() {
			Status::Ready => self.clone().process_queue(),
			Status::Pending => {}
		}
	}

	fn subscribe(self, status: Receiver<Status>) {
		// TODO: This would probably be way more efficient to do with async/await
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
			handle!(self, queue, {
				Hover => hover::handle,
				SemanticTokens => semantic_tokens::handle,
				DocumentSymbols => docsym::handle,
				GotoDefinition => definition::handle,
				DebugAst => debug_ast::handle,
				DebugTokens => debug_tokens::handle,
			})
		}
	}
}

macro_rules! request {
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

macro_rules! doc_event {
	($notification:ident, { $($msg_type:ident => $variant:ident),*$(,)? }) => {
		match &$notification.method[..] {
			$($msg_type::METHOD => {
				let params = $notification.extract($msg_type::METHOD)?;
				eprintln!(
					"[Notification] {} : {}",
					$msg_type::METHOD,
					GetUri::uri(&params),
				);

				Ok(DocEvent::$variant(params))
			})*
			_ => Err($notification.into())
		}
	}
}

impl TryFrom<LSPRequest> for Request {
	type Error = DispatchError;

	fn try_from(value: LSPRequest) -> Result<Self, Self::Error> {
		request!(value, {
			HoverRequest => Hover,
			SemanticTokensFullRequest => SemanticTokens,
			DocumentSymbolRequest => DocumentSymbols,
			GotoDefinition => GotoDefinition,
			DebugAst => DebugAst,
			DebugTokens => DebugTokens,
		})
	}
}

impl TryFrom<Notification> for DocEvent {
	type Error = DispatchError;

	fn try_from(value: Notification) -> Result<Self, Self::Error> {
		doc_event!(value, {
			DidOpenTextDocument => Open,
			DidChangeTextDocument => Change,
			DidCloseTextDocument => Close,
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
			message: format!("No event handler for request:\n{:?}", req),
		}
	}
}

impl From<Notification> for DispatchError {
	fn from(notif: Notification) -> Self {
		Self {
			message: format!("No event handler for notification:\n{:?}", notif),
		}
	}
}

impl From<Message> for DispatchError {
	fn from(msg: Message) -> Self {
		Self {
			message: format!("No event handler for message:\n{:?}", msg),
		}
	}
}

impl fmt::Display for DispatchError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "DispatchError: {}", self.message)
	}
}

trait GetUri<'a> {
	fn uri(&'a self) -> &'a Url;
}

impl<'a> GetUri<'a> for DidOpenTextDocumentParams {
	fn uri(&'a self) -> &'a Url {
		&self.text_document.uri
	}
}

impl<'a> GetUri<'a> for DidChangeTextDocumentParams {
	fn uri(&'a self) -> &'a Url {
		&self.text_document.uri
	}
}

impl<'a> GetUri<'a> for DidCloseTextDocumentParams {
	fn uri(&'a self) -> &'a Url {
		&self.text_document.uri
	}
}

impl std::error::Error for DispatchError {}
