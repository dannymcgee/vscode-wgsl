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
	definition, docsym,
	documents_v2::{Documents, Status},
	hover, semantic_tokens,
};

lazy_static! {
	static ref QUEUE: Arc<Mutex<VecDeque<Request>>> = Arc::new(Mutex::new(VecDeque::new()));
}

enum Request {
	Hover(RequestId, HoverParams),
	SemanticTokens(RequestId, SemanticTokensParams),
	DocumentSymbols(RequestId, DocumentSymbolParams),
	GotoDefinition(RequestId, GotoDefinitionParams),
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
		match notif.try_into() {
			Ok(event) => match event {
				DocEvent::Open(params) => match self.documents.open(params) {
					Ok(_) => {}
					Err(err) => {
						eprintln!("{}", err);
					}
				},
				DocEvent::Change(_) => {} // TODO
				DocEvent::Close(_) => {}  // TODO
			},
			Err(err) => {
				eprintln!("{}", err);
			}
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
				match update {
					// FIXME - This is currently trying to process the queue with unresolved
					// dependencies still hanging around, which causes the server to crash
					Status::Ready => self.clone().process_queue(),
					Status::Pending => {}
				}
			}
		});
	}

	fn process_queue(self) {
		use Request::*;

		let mut queue = self.queue.lock();
		while !queue.is_empty() {
			match queue.pop_front().unwrap() {
				Hover(id, params) => hover::handle(id, params, self.ipc.clone()),
				SemanticTokens(id, params) => {
					let msg = semantic_tokens::handle(id, params, &self.documents);
					self.ipc.send(msg).unwrap();
				}
				DocumentSymbols(id, params) => {
					let msg = docsym::handle(id, params, &self.documents);
					self.ipc.send(msg).unwrap();
				}
				GotoDefinition(id, params) => definition::handle(id, params, self.ipc.clone()),
			}
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
