use std::{fmt, sync::Arc};

use crossbeam::channel::Sender;
use dashmap::DashMap;
use gramatika::once_cell::sync::OnceCell;
use lsp_server::{Message, Request, RequestId, Response, ResponseError};
use serde_json as json;

use crate::events::DispatchError;

type RequestHandler = dyn Fn(Result<json::Value, ResponseError>) + Send + Sync;

static INSTANCE: OnceCell<Requester> = OnceCell::new();

#[derive(Clone)]
pub struct Requester {
	tx: Sender<Message>,
	queue: Arc<DashMap<RequestId, Box<RequestHandler>>>,
}

impl fmt::Debug for Requester {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.debug_struct("Requester")
			.field("tx", &self.tx)
			.field(
				"queue",
				&self
					.queue
					.iter()
					.map(|entry| entry.key().clone())
					.collect::<Vec<_>>(),
			)
			.finish()
	}
}

impl Requester {
	pub fn global() -> Self {
		INSTANCE.get().unwrap().clone()
	}

	pub fn init(tx: Sender<Message>) -> Self {
		let queue = Arc::new(DashMap::default());
		INSTANCE.set(Self { tx, queue }).unwrap();

		Self::global()
	}

	pub fn send<F>(&self, request: Request, handler: F)
	where F: Fn(Result<json::Value, ResponseError>) + Send + Sync + 'static {
		self.queue.insert(request.id.clone(), Box::new(handler));
		self.tx.send(Message::Request(request)).unwrap();
	}

	pub fn dispatch(&self, response: Response) {
		match self.queue.remove(&response.id) {
			Some((_, handle)) => match (response.result, response.error) {
				(None, Some(err)) => handle(Err(err)),
				(Some(value), None) => handle(Ok(value)),
				_ => unreachable!(),
			},
			None => {
				eprintln!("{}", DispatchError::from(response));
			}
		}
	}
}
