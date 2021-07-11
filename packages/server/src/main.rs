use lsp_server::{Connection, Message};
use lsp_types::{
	request::{DocumentSymbolRequest, HoverRequest, Request, SemanticTokensFullRequest},
	InitializeParams,
};
use serde_json as json;

#[macro_use]
extern crate lazy_static;

mod capabilities;
mod docsym;
mod documents;
mod hover;
mod semtok;

type Error = Box<dyn std::error::Error + Send + Sync>;

fn main() -> Result<(), Error> {
	let (cx, io_threads) = Connection::stdio();
	let capabilities = capabilities::define();
	let init_params = cx.initialize(capabilities)?;

	main_loop(&cx, init_params)?;

	io_threads.join()?;

	Ok(())
}

fn main_loop(cx: &Connection, params: json::Value) -> Result<(), Error> {
	let _: InitializeParams = json::from_value(params).unwrap();

	let rx = cx.receiver.clone();
	let tx = cx.sender.clone();

	for msg in rx {
		match msg {
			Message::Request(req) => {
				eprintln!("[Request] {}", req.method);

				if cx.handle_shutdown(&req)? {
					return Ok(());
				}

				match &req.method[..] {
					HoverRequest::METHOD => hover::handle(req, tx.clone()),
					SemanticTokensFullRequest::METHOD => semtok::handle(req, tx.clone()),
					DocumentSymbolRequest::METHOD => docsym::handle(req, tx.clone()),
					_ => {}
				}
			}
			Message::Response(res) => {
				eprintln!("{:?}", res);
			}
			Message::Notification(notif) => {
				eprintln!("{:?}", notif);
			}
		}
	}

	Ok(())
}
