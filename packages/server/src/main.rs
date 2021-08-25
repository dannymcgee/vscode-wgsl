use lsp_server::{Connection, Message};
use lsp_types::InitializeParams;
use serde_json as json;

#[macro_use]
extern crate serde;
#[macro_use]
extern crate gramatika;

mod capabilities;
mod diagnostics;
mod documents;
mod documents_v2;
mod events;
mod lsp_extensions;

type Error = Box<dyn std::error::Error + Send + Sync>;

fn main() -> Result<(), Error> {
	let (cx, io_threads) = Connection::stdio();
	let capabilities = capabilities::define();
	let init_params = cx.initialize(capabilities)?;

	unsafe {
		diagnostics::bootstrap(cx.sender.clone());
	}
	main_loop(&cx, init_params)?;

	io_threads.join()?;

	Ok(())
}

fn main_loop(cx: &Connection, params: json::Value) -> anyhow::Result<()> {
	let _: InitializeParams = json::from_value(params).unwrap();

	let rx = cx.receiver.clone();
	let tx = cx.sender.clone();

	let dispatcher = events::Dispatcher::new(tx);

	for msg in rx {
		match msg {
			Message::Request(req) => dispatcher.dispatch(req),
			Message::Notification(notif) => dispatcher.notify(notif),
			_ => unreachable!(),
		}
	}

	Ok(())
}
