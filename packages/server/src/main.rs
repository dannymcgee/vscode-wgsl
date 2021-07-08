use lsp_server::{Connection, Message};
use lsp_types::{
	request::{HoverRequest, Request},
	InitializeParams,
};
use serde_json as json;

mod capabilities;
mod hover;

type Error = Box<dyn std::error::Error + Send + Sync>;

fn main() -> Result<(), Error> {
	eprintln!("Starting WGSL Language Server");

	let (cx, io_threads) = Connection::stdio();
	let capabilities = capabilities::define();
	let init_params = cx.initialize(capabilities)?;

	main_loop(&cx, init_params)?;

	io_threads.join()?;

	eprintln!("Stopping WGSL Language Server");
	Ok(())
}

fn main_loop(cx: &Connection, params: json::Value) -> Result<(), Error> {
	let _: InitializeParams = json::from_value(params).unwrap();

	for msg in &cx.receiver {
		match msg {
			Message::Request(req) => {
				eprintln!("[Request] {}", req.method);

				if cx.handle_shutdown(&req)? {
					return Ok(());
				}

				let response = match &req.method[..] {
					HoverRequest::METHOD => hover::handle(req),
					_ => None,
				};

				if let Some(res) = response {
					cx.sender.send(Message::Response(res))?;
				}
			}
			Message::Response(res) => {
				eprintln!("{:#?}", res);
			}
			Message::Notification(notif) => {
				eprintln!("{:#?}", notif);
			}
		}
	}

	Ok(())
}
