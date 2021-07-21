use lsp_server::{Connection, Message};
use lsp_types::{
	notification::{
		DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, Notification,
	},
	request::{
		DocumentSymbolRequest, GotoDefinition, HoverRequest, Request, SemanticTokensFullRequest,
	},
	DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
	InitializeParams,
};
use serde_json as json;

#[macro_use]
extern crate lazy_static;

mod capabilities;
mod definition;
mod diagnostics;
mod docsym;
mod documents;
mod hover;
mod semtok;

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
					GotoDefinition::METHOD => definition::handle(req, tx.clone()),
					_ => {}
				}
			}
			Message::Response(res) => {
				eprintln!("{:?}", res);
			}
			Message::Notification(notif) => {
				eprintln!("[Notification] {}", notif.method);

				match &notif.method[..] {
					DidOpenTextDocument::METHOD => {
						let params = notif
							.extract::<DidOpenTextDocumentParams>(DidOpenTextDocument::METHOD)
							.unwrap();

						documents::open(&params)?;
					}
					DidChangeTextDocument::METHOD => {
						let params = notif
							.extract::<DidChangeTextDocumentParams>(DidChangeTextDocument::METHOD)
							.unwrap();

						documents::update(&params)?;
					}
					DidCloseTextDocument::METHOD => {
						let params = notif
							.extract::<DidCloseTextDocumentParams>(DidCloseTextDocument::METHOD)
							.unwrap();

						documents::close(&params.text_document.uri);
					}
					_ => {}
				}
			}
		}
	}

	Ok(())
}
