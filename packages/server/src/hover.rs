use std::thread;

use crossbeam_channel::Sender;
use lsp_server::{Message, Response};
use lsp_types::{
	request::{HoverRequest, Request},
	Hover, HoverContents, HoverParams, MarkupContent, MarkupKind,
};
use serde_json as json;

#[allow(unused_must_use)]
pub fn handle(req: lsp_server::Request, tx: Sender<Message>) {
	thread::spawn(move || {
		let (id, _params) = req.extract::<HoverParams>(HoverRequest::METHOD).unwrap();

		let result = Hover {
			contents: HoverContents::Markup(MarkupContent {
				kind: MarkupKind::PlainText,
				value: "Yep, this is definitely a thing.".into(),
			}),
			range: None,
		};

		tx.send(Message::Response(Response {
			id,
			result: Some(json::to_value(&result).unwrap()),
			error: None,
		}));
	});
}
