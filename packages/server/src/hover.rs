use std::thread;

use crossbeam::channel::Sender;
use lsp_server::{Message, Response};
use lsp_types::{
	request::{HoverRequest, Request},
	Hover, HoverContents, HoverParams, MarkupContent, MarkupKind, Range,
};
use parser::{GetRange, IsWithin};
use serde_json as json;

use crate::documents;

pub fn handle(req: lsp_server::Request, tx: Sender<Message>) {
	thread::spawn(move || {
		let (id, params) = req.extract::<HoverParams>(HoverRequest::METHOD).unwrap();

		let uri = params.text_document_position_params.text_document.uri;
		let pos = params.text_document_position_params.position;
		let hover_range = Range {
			start: pos,
			end: pos,
		};

		let result = documents::tokens(&uri)
			.and_then(|tokens| {
				tokens
					.iter()
					.find(|token| hover_range.is_within(&token.range()))
					.cloned()
			})
			.and_then(|token| {
				documents::scopes(&uri)
					.and_then(|scopes| scopes.find_decl(&token))
					.map(|decl| (token.range(), decl))
			})
			.map(|(range, decl)| {
				let result = Hover {
					contents: HoverContents::Markup(MarkupContent {
						kind: MarkupKind::Markdown,
						value: format!("```\n{}\n```", decl.decl),
					}),
					range: Some(range),
				};

				json::to_value(&result).unwrap()
			})
			.or_else(|| {
				Some(
					json::to_value(&Hover {
						contents: HoverContents::Array(vec![]),
						range: None,
					})
					.unwrap(),
				)
			});

		let _ = tx.send(Message::Response(Response {
			id,
			result,
			error: None,
		}));
	});
}
