use std::thread;

use crossbeam::channel::Sender;
use lsp_server::{Message, RequestId, Response};
use lsp_types::{GotoDefinitionParams, GotoDefinitionResponse, Location, Range};
use parser::{GetRange, IsWithin};
use serde_json as json;

use crate::documents;

pub fn handle(id: RequestId, params: GotoDefinitionParams, tx: Sender<Message>) {
	thread::spawn(move || {
		let uri = params.text_document_position_params.text_document.uri;
		let pos = params.text_document_position_params.position;
		let symbol_range = Range {
			start: pos,
			end: pos,
		};

		let result = documents::tokens(&uri)
			.and_then(|tokens| {
				tokens
					.iter()
					.find(|token| symbol_range.is_within(&token.range()))
					.cloned()
			})
			.and_then(|token| documents::scopes(&uri).and_then(|scopes| scopes.find_decl(&token)))
			.map(|decl| {
				let uri = decl.uri.unwrap_or(uri);
				let range = decl.decl.ident().range();

				let result = GotoDefinitionResponse::Scalar(Location { uri, range });

				json::to_value(&result).unwrap()
			})
			.or_else(|| Some(json::to_value(&GotoDefinitionResponse::Array(vec![])).unwrap()));

		tx.send(Message::Response(Response {
			id,
			result,
			error: None,
		}))
		.unwrap();
	});
}
