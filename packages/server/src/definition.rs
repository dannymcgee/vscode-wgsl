use std::thread;

use crossbeam::channel::Sender;
use lsp_server::{Message, Response};
use lsp_types::{
	request::{GotoDefinition, Request},
	GotoDefinitionParams, GotoDefinitionResponse, Location, Range,
};
use parser::{FlatTokens, GetRange, IsWithin};
use serde_json as json;

use crate::documents;

pub fn handle(req: lsp_server::Request, tx: Sender<Message>) {
	thread::spawn(move || {
		let (id, params) = req
			.extract::<GotoDefinitionParams>(GotoDefinition::METHOD)
			.unwrap();

		let uri = params.text_document_position_params.text_document.uri;
		let pos = params.text_document_position_params.position;
		let symbol_range = Range {
			start: pos,
			end: pos,
		};

		let result = documents::parse(&uri)
			.and_then(|ast| {
				let mut tokens = vec![];
				ast.flat_tokens(&mut tokens);

				tokens
					.iter()
					.find(|token| symbol_range.is_within(&token.range()))
					.cloned()
			})
			.and_then(|token| documents::scopes(&uri).and_then(|scopes| scopes.find_decl(&token)))
			.map(|decl| {
				let result = GotoDefinitionResponse::Scalar(Location {
					uri,
					range: decl.ident().range(),
				});

				json::to_value(&result).unwrap()
			})
			.or_else(|| Some(json::to_value(&GotoDefinitionResponse::Array(vec![])).unwrap()));

		let _ = tx.send(Message::Response(Response {
			id,
			result,
			error: None,
		}));
	});
}
