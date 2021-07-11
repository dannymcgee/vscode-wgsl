use std::thread;

use crossbeam_channel::Sender;
use lsp_server::{Message, Response};
use lsp_types::{
	request::{Request, SemanticTokensFullRequest},
	SemanticTokens, SemanticTokensParams,
};
use serde_json as json;

#[allow(unused_must_use)]
pub fn handle(req: lsp_server::Request, tx: Sender<Message>) {
	thread::spawn(move || {
		let (id, _params) = req
			.extract::<SemanticTokensParams>(SemanticTokensFullRequest::METHOD)
			.unwrap();

		tx.send(Message::Response(Response {
			id,
			error: None,
			result: Some(
				json::to_value(&SemanticTokens {
					result_id: None,
					data: vec![],
				})
				.unwrap(),
			),
		}));
	});
}
