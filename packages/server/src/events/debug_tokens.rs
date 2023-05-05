use lsp_server::{Message, RequestId, Response};
use parser_v2::Token;
use serde_json as json;

use crate::{documents::Documents, lsp_extensions::DebugDocumentParams};

#[derive(DebugLisp)]
struct TokensPrinter {
	inner: Vec<Token>,
}

impl TokensPrinter {
	fn new(tokens: &[Token]) -> Self {
		Self {
			inner: tokens.into(),
		}
	}
}

pub fn handle(id: RequestId, params: DebugDocumentParams, docs: &Documents) -> Message {
	let uri = params.text_document.uri;

	let result = docs
		.get(&uri)
		.map(|document| {
			let printer = TokensPrinter::new(&document.tokens);
			std::format!("{:#?}", printer)
		})
		.unwrap_or_else(|| "".into());

	Message::Response(Response {
		id,
		result: Some(json::to_value(result).unwrap()),
		error: None,
	})
}
