use lsp_server::{Message, RequestId, Response};
use parser_v2::Token;
use serde_json as json;

use crate::{documents_v2::Documents, extensions::DebugDocumentParams};

#[derive(DebugLisp)]
struct TokensPrinter<'a> {
	inner: Vec<Token<'a>>,
}

impl<'a> TokensPrinter<'a> {
	fn new(tokens: Vec<Token<'a>>) -> Self {
		Self { inner: tokens }
	}
}

pub fn handle(id: RequestId, params: DebugDocumentParams, docs: &Documents) -> Message {
	let uri = params.text_document.uri;

	let result = docs
		.get(&uri)
		.map(|document| {
			let printer = TokensPrinter::new(document.tokens.clone());
			format!("{:#?}", printer)
		})
		.unwrap_or_else(|| "".into());

	Message::Response(Response {
		id,
		result: Some(json::to_value(&result).unwrap()),
		error: None,
	})
}
