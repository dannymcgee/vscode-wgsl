use lsp_server::{Message, RequestId, Response};
use serde_json as json;

use crate::{documents_v2::Documents, extensions::DebugAstParams};

pub fn handle(id: RequestId, params: DebugAstParams, docs: &Documents) -> Message {
	let uri = params.text_document.uri;

	let result = docs
		.get(&uri)
		.map(|document| {
			let tree = document.ast.as_ref();
			format!("{:#?}", tree)
		})
		.unwrap_or_else(|| "".into());

	Message::Response(Response {
		id,
		result: Some(json::to_value(&result).unwrap()),
		error: None,
	})
}
