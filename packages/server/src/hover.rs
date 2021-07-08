use lsp_server::Response;
use lsp_types::{
	request::{HoverRequest, Request},
	Hover, HoverContents, HoverParams, MarkupContent, MarkupKind,
};
use serde_json as json;

pub fn handle(req: lsp_server::Request) -> Option<Response> {
	let (id, _params) = req.extract::<HoverParams>(HoverRequest::METHOD).unwrap();

	let result = Hover {
		contents: HoverContents::Markup(MarkupContent {
			kind: MarkupKind::PlainText,
			value: "Yep, this is definitely a thing.".into(),
		}),
		range: None,
	};

	Some(Response {
		id,
		result: Some(json::to_value(&result).unwrap()),
		error: None,
	})
}
