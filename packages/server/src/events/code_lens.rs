use std::sync::Arc;

use gramatika::Spanned;
use lsp_server::{Message, RequestId, Response};
use lsp_types::{
	CodeLens, CodeLensParams, Command, Position, ReferenceContext, ReferenceParams,
	TextDocumentPositionParams,
};
use parser_v2::utils::ToRange;
use serde_json as json;

use super::references;
use crate::documents::Documents;

pub fn handle(id: RequestId, params: CodeLensParams, docs: &Documents) -> Message {
	let result = get_lenses(&params, docs).unwrap_or_else(Vec::new);

	Message::Response(Response {
		id,
		result: Some(json::to_value(&result).unwrap()),
		error: None,
	})
}

fn get_lenses(params: &CodeLensParams, docs: &Documents) -> Option<Vec<CodeLens>> {
	let uri = &params.text_document.uri;
	let document = docs.get(uri)?;
	let tree = Arc::clone(&document.ast);

	let lenses = tree
		.inner
		.iter()
		.filter_map(|decl| {
			let ident_span = decl.name().span();
			let pos = Position {
				line: ident_span.start.line as u32,
				character: ident_span.start.character as u32,
			};
			let refs = references::find_all(uri, pos, false, docs)?;

			let title = if refs.len() == 1 {
				"1 Reference".to_string()
			} else {
				std::format!("{} References", refs.len())
			};

			let cmd_params = ReferenceParams {
				text_document_position: TextDocumentPositionParams::new(
					params.text_document.clone(),
					pos,
				),
				context: ReferenceContext {
					include_declaration: false,
				},
				partial_result_params: Default::default(),
				work_done_progress_params: Default::default(),
			};

			let range = if let Some(attributes) = decl.attributes() {
				attributes.span().to_range()
			} else {
				ident_span.to_range()
			};

			Some(CodeLens {
				command: Some(Command {
					title,
					command: "wgsl.resolveLensReferences".into(),
					arguments: Some(vec![json::to_value(&cmd_params).unwrap()]),
				}),
				data: Some(json::to_value(&refs).unwrap()),
				range,
			})
		})
		.collect();

	Some(lenses)
}
