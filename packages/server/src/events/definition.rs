use gramatika::{span, Spanned};
use lsp_server::{Message, RequestId, Response};
use lsp_types::{GotoDefinitionParams, GotoDefinitionResponse, Location};
use parser_v2::{decl::Decl, utils::ToRange, Token};
use serde_json as json;
use wgsl_plus::ResolveImportPath;

use crate::documents::Documents;

pub fn handle(id: RequestId, params: GotoDefinitionParams, docs: &Documents) -> Message {
	let uri = params.text_document_position_params.text_document.uri;
	let pos = params.text_document_position_params.position;

	let result = docs
		.get(&uri)
		.and_then(|document| {
			let tokens = &document.tokens;
			let needle = tokens.iter().find(|token| {
				let range = token.span().to_range();
				pos >= range.start && pos <= range.end
			})?;

			match needle {
				Token::Path(lexeme, _) => {
					let path = &lexeme[1..lexeme.len() - 1];
					let uri = uri.resolve_import(path);

					Some(GotoDefinitionResponse::Scalar(Location {
						uri,
						range: span![0:0...0:0].to_range(),
					}))
				}
				other => docs.find_decl(&uri, other).map(|found| {
					let range = found.decl.name().span().to_range();
					let uri = found
						.source_module
						.and_then(|module| match module.as_ref() {
							Decl::Module(inner) => Some(uri.resolve_import(inner.path().as_str())),
							_ => None,
						})
						.unwrap_or(uri);

					GotoDefinitionResponse::Scalar(Location { uri, range })
				}),
			}
		})
		.unwrap_or_else(|| GotoDefinitionResponse::Array(vec![]));

	Message::Response(Response {
		id,
		result: Some(json::to_value(&result).unwrap()),
		error: None,
	})
}
