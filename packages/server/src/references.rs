use std::sync::Arc;

use gramatika::{Spanned, Token};
use itertools::Itertools;
use lsp_server::{Message, RequestId, Response};
use lsp_types::{Location, ReferenceParams};
use parser_v2::utils::ToRange;
use serde_json as json;

use crate::documents_v2::Documents;

pub fn handle(id: RequestId, params: ReferenceParams, docs: &Documents) -> Message {
	let result = get_references(&params, docs).unwrap_or_else(Vec::new);

	Message::Response(Response {
		id,
		result: Some(json::to_value(&result).unwrap()),
		error: None,
	})
}

fn get_references(params: &ReferenceParams, docs: &Documents) -> Option<Vec<Location>> {
	let uri = &params.text_document_position.text_document.uri;
	let pos = params.text_document_position.position;

	let document = docs.get(uri)?;
	let tokens = &document.tokens;
	let scopes = Arc::clone(&document.scopes);

	let needle = tokens.iter().find(|token| {
		let range = token.span().to_range();
		pos >= range.start && pos <= range.end
	})?;

	let needle_decl = docs.find_decl(uri.clone(), *needle)?.decl;
	let scope = scopes.find(needle_decl.name())?;

	let mut references = tokens
		.iter()
		.filter_map(|token| {
			if token.lexeme() == needle.lexeme() && scope.span().contains(token.span()) {
				let decl = docs.find_decl(uri.clone(), *token)?.decl;
				if decl.span() == needle_decl.span() {
					Some(Location {
						uri: uri.clone(),
						range: token.span().to_range(),
					})
				} else {
					None
				}
			} else {
				None
			}
		})
		.collect_vec();

	if params.context.include_declaration {
		references.insert(
			0,
			Location {
				uri: uri.clone(),
				range: needle_decl.name().span().to_range(),
			},
		);
	}

	Some(references)
}
