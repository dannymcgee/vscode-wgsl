use std::sync::Arc;

use gramatika::{Spanned, Token as _};
use itertools::Itertools;
use lsp_server::{Message, RequestId, Response};
use lsp_types::{SemanticToken, SemanticTokens, SemanticTokensParams, Url};
use parser_v2::traversal::Walk;
use serde_json as json;

use crate::documents_v2::Documents;

mod builder;
mod deltas;
mod legend;

use builder::SemanticTokensBuilder;
use deltas::{Delta, TokenDelta};
pub use legend::{legend, TokenMod, TokenType};

pub fn handle(id: RequestId, params: SemanticTokensParams, docs: &Documents) -> Message {
	let tokens = get_semantic_tokens(params.text_document.uri, docs);
	let result = Some(json::to_value(&tokens).unwrap());

	Message::Response(Response {
		id,
		result,
		error: None,
	})
}

fn get_semantic_tokens(uri: Url, docs: &Documents) -> SemanticTokens {
	let document = if let Some(document) = docs.get(&uri) {
		document
	} else {
		return empty_result();
	};

	let tree = document.ast.as_ref();
	let scopes = Arc::clone(&document.scopes);
	let deps = Arc::clone(&document.deps)
		.iter()
		.map(|(name, uri)| (*name, docs.get(uri).unwrap()))
		.collect();

	let mut builder = SemanticTokensBuilder::new(scopes, deps);
	tree.walk(&mut builder);

	let tokens = builder.build();
	let tokens = tokens
		.iter()
		.sorted_by_key(|(token, _)| token.span())
		.collect_vec();

	let data = tokens
		.iter()
		.enumerate()
		.map(|(idx, (token, (ttype, tmod)))| {
			let delta = if idx == 0 {
				TokenDelta {
					delta_line: token.span().start.line as u32,
					delta_start_char: token.span().start.character as u32,
				}
			} else {
				let (prev, _) = tokens[idx - 1];
				token.delta(prev)
			};

			SemanticToken {
				delta_line: delta.delta_line,
				delta_start: delta.delta_start_char,
				length: token.lexeme().len() as u32,
				token_type: (*ttype) as u32,
				token_modifiers_bitset: *tmod,
			}
		})
		.collect_vec();

	SemanticTokens {
		data,
		result_id: None,
	}
}

fn empty_result() -> SemanticTokens {
	SemanticTokens {
		data: vec![],
		result_id: None,
	}
}
