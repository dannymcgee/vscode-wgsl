use lsp_types::{
	CodeLensOptions, DocumentSymbolOptions, HoverProviderCapability, OneOf,
	SemanticTokensFullOptions, SemanticTokensOptions, SemanticTokensServerCapabilities,
	ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, WorkDoneProgressOptions,
};
use serde_json as json;

use crate::events::semantic_tokens;

pub fn define() -> json::Value {
	json::to_value(&ServerCapabilities {
		hover_provider: Some(HoverProviderCapability::Simple(true)),
		semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(
			SemanticTokensOptions {
				full: Some(SemanticTokensFullOptions::Bool(true)),
				range: None,
				legend: semantic_tokens::legend(),
				..Default::default()
			},
		)),
		document_symbol_provider: Some(OneOf::Right(DocumentSymbolOptions {
			label: None,
			work_done_progress_options: WorkDoneProgressOptions::default(),
		})),
		definition_provider: Some(OneOf::Left(true)),
		text_document_sync: Some(TextDocumentSyncCapability::Kind(
			TextDocumentSyncKind::INCREMENTAL,
		)),
		references_provider: Some(OneOf::Left(true)),
		code_lens_provider: Some(CodeLensOptions {
			resolve_provider: Some(false),
		}),
		..Default::default()
	})
	.unwrap()
}
