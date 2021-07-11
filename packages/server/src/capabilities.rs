use lsp_types::{
	DocumentSymbolOptions, HoverProviderCapability, OneOf, SemanticTokensFullOptions,
	SemanticTokensLegend, SemanticTokensOptions, SemanticTokensServerCapabilities,
	ServerCapabilities, WorkDoneProgressOptions,
};
use serde_json as json;

pub fn define() -> json::Value {
	json::to_value(&ServerCapabilities {
		hover_provider: Some(HoverProviderCapability::Simple(true)),
		semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(
			SemanticTokensOptions {
				full: Some(SemanticTokensFullOptions::Bool(true)),
				range: None,
				legend: SemanticTokensLegend {
					token_types: vec![],     // TODO
					token_modifiers: vec![], // TODO
				},
				..Default::default()
			},
		)),
		document_symbol_provider: Some(OneOf::Right(DocumentSymbolOptions {
			label: None,
			work_done_progress_options: WorkDoneProgressOptions::default(),
		})),
		..Default::default()
	})
	.unwrap()
}
