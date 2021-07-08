use lsp_types::{
	HoverProviderCapability, SemanticTokensFullOptions, SemanticTokensLegend,
	SemanticTokensOptions, SemanticTokensServerCapabilities, ServerCapabilities,
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
		..Default::default()
	})
	.unwrap()
}
