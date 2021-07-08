use lsp_types::{HoverProviderCapability, ServerCapabilities};
use serde_json as json;

pub fn define() -> json::Value {
	json::to_value(&ServerCapabilities {
		hover_provider: Some(HoverProviderCapability::Simple(true)),
		..Default::default()
	})
	.unwrap()
}
