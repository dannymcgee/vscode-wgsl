use lsp_types::{notification::Notification, Url};

#[derive(Deserialize, Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct UnreadDependencyParams {
	pub dependency: Url,
	pub dependant: Url,
}

pub enum UnreadDependency {}

impl Notification for UnreadDependency {
	type Params = UnreadDependencyParams;
	const METHOD: &'static str = "wgsl/unreadDependency";
}
