use lsp_types::{notification::Notification, request::Request, TextDocumentIdentifier, Url};

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

#[derive(Deserialize, Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct DebugAstParams {
	pub text_document: TextDocumentIdentifier,
}

pub enum DebugAst {}
impl Request for DebugAst {
	type Params = DebugAstParams;
	type Result = String;
	const METHOD: &'static str = "wgsl/debugAst";
}
