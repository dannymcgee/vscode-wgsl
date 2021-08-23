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
pub struct DebugDocumentParams {
	pub text_document: TextDocumentIdentifier,
}

pub enum DebugAst {}
impl Request for DebugAst {
	type Params = DebugDocumentParams;
	type Result = String;
	const METHOD: &'static str = "wgsl/debugAst";
}

pub enum DebugTokens {}
impl Request for DebugTokens {
	type Params = DebugDocumentParams;
	type Result = String;
	const METHOD: &'static str = "wgsl/debugTokens";
}
