use std::{
	fs,
	path::{Path, PathBuf},
};

use lsp_server::Response;
use lsp_types::{
	request::{Request, SemanticTokensFullRequest},
	SemanticTokens, SemanticTokensParams, SemanticTokensResult, Url,
};
use serde_json as json;

pub fn handle(req: lsp_server::Request) -> Option<Response> {
	let (id, params) = req
		.extract::<SemanticTokensParams>(SemanticTokensFullRequest::METHOD)
		.unwrap();

	eprintln!("{:#?}", params);
	let path = PathBuf::from_uri(params.text_document.uri);

	match fs::read_to_string(&path) {
		Ok(content) => {
			eprintln!("Content:\n{}", content);
		}
		Err(err) => {
			eprintln!("Error reading path '{}': {}", path.to_str().unwrap(), err);
		}
	}

	let result = SemanticTokensResult::Tokens(SemanticTokens {
		result_id: None,
		data: vec![], // TODO
	});

	Some(Response {
		id,
		result: Some(json::to_value(&result).unwrap()),
		error: None,
	})
}

trait FromUri {
	fn from_uri(input: Url) -> Self;
}

impl FromUri for PathBuf {
	#[cfg(target_family = "windows")]
	fn from_uri(input: Url) -> PathBuf {
		let string = urlencoding::decode(input.path()).unwrap();
		Path::new(&string[1..]).to_path_buf()
	}

	#[cfg(not(target_family = "windows"))]
	fn from_uri(input: Url) -> PathBuf {
		Path::new(input.path()).to_path_buf()
	}
}
