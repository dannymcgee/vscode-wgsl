use json::Value;
use lsp_server::Request;
use lsp_types::{
	request::{Request as _, WorkspaceConfiguration},
	ConfigurationItem, ConfigurationParams, WorkspaceClientCapabilities,
};
use serde_json as json;

use crate::{documents::Documents, events::Requester};

#[derive(Clone, Debug)]
pub struct Configuration {
	pub preprocessor: PreprocessorConfig,
}

#[derive(Clone, Debug)]
pub struct PreprocessorConfig {
	pub include_keyword: String,
	pub include_module_aliases: Vec<ModuleAlias>,
	pub include_paths: Vec<String>,
}

#[derive(Clone, Debug)]
pub struct ModuleAlias {
	pub name: String,
	pub path: String,
}

impl Configuration {
	pub fn init(capabilities: Option<WorkspaceClientCapabilities>) {
		if let Some(WorkspaceClientCapabilities {
			configuration: Some(true),
			..
		}) = capabilities
		{
			let params = ConfigurationParams {
				items: vec![ConfigurationItem {
					section: Some("wgsl".into()),
					scope_uri: None,
				}],
			};

			let request = Request {
				id: "workspace/configuration".to_string().into(),
				method: WorkspaceConfiguration::METHOD.into(),
				params: json::to_value(&params).unwrap(),
			};

			Requester::global().send(request, |response| {
				Documents::global().configure(response.unwrap().into());
			});
		}
	}
}

// TODO: Turn this crap into a proc macro derive
// P.S. Am I using serde_json wrong or why doesn't derive(Deserialize) already do this?

#[derive(Debug, Default)]
struct PreprocessorConfigBuilder {
	include_keyword: Option<String>,
	include_module_aliases: Option<Vec<ModuleAlias>>,
	include_paths: Option<Vec<String>>,
}
impl PreprocessorConfig {
	fn build() -> PreprocessorConfigBuilder {
		Default::default()
	}
}
impl PreprocessorConfigBuilder {
	fn include_keyword(mut self, value: String) -> Self {
		self.include_keyword = Some(value);
		self
	}
	fn include_module_aliases(mut self, value: Vec<ModuleAlias>) -> Self {
		self.include_module_aliases = Some(value);
		self
	}
	fn include_paths(mut self, value: Vec<String>) -> Self {
		self.include_paths = Some(value);
		self
	}
	fn finish(self) -> PreprocessorConfig {
		PreprocessorConfig {
			include_keyword: self.include_keyword.unwrap(),
			include_module_aliases: self.include_module_aliases.unwrap(),
			include_paths: self.include_paths.unwrap(),
		}
	}
}

#[derive(Debug, Default)]
struct ModuleAliasBuilder {
	name: Option<String>,
	path: Option<String>,
}
impl ModuleAlias {
	fn build() -> ModuleAliasBuilder {
		Default::default()
	}
}
impl ModuleAliasBuilder {
	fn name(mut self, value: String) -> Self {
		self.name = Some(value);
		self
	}
	fn path(mut self, value: String) -> Self {
		self.path = Some(value);
		self
	}
	fn finish(self) -> ModuleAlias {
		ModuleAlias {
			name: self.name.unwrap(),
			path: self.path.unwrap(),
		}
	}
}

impl From<Value> for Configuration {
	fn from(value: Value) -> Self {
		match value {
			Value::Array(arr) => arr.into_iter().next().unwrap().into(),
			Value::Object(map) => match map.into_iter().next().unwrap() {
				(key, value) if key == "preprocessor" => Self {
					preprocessor: value.into(),
				},
				_ => unreachable!(),
			},
			_ => unreachable!(),
		}
	}
}

impl From<Value> for PreprocessorConfig {
	fn from(value: Value) -> Self {
		match value {
			Value::Object(map) => map
				.into_iter()
				.fold(Self::build(), |builder, (key, value)| match &key[..] {
					"includeKeyword" => builder.include_keyword(json::from_value(value).unwrap()),
					"includeModuleAliases" => builder.include_module_aliases(match value {
						Value::Array(vec) => vec.into_iter().map(ModuleAlias::from).collect(),
						_ => unreachable!(),
					}),
					"includePaths" => builder.include_paths(match value {
						Value::Array(vec) => vec
							.into_iter()
							.map(|v| json::from_value(v).unwrap())
							.collect(),
						_ => unreachable!(),
					}),
					_ => unreachable!(),
				})
				.finish(),
			_ => unreachable!(),
		}
	}
}

impl From<Value> for ModuleAlias {
	fn from(value: Value) -> Self {
		match value {
			Value::Object(map) => map
				.into_iter()
				.fold(Self::build(), |builder, (key, value)| match &key[..] {
					"name" => builder.name(json::from_value(value).unwrap()),
					"path" => builder.path(json::from_value(value).unwrap()),
					_ => unreachable!(),
				})
				.finish(),
			_ => unreachable!(),
		}
	}
}
