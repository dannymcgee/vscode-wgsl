use std::{collections::HashMap, sync::Arc};

use lsp_types::Url;
use parser::{
	ast::{Decl, FunctionDecl, StructDecl},
	Rename,
};

mod replace_namespaces;
pub(super) use replace_namespaces::*;

pub trait IsExport {
	fn is_export(&self) -> bool;
}

impl IsExport for Decl {
	fn is_export(&self) -> bool {
		match self {
			Decl::Struct(inner) => inner.storage_modifier.as_ref().map_or(false, |token| {
				let (kw, _) = token.borrow_inner();
				kw == "export"
			}),
			Decl::Function(inner) => inner.storage_modifier.as_ref().map_or(false, |token| {
				let (kw, _) = token.borrow_inner();
				kw == "export"
			}),
			_ => false,
		}
	}
}

pub trait GetExports {
	fn exports(&self) -> Vec<Decl>;
}

impl GetExports for Vec<Decl> {
	fn exports(&self) -> Vec<Decl> {
		self.iter()
			.filter(|decl| decl.is_export())
			.cloned()
			.collect()
	}
}

pub trait ResolveDeps {
	fn resolve_deps(&self, source: &Url) -> Vec<Url>;
}

impl ResolveDeps for Vec<Decl> {
	fn resolve_deps(&self, source: &Url) -> Vec<Url> {
		self.iter()
			.filter_map(|decl| match decl {
				Decl::Module(inner) => {
					let (quoted, _) = inner.path.borrow_inner();
					let slice = &quoted[1..quoted.len() - 1];
					let uri = source.resolve_import(slice);

					Some(uri)
				}
				_ => None,
			})
			.collect()
	}
}

pub trait ResolveImportPath {
	fn resolve_import(&self, path: &str) -> Url;
}

impl ResolveImportPath for Url {
	fn resolve_import(&self, path: &str) -> Self {
		let mut resolved = self.clone();
		{
			let resolved_segs = &mut resolved.path_segments_mut().unwrap();
			resolved_segs.pop();

			let segments = path.split('/');
			for segment in segments {
				match segment {
					"." => {}
					".." => {
						resolved_segs.pop();
					}
					other => {
						resolved_segs.push(other);
					}
				}
			}
		}

		resolved.set_path(&format!("{}.wgsl", resolved.path()));

		resolved
	}
}

pub(crate) trait TranscludeModule<'a> {
	type Output: IntoIterator<Item = Decl>;

	fn transclude(&'a self, module_name: &'a str) -> Self::Output;
}

impl<'a> TranscludeModule<'a> for HashMap<String, Arc<Decl>> {
	type Output = impl IntoIterator<Item = Decl>;

	fn transclude(&'a self, module_name: &'a str) -> Self::Output {
		use Decl::*;

		self.iter()
			.filter_map(move |(_, decl)| match decl.as_ref() {
				Struct(decl) => Some(decl.transclude(module_name)),
				Function(decl) => Some(decl.transclude(module_name)),
				_ => None,
			})
	}
}

pub(crate) trait TranscludeExport {
	fn transclude(&self, module_name: &str) -> Decl;
}

macro_rules! transclude {
	($self:ident, $module_name:ident, $variant:path) => {{
		let mut xformed = $self.clone();
		let name = format!("{}_{}", $module_name, $self.name.as_str());
		xformed.rename(name);
		xformed.storage_modifier = None;

		$variant(xformed)
	}};
}

impl TranscludeExport for StructDecl {
	fn transclude(&self, module_name: &str) -> Decl {
		transclude!(self, module_name, Decl::Struct)
	}
}

impl TranscludeExport for FunctionDecl {
	fn transclude(&self, module_name: &str) -> Decl {
		transclude!(self, module_name, Decl::Function)
	}
}
