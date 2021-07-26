#![feature(min_type_alias_impl_trait)]

use std::sync::Arc;

use itertools::Itertools;
use parser::{ast::*, NamespaceScope, Scopes};

mod error;
mod import_export;

pub use error::*;
pub use import_export::*;

#[cfg(test)]
mod tests;

pub fn transpile_parsed(ast: Arc<Vec<Decl>>, scopes: &Scopes) -> Result<String> {
	let (results, transformed) = ast.iter().fold(
		(vec![], vec![]),
		|(mut results, mut tree), decl| match decl {
			Decl::Extension(_) => (results, tree),
			Decl::Module(module) => {
				let module_name = module.name.as_str();
				match scopes.namespaces.get(module_name) {
					Some(NamespaceScope { scope, .. }) => {
						let decls = scope.transclude(module_name);
						tree.extend(decls);
						(results, tree)
					}
					None => {
						results.push(Err(TranspileError::UnresolvedModule(module.name.clone())));
						(results, tree)
					}
				}
			}
			other => {
				let mut decl = other.clone();
				let mut decl_results = Ok(());

				decl.replace_namespaces(&scopes.namespaces, &mut decl_results);
				tree.push(decl);
				results.push(decl_results);

				(results, tree)
			}
		},
	);

	results
		.into_iter()
		.filter_map(|result| result.err())
		.fold_errors(())
		.map(|_| {
			transformed
				.iter()
				.map(|decl| format!("{}", decl))
				.join("\n\n")
		})
}
