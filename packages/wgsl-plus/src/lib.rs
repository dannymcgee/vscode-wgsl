use std::sync::Arc;

use anyhow::{anyhow, Result};

use itertools::Itertools;
use lsp_types::Range;
use parser::{ast::*, GetRange, Rename, Scopes};

#[cfg(test)]
mod tests;

// FIXME: Make an actual error type
pub fn transpile_parsed(ast: Arc<Vec<Decl>>, scopes: &Scopes) -> Result<String> {
	let (result, transformed) = ast
		.iter()
		.fold((Ok(()), vec![]), |(result, mut tree), decl| match decl {
			Decl::Extension(_) => (result, tree),
			Decl::Module(module) => {
				let (module_name, _) = module.name.borrow_inner();
				match scopes.namespaces.get(module_name) {
					Some(inner) => {
						let decls = inner.scope.iter().filter_map(|(decl_name, decl)| {
							match decl.as_ref() {
								Decl::Extension(_) => None,
								Decl::Struct(inner) => {
									let mut inner = inner.clone();
									let name = format!("{}_{}", module_name, decl_name);
									inner.storage_modifier = None;

									Some(Decl::Struct(inner.rename(name)))
								}
								Decl::Function(inner) => {
									let mut inner = inner.clone();
									let name = format!("{}_{}", module_name, decl_name);
									inner.storage_modifier = None;

									Some(Decl::Function(inner.rename(name)))
								}
								_ => unreachable!(),
							}
						});
						tree.extend(decls);

						(result, tree)
					}
					None => {
						let err = anyhow!("Failed to resolve module `{}`", module_name);

						(Err(err), tree)
					}
				}
			}
			other => {
				let mut decl = other.clone();
				decl.replace_namespaces();
				tree.push(decl);

				(result, tree)
			}
		});

	result.map(|_| {
		transformed
			.iter()
			.map(|decl| format!("{}", decl))
			.join("\n\n")
	})
}

trait ReplaceNamespaces {
	fn replace_namespaces(&mut self);
}

impl ReplaceNamespaces for Decl {
	fn replace_namespaces(&mut self) {
		use Decl::*;

		match self {
			Var(decl) => decl.replace_namespaces(),
			Const(decl) => decl.replace_namespaces(),
			Struct(decl) => decl.replace_namespaces(),
			Field(decl) => decl.replace_namespaces(),
			TypeAlias(decl) => decl.replace_namespaces(),
			Function(decl) => decl.replace_namespaces(),
			Param(decl) => decl.replace_namespaces(),
			Extension(_) | Module(_) => {}
		}
	}
}

impl ReplaceNamespaces for TypeDecl {
	fn replace_namespaces(&mut self) {
		if let Some(namespace) = self.namespace.take() {
			let name = self.name.clone();
			let start_range = namespace.range();
			let end_range = name.range();

			self.name = Token::Ident(
				format!("{}_{}", namespace, name),
				Range {
					start: start_range.start,
					end: end_range.end,
				},
			);
		}
	}
}

impl ReplaceNamespaces for FunctionCallExpr {
	fn replace_namespaces(&mut self) {
		if let Some(namespace) = self.namespace.take() {
			let name = self.ident.clone();
			let start_range = namespace.range();
			let end_range = name.range();

			self.ident = Token::Ident(
				format!("{}_{}", namespace, name),
				Range {
					start: start_range.start,
					end: end_range.end,
				},
			);
		}
	}
}

impl ReplaceNamespaces for Option<Box<TypeDecl>> {
	fn replace_namespaces(&mut self) {
		if let Some(ty) = self.as_mut() {
			ty.replace_namespaces();
		}
	}
}

impl ReplaceNamespaces for VarDecl {
	fn replace_namespaces(&mut self) {
		self.type_decl.replace_namespaces();

		if let Some(assignment) = self.assignment.as_mut() {
			assignment.replace_namespaces();
		}
	}
}

impl ReplaceNamespaces for StructDecl {
	fn replace_namespaces(&mut self) {
		for field in self.body.iter_mut() {
			field.replace_namespaces();
		}
	}
}

impl ReplaceNamespaces for StructField {
	fn replace_namespaces(&mut self) {
		self.type_decl.replace_namespaces();
	}
}

impl ReplaceNamespaces for TypeAliasDecl {
	fn replace_namespaces(&mut self) {
		self.value.replace_namespaces();
	}
}

impl ReplaceNamespaces for FunctionDecl {
	fn replace_namespaces(&mut self) {
		self.signature.return_type.replace_namespaces();

		for param in self.signature.params.iter_mut() {
			param.replace_namespaces();
		}

		let (_, stmts) = &mut self.body;
		for stmt in stmts.iter_mut() {
			stmt.replace_namespaces();
		}
	}
}

impl ReplaceNamespaces for FunctionParam {
	fn replace_namespaces(&mut self) {
		self.type_decl.replace_namespaces();
	}
}

impl ReplaceNamespaces for Stmt {
	fn replace_namespaces(&mut self) {
		use Stmt::*;

		match self {
			Variable(decl) => decl.replace_namespaces(),
			FunctionCall(expr) => expr.replace_namespaces(),
			_ => {}
		}
	}
}

impl ReplaceNamespaces for Expr {
	fn replace_namespaces(&mut self) {
		use Expr::*;

		match self {
			Singular(expr) => expr.replace_namespaces(),
			Binary(expr) => expr.replace_namespaces(),
		}
	}
}

impl ReplaceNamespaces for SingularExpr {
	fn replace_namespaces(&mut self) {
		self.expr.replace_namespaces();
	}
}

impl ReplaceNamespaces for PrimaryExpr {
	fn replace_namespaces(&mut self) {
		use PrimaryExpr::*;

		match self {
			TypeCtor(expr) => expr.replace_namespaces(),
			Paren(expr) => expr.replace_namespaces(),
			Bitcast(expr) => expr.replace_namespaces(),
			FunctionCall(expr) => expr.replace_namespaces(),
			_ => {}
		}
	}
}

impl ReplaceNamespaces for TypeCtorExpr {
	fn replace_namespaces(&mut self) {
		for expr in self.args.iter_mut() {
			expr.replace_namespaces();
		}
	}
}

impl ReplaceNamespaces for BitcastExpr {
	fn replace_namespaces(&mut self) {
		self.expr.replace_namespaces();
	}
}

impl ReplaceNamespaces for BinaryExpr {
	fn replace_namespaces(&mut self) {
		self.lhs.replace_namespaces();
		self.rhs.replace_namespaces();
	}
}
