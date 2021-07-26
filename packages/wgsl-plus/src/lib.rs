use std::{collections::HashMap, fmt, result, sync::Arc};

use itertools::Itertools;
use lsp_types::Range;
use parser::{ast::*, GetRange, NamespaceScope, Rename, Scopes};

#[cfg(test)]
mod tests;

pub fn transpile_parsed(ast: Arc<Vec<Decl>>, scopes: &Scopes) -> Result<String> {
	let (results, transformed) = ast.iter().fold(
		(vec![], vec![]),
		|(mut results, mut tree), decl| match decl {
			Decl::Extension(_) => (results, tree),
			Decl::Module(module) => {
				let (module_name, _) = module.name.borrow_inner();
				match scopes.namespaces.get(module_name) {
					Some(inner) => {
						// FIXME: This is currently copying over every struct/func declaration,
						// not just the exports
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
		.fold(Ok(()), |mut accum, current| {
			use TranspileError::*;

			match accum {
				Ok(_) => Err(current),
				Err(Multiple(ref mut accum_errs)) => {
					match current {
						Multiple(cur_errs) => accum_errs.extend(cur_errs),
						single => accum_errs.push(single),
					}
					accum
				}
				Err(accum_single) => match current {
					Multiple(cur_errs) => {
						let mut combined = vec![accum_single];
						combined.extend(cur_errs);

						Err(Multiple(combined))
					}
					cur_single => Err(Multiple(vec![accum_single, cur_single])),
				},
			}
		})
		.map(|_| {
			transformed
				.iter()
				.map(|decl| format!("{}", decl))
				.join("\n\n")
		})
}

#[derive(Clone, Debug)]
pub enum TranspileError {
	UnknownNamespace(Token),
	UnresolvedModule(Token),
	Multiple(Vec<TranspileError>),
}

trait CumulativeResult {
	type Error: fmt::Display;

	fn add_error(&mut self, err: Self::Error);
}

impl CumulativeResult for Result<()> {
	type Error = TranspileError;

	fn add_error(&mut self, err: Self::Error) {
		use TranspileError::*;

		match self {
			Ok(_) => {
				*self = Err(err);
			}
			Err(Multiple(ref mut errs)) => {
				errs.push(err);
			}
			Err(single) => {
				*self = Err(Multiple(vec![single.to_owned()]));
			}
		}
	}
}

pub type Result<T> = result::Result<T, TranspileError>;

impl fmt::Display for TranspileError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use TranspileError::*;

		match self {
			UnknownNamespace(token) => write!(f, "Unknown namespace `{}`", token),
			UnresolvedModule(token) => write!(f, "Unresolved module `{}`", token),
			Multiple(errors) => {
				for err in errors.iter() {
					write!(f, "{}", err)?;
				}
				Ok(())
			}
		}
	}
}

trait ReplaceNamespaces {
	fn replace_namespaces(
		&mut self,
		namespaces: &HashMap<String, NamespaceScope>,
		accum_result: &mut Result<()>,
	);
}

impl ReplaceNamespaces for Decl {
	fn replace_namespaces(
		&mut self,
		namespaces: &HashMap<String, NamespaceScope>,
		accum_result: &mut Result<()>,
	) {
		use Decl::*;

		match self {
			Var(decl) => decl.replace_namespaces(namespaces, accum_result),
			Const(decl) => decl.replace_namespaces(namespaces, accum_result),
			Struct(decl) => decl.replace_namespaces(namespaces, accum_result),
			Field(decl) => decl.replace_namespaces(namespaces, accum_result),
			TypeAlias(decl) => decl.replace_namespaces(namespaces, accum_result),
			Function(decl) => decl.replace_namespaces(namespaces, accum_result),
			Param(decl) => decl.replace_namespaces(namespaces, accum_result),
			Extension(_) | Module(_) => {}
		}
	}
}

impl ReplaceNamespaces for TypeDecl {
	fn replace_namespaces(
		&mut self,
		namespaces: &HashMap<String, NamespaceScope>,
		accum_result: &mut Result<()>,
	) {
		if let Some(namespace) = self.namespace.take() {
			let name = self.name.clone();

			if !namespaces.contains_key(namespace.as_str()) {
				accum_result.add_error(TranspileError::UnknownNamespace(namespace.clone()));
			}

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
	fn replace_namespaces(
		&mut self,
		namespaces: &HashMap<String, NamespaceScope>,
		accum_result: &mut Result<()>,
	) {
		if let Some(namespace) = self.namespace.take() {
			let name = self.ident.clone();

			if !namespaces.contains_key(namespace.as_str()) {
				accum_result.add_error(TranspileError::UnknownNamespace(namespace.clone()));
			}

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
	fn replace_namespaces(
		&mut self,
		namespaces: &HashMap<String, NamespaceScope>,
		accum_result: &mut Result<()>,
	) {
		if let Some(ty) = self.as_mut() {
			ty.replace_namespaces(namespaces, accum_result);
		}
	}
}

impl ReplaceNamespaces for VarDecl {
	fn replace_namespaces(
		&mut self,
		namespaces: &HashMap<String, NamespaceScope>,
		accum_result: &mut Result<()>,
	) {
		self.type_decl.replace_namespaces(namespaces, accum_result);

		if let Some(assignment) = self.assignment.as_mut() {
			assignment.replace_namespaces(namespaces, accum_result);
		}
	}
}

impl ReplaceNamespaces for StructDecl {
	fn replace_namespaces(
		&mut self,
		namespaces: &HashMap<String, NamespaceScope>,
		accum_result: &mut Result<()>,
	) {
		for field in self.body.iter_mut() {
			field.replace_namespaces(namespaces, accum_result);
		}
	}
}

impl ReplaceNamespaces for StructField {
	fn replace_namespaces(
		&mut self,
		namespaces: &HashMap<String, NamespaceScope>,
		accum_result: &mut Result<()>,
	) {
		self.type_decl.replace_namespaces(namespaces, accum_result);
	}
}

impl ReplaceNamespaces for TypeAliasDecl {
	fn replace_namespaces(
		&mut self,
		namespaces: &HashMap<String, NamespaceScope>,
		accum_result: &mut Result<()>,
	) {
		self.value.replace_namespaces(namespaces, accum_result);
	}
}

impl ReplaceNamespaces for FunctionDecl {
	fn replace_namespaces(
		&mut self,
		namespaces: &HashMap<String, NamespaceScope>,
		accum_result: &mut Result<()>,
	) {
		self.signature
			.return_type
			.replace_namespaces(namespaces, accum_result);

		for param in self.signature.params.iter_mut() {
			param.replace_namespaces(namespaces, accum_result);
		}

		let (_, stmts) = &mut self.body;
		for stmt in stmts.iter_mut() {
			stmt.replace_namespaces(namespaces, accum_result);
		}
	}
}

impl ReplaceNamespaces for FunctionParam {
	fn replace_namespaces(
		&mut self,
		namespaces: &HashMap<String, NamespaceScope>,
		accum_result: &mut Result<()>,
	) {
		self.type_decl.replace_namespaces(namespaces, accum_result);
	}
}

impl ReplaceNamespaces for Stmt {
	fn replace_namespaces(
		&mut self,
		namespaces: &HashMap<String, NamespaceScope>,
		accum_result: &mut Result<()>,
	) {
		use Stmt::*;

		match self {
			Variable(decl) => decl.replace_namespaces(namespaces, accum_result),
			FunctionCall(expr) => expr.replace_namespaces(namespaces, accum_result),
			_ => {}
		}
	}
}

impl ReplaceNamespaces for Expr {
	fn replace_namespaces(
		&mut self,
		namespaces: &HashMap<String, NamespaceScope>,
		accum_result: &mut Result<()>,
	) {
		use Expr::*;

		match self {
			Singular(expr) => expr.replace_namespaces(namespaces, accum_result),
			Binary(expr) => expr.replace_namespaces(namespaces, accum_result),
		}
	}
}

impl ReplaceNamespaces for SingularExpr {
	fn replace_namespaces(
		&mut self,
		namespaces: &HashMap<String, NamespaceScope>,
		accum_result: &mut Result<()>,
	) {
		self.expr.replace_namespaces(namespaces, accum_result);
	}
}

impl ReplaceNamespaces for PrimaryExpr {
	fn replace_namespaces(
		&mut self,
		namespaces: &HashMap<String, NamespaceScope>,
		accum_result: &mut Result<()>,
	) {
		use PrimaryExpr::*;

		match self {
			TypeCtor(expr) => expr.replace_namespaces(namespaces, accum_result),
			Paren(expr) => expr.replace_namespaces(namespaces, accum_result),
			Bitcast(expr) => expr.replace_namespaces(namespaces, accum_result),
			FunctionCall(expr) => expr.replace_namespaces(namespaces, accum_result),
			_ => {}
		}
	}
}

impl ReplaceNamespaces for TypeCtorExpr {
	fn replace_namespaces(
		&mut self,
		namespaces: &HashMap<String, NamespaceScope>,
		accum_result: &mut Result<()>,
	) {
		for expr in self.args.iter_mut() {
			expr.replace_namespaces(namespaces, accum_result);
		}
	}
}

impl ReplaceNamespaces for BitcastExpr {
	fn replace_namespaces(
		&mut self,
		namespaces: &HashMap<String, NamespaceScope>,
		accum_result: &mut Result<()>,
	) {
		self.expr.replace_namespaces(namespaces, accum_result);
	}
}

impl ReplaceNamespaces for BinaryExpr {
	fn replace_namespaces(
		&mut self,
		namespaces: &HashMap<String, NamespaceScope>,
		accum_result: &mut Result<()>,
	) {
		self.lhs.replace_namespaces(namespaces, accum_result);
		self.rhs.replace_namespaces(namespaces, accum_result);
	}
}
