use std::collections::HashMap;

use lsp_types::Range;
use parser::{ast::*, GetRange, NamespaceScope};

use crate::{CumulativeResult, Result, TranspileError};

pub(crate) trait ReplaceNamespaces {
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

			self.name = Token::Ident(format!("{}_{}", namespace, name), Range {
				start: start_range.start,
				end: end_range.end,
			});
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

			self.ident = Token::Ident(format!("{}_{}", namespace, name), Range {
				start: start_range.start,
				end: end_range.end,
			});
		}

		for arg in self.args.iter_mut() {
			arg.replace_namespaces(namespaces, accum_result);
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
