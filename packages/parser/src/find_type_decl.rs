use crate::{ast::*, AstNode};

pub trait FindTypeDecl {
	fn type_decl(&self) -> Option<TypeDecl>;
}

impl FindTypeDecl for AstNode {
	fn type_decl(&self) -> Option<TypeDecl> {
		match self {
			AstNode::Decl(decl) => decl.type_decl(),
			AstNode::Stmt(stmt) => stmt.type_decl(),
			AstNode::Expr(expr) => expr.type_decl(),
		}
	}
}

impl FindTypeDecl for Decl {
	fn type_decl(&self) -> Option<TypeDecl> {
		use Decl::*;

		match self {
			Var(decl) | Const(decl) => decl.type_decl.as_ref().map(|ty| *ty.clone()),
			TypeAlias(decl) => Some(decl.value.clone()),
			Field(decl) => Some(*decl.type_decl.clone()),
			Function(decl) => decl.signature.return_type.as_ref().map(|ty| *ty.clone()),
			Param(decl) => Some(*decl.type_decl.clone()),
			_ => None,
		}
	}
}

impl FindTypeDecl for Stmt {
	fn type_decl(&self) -> Option<TypeDecl> {
		use Stmt::*;

		match self {
			Variable(decl) => decl.type_decl.as_ref().map(|ty| *ty.clone()),
			_ => None,
		}
	}
}

impl FindTypeDecl for Expr {
	fn type_decl(&self) -> Option<TypeDecl> {
		use Expr::*;

		match self {
			Singular(expr) => expr.expr.type_decl(),
			_ => None,
		}
	}
}

impl FindTypeDecl for PrimaryExpr {
	fn type_decl(&self) -> Option<TypeDecl> {
		use PrimaryExpr::*;

		match self {
			TypeCtor(expr) => Some(*expr.ty.clone()),
			Bitcast(expr) => Some(*expr.ty.clone()),
			_ => None,
		}
	}
}
