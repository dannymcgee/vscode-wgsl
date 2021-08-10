use gramatika::Token as _;

use crate::{
	common::{AttributeList, TypeDecl},
	decl::{
		Decl, ExtensionDecl, FieldDecl, FunctionDecl, ModuleDecl, ParamDecl, StructDecl,
		TypeAliasDecl, VarDecl,
	},
	expr::{
		BinaryExpr, BitcastExpr, Expr, FnCallExpr, GroupExpr, IdentExpr, PostfixExpr, PrimaryExpr,
		TypeCtorExpr, UnaryExpr,
	},
	stmt::{
		CaseStmt, ContinuingStmt, ElseStmt, ExprStmt, ForStmt, IfStmt, LoopStmt, ReturnStmt, Stmt,
		SwitchStmt,
	},
	SyntaxTree, Token,
};

#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
pub enum FlowControl {
	Continue,
	Break,
}

#[allow(unused_variables)]
#[rustfmt::skip]
pub trait Visitor<'a> {
	#[must_use] fn visit_decl(&mut self, decl: &Decl<'a>) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_stmt(&mut self, stmt: &Stmt<'a>) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_expr(&mut self, expr: &Expr<'a>) -> FlowControl { FlowControl::Continue }

	#[must_use] fn visit_type(&mut self, decl: &TypeDecl<'a>) -> FlowControl { FlowControl::Continue }
	fn visit_attributes(&mut self, attr: &AttributeList<'a>) {}

	#[must_use] fn visit_var_decl(&mut self, decl: &VarDecl<'a>) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_type_alias_decl(&mut self, decl: &TypeAliasDecl<'a>) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_struct_decl(&mut self, decl: &StructDecl<'a>) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_field_decl(&mut self, decl: &FieldDecl<'a>) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_func_decl(&mut self, decl: &FunctionDecl<'a>) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_param_decl(&mut self, decl: &ParamDecl<'a>) -> FlowControl { FlowControl::Continue }
	fn visit_extension_decl(&mut self, decl: &ExtensionDecl<'a>) {}
	fn visit_module_decl(&mut self, decl: &ModuleDecl<'a>) {}

	#[must_use] fn visit_return_stmt(&mut self, stmt: &ReturnStmt<'a>) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_if_stmt(&mut self, stmt: &IfStmt<'a>) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_elseif_stmt(&mut self, stmt: &IfStmt<'a>) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_else_stmt(&mut self, stmt: &ElseStmt<'a>) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_switch_stmt(&mut self, stmt: &SwitchStmt<'a>) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_case_stmt(&mut self, stmt: &CaseStmt<'a>) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_loop_stmt(&mut self, stmt: &LoopStmt<'a>) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_continuing_stmt(&mut self, stmt: &ContinuingStmt<'a>) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_for_stmt(&mut self, stmt: &ForStmt<'a>) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_expr_stmt(&mut self, stmt: &ExprStmt<'a>) -> FlowControl { FlowControl::Continue }

	#[must_use] fn visit_unary_expr(&mut self, expr: &UnaryExpr<'a>) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_binary_expr(&mut self, expr: &BinaryExpr<'a>) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_assignment_expr(&mut self, expr: &BinaryExpr<'a>) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_fn_call_expr(&mut self, expr: &FnCallExpr<'a>) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_type_ctor_expr(&mut self, expr: &TypeCtorExpr<'a>) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_group_expr(&mut self, expr: &GroupExpr<'a>) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_bitcast_expr(&mut self, expr: &BitcastExpr<'a>) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_primary_expr(&mut self, expr: &PrimaryExpr<'a>) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_postfix_expr(&mut self, expr: &PostfixExpr<'a>) -> FlowControl { FlowControl::Continue }
	fn visit_literal_expr(&mut self, expr: &Token<'a>) {}
	fn visit_ident_expr(&mut self, expr: &IdentExpr<'a>) {}
}

pub trait Walk<'a> {
	fn walk(&self, visitor: &mut dyn Visitor<'a>);
}

impl<'a> Walk<'a> for SyntaxTree<'a> {
	fn walk(&self, visitor: &mut dyn Visitor<'a>) {
		for decl in self.inner.iter() {
			decl.walk(visitor);
		}
	}
}

// --- Declarations ----------------------------------------------------------------------

impl<'a> Walk<'a> for Decl<'a> {
	fn walk(&self, visitor: &mut dyn Visitor<'a>) {
		use Decl::*;

		if visitor.visit_decl(self) == FlowControl::Continue {
			match self {
				Var(decl) | Const(decl) => decl.walk(visitor),
				TypeAlias(decl) => decl.walk(visitor),
				Struct(decl) => decl.walk(visitor),
				Field(decl) => decl.walk(visitor),
				Function(decl) => decl.walk(visitor),
				Param(decl) => decl.walk(visitor),
				Extension(decl) => decl.walk(visitor),
				Module(decl) => decl.walk(visitor),
			}
		}
	}
}

impl<'a> Walk<'a> for VarDecl<'a> {
	fn walk(&self, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_var_decl(self) == FlowControl::Continue {
			if let Some(ref attributes) = self.attributes {
				visitor.visit_attributes(attributes);
			}

			if let Some(ref ty) = self.ty {
				ty.walk(visitor);
			}

			if let Some(ref expr) = self.assignment {
				expr.walk(visitor);
			}
		}
	}
}

impl<'a> Walk<'a> for TypeAliasDecl<'a> {
	fn walk(&self, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_type_alias_decl(self) == FlowControl::Continue {
			self.value.walk(visitor);
		}
	}
}

impl<'a> Walk<'a> for StructDecl<'a> {
	fn walk(&self, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_struct_decl(self) == FlowControl::Continue {
			for field in self.body.fields.iter() {
				field.walk(visitor);
			}
		}
	}
}

impl<'a> Walk<'a> for FieldDecl<'a> {
	fn walk(&self, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_field_decl(self) == FlowControl::Continue {
			self.ty.walk(visitor);
		}
	}
}

impl<'a> Walk<'a> for FunctionDecl<'a> {
	fn walk(&self, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_func_decl(self) == FlowControl::Continue {
			for param in self.params.iter() {
				param.walk(visitor);
			}

			if let Some(ref ty) = self.return_ty {
				ty.walk(visitor);
			}

			for stmt in self.body.stmts.iter() {
				stmt.walk(visitor);
			}
		}
	}
}

impl<'a> Walk<'a> for ParamDecl<'a> {
	fn walk(&self, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_param_decl(self) == FlowControl::Continue {
			self.ty.walk(visitor);
		}
	}
}

impl<'a> Walk<'a> for ExtensionDecl<'a> {
	fn walk(&self, visitor: &mut dyn Visitor<'a>) {
		visitor.visit_extension_decl(self);
	}
}

impl<'a> Walk<'a> for ModuleDecl<'a> {
	fn walk(&self, visitor: &mut dyn Visitor<'a>) {
		visitor.visit_module_decl(self);
	}
}

// --- Statements ------------------------------------------------------------------------

impl<'a> Walk<'a> for Stmt<'a> {
	fn walk(&self, visitor: &mut dyn Visitor<'a>) {
		use Stmt::*;

		if visitor.visit_stmt(self) == FlowControl::Continue {
			match self {
				Block(stmt) => {
					for stmt in stmt.stmts.iter() {
						stmt.walk(visitor);
					}
				}
				Return(stmt) => stmt.walk(visitor),
				If(stmt) => stmt.walk(visitor),
				Switch(stmt) => stmt.walk(visitor),
				Loop(stmt) => stmt.walk(visitor),
				Continuing(stmt) => stmt.walk(visitor),
				For(stmt) => stmt.walk(visitor),
				Var(decl) => decl.walk(visitor),
				Expr(stmt) => stmt.walk(visitor),
				_ => {}
			}
		}
	}
}

impl<'a> Walk<'a> for ReturnStmt<'a> {
	fn walk(&self, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_return_stmt(self) == FlowControl::Continue {
			if let Some(ref expr) = self.value {
				expr.walk(visitor);
			}
		}
	}
}

impl<'a> Walk<'a> for IfStmt<'a> {
	fn walk(&self, visitor: &mut dyn Visitor<'a>) {
		let visit_method = match self.keyword.lexeme() {
			"if" => Visitor::visit_if_stmt,
			"elseif" => Visitor::visit_elseif_stmt,
			_ => unreachable!(),
		};

		if visit_method(visitor, self) == FlowControl::Continue {
			self.condition.walk(visitor);

			for stmt in self.then_branch.stmts.iter() {
				stmt.walk(visitor);
			}

			if let Some(ref stmt) = self.elseif_branch {
				stmt.walk(visitor);
			}

			if let Some(ref stmt) = self.else_branch {
				stmt.walk(visitor);
			}
		}
	}
}

impl<'a> Walk<'a> for ElseStmt<'a> {
	fn walk(&self, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_else_stmt(self) == FlowControl::Continue {
			for stmt in self.body.stmts.iter() {
				stmt.walk(visitor);
			}
		}
	}
}

impl<'a> Walk<'a> for SwitchStmt<'a> {
	fn walk(&self, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_switch_stmt(self) == FlowControl::Continue {
			self.subject.walk(visitor);

			for stmt in self.body.cases.iter() {
				stmt.walk(visitor);
			}
		}
	}
}

impl<'a> Walk<'a> for CaseStmt<'a> {
	fn walk(&self, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_case_stmt(self) == FlowControl::Continue {
			for stmt in self.body.stmts.iter() {
				stmt.walk(visitor);
			}
		}
	}
}

impl<'a> Walk<'a> for LoopStmt<'a> {
	fn walk(&self, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_loop_stmt(self) == FlowControl::Continue {
			for stmt in self.body.stmts.iter() {
				stmt.walk(visitor);
			}
		}
	}
}

impl<'a> Walk<'a> for ContinuingStmt<'a> {
	fn walk(&self, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_continuing_stmt(self) == FlowControl::Continue {
			for stmt in self.body.stmts.iter() {
				stmt.walk(visitor);
			}
		}
	}
}

impl<'a> Walk<'a> for ForStmt<'a> {
	fn walk(&self, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_for_stmt(self) == FlowControl::Continue {
			if let Some(ref stmt) = self.initializer {
				stmt.walk(visitor);
			}

			if let Some(ref expr) = self.condition {
				expr.walk(visitor);
			}

			if let Some(ref expr) = self.increment {
				expr.walk(visitor);
			}

			for stmt in self.body.stmts.iter() {
				stmt.walk(visitor);
			}
		}
	}
}

impl<'a> Walk<'a> for ExprStmt<'a> {
	fn walk(&self, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_expr_stmt(self) == FlowControl::Continue {
			self.expr.walk(visitor);
		}
	}
}

// --- Expressions -----------------------------------------------------------------------

impl<'a> Walk<'a> for Expr<'a> {
	fn walk(&self, visitor: &mut dyn Visitor<'a>) {
		use Expr::*;

		if visitor.visit_expr(self) == FlowControl::Continue {
			match self {
				Unary(expr) => expr.walk(visitor),
				Binary(expr) => expr.walk(visitor),
				Assignment(expr) => expr.walk(visitor),
				FnCall(expr) => expr.walk(visitor),
				TypeCtor(expr) => expr.walk(visitor),
				Group(expr) => expr.walk(visitor),
				Bitcast(expr) => expr.walk(visitor),
				Literal(expr) => visitor.visit_literal_expr(expr),
				Ident(expr) => visitor.visit_ident_expr(expr),
				Primary(expr) => expr.walk(visitor),
			};
		}
	}
}

impl<'a> Walk<'a> for UnaryExpr<'a> {
	fn walk(&self, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_unary_expr(self) == FlowControl::Continue {
			self.expr.walk(visitor);
		}
	}
}

impl<'a> Walk<'a> for BinaryExpr<'a> {
	fn walk(&self, visitor: &mut dyn Visitor<'a>) {
		let visit_method = match self.op.lexeme() {
			"=" => Visitor::visit_assignment_expr,
			_ => Visitor::visit_binary_expr,
		};

		if visit_method(visitor, self) == FlowControl::Continue {
			self.lhs.walk(visitor);
			self.rhs.walk(visitor);
		}
	}
}

impl<'a> Walk<'a> for FnCallExpr<'a> {
	fn walk(&self, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_fn_call_expr(self) == FlowControl::Continue {
			for arg in self.arguments.arguments.iter() {
				arg.walk(visitor);
			}
		}
	}
}

impl<'a> Walk<'a> for TypeCtorExpr<'a> {
	fn walk(&self, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_type_ctor_expr(self) == FlowControl::Continue {
			self.ty.walk(visitor);

			for arg in self.arguments.arguments.iter() {
				arg.walk(visitor);
			}
		}
	}
}

impl<'a> Walk<'a> for GroupExpr<'a> {
	fn walk(&self, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_group_expr(self) == FlowControl::Continue {
			self.expr.walk(visitor);
		}
	}
}

impl<'a> Walk<'a> for BitcastExpr<'a> {
	fn walk(&self, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_bitcast_expr(self) == FlowControl::Continue {
			self.ty.walk(visitor);
			self.expr.walk(visitor);
		}
	}
}

impl<'a> Walk<'a> for PrimaryExpr<'a> {
	fn walk(&self, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_primary_expr(self) == FlowControl::Continue {
			self.expr.walk(visitor);

			if let Some(ref expr) = self.postfix {
				expr.walk(visitor);
			}
		}
	}
}

impl<'a> Walk<'a> for PostfixExpr<'a> {
	fn walk(&self, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_postfix_expr(self) == FlowControl::Continue {
			self.expr.walk(visitor);

			if let Some(ref expr) = self.postfix {
				expr.walk(visitor);
			}
		}
	}
}

// --- Common ----------------------------------------------------------------------------

impl<'a> Walk<'a> for TypeDecl<'a> {
	fn walk(&self, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_type(self) == FlowControl::Continue {
			if let Some(ref attributes) = self.attributes {
				visitor.visit_attributes(attributes);
			}

			visitor.visit_ident_expr(&self.name);
		}
	}
}
