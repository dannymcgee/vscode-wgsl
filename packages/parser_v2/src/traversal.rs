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

// FIXME:
// Instead of individual `walk_x` methods, create a `Walk` trait with a single
// `fn walk(&self, visitor: &mut dyn Visitor)` method, and implement it for each node type.
// That will allow a visitor to selectively `walk` individual children of a node while
// returning `FlowControl::Break` from the parent's `visit_x` method.
impl<'a> SyntaxTree<'a> {
	pub fn walk(&self, visitor: &mut dyn Visitor<'a>) {
		for decl in self.inner.iter() {
			self.walk_decl(decl, visitor);
		}
	}

	fn walk_decl(&self, decl: &Decl<'a>, visitor: &mut dyn Visitor<'a>) {
		use Decl::*;

		if visitor.visit_decl(decl) == FlowControl::Continue {
			match decl {
				Var(decl) | Const(decl) => self.walk_var_decl(decl, visitor),
				TypeAlias(decl) => self.walk_type_alias_decl(decl, visitor),
				Struct(decl) => self.walk_struct_decl(decl, visitor),
				Field(decl) => self.walk_field_decl(decl, visitor),
				Function(decl) => self.walk_func_decl(decl, visitor),
				Param(decl) => self.walk_param_decl(decl, visitor),
				Extension(decl) => self.walk_extension_decl(decl, visitor),
				Module(decl) => self.walk_module_decl(decl, visitor),
			}
		}
	}

	fn walk_var_decl(&self, decl: &VarDecl<'a>, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_var_decl(decl) == FlowControl::Continue {
			if let Some(ref attributes) = decl.attributes {
				visitor.visit_attributes(attributes);
			}

			if let Some(ref ty) = decl.ty {
				self.walk_type(ty, visitor);
			}

			if let Some(ref expr) = decl.assignment {
				self.walk_expr(expr, visitor);
			}
		}
	}

	fn walk_type_alias_decl(&self, decl: &TypeAliasDecl<'a>, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_type_alias_decl(decl) == FlowControl::Continue {
			self.walk_type(&decl.value, visitor);
		}
	}

	fn walk_struct_decl(&self, decl: &StructDecl<'a>, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_struct_decl(decl) == FlowControl::Continue {
			for field in decl.body.fields.iter() {
				self.walk_field_decl(field, visitor);
			}
		}
	}

	fn walk_field_decl(&self, decl: &FieldDecl<'a>, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_field_decl(decl) == FlowControl::Continue {
			self.walk_type(&decl.ty, visitor);
		}
	}

	fn walk_func_decl(&self, decl: &FunctionDecl<'a>, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_func_decl(decl) == FlowControl::Continue {
			for param in decl.params.iter() {
				self.walk_param_decl(param, visitor);
			}

			if let Some(ref ty) = decl.return_ty {
				self.walk_type(ty, visitor);
			}

			for stmt in decl.body.stmts.iter() {
				self.walk_stmt(stmt, visitor);
			}
		}
	}

	fn walk_param_decl(&self, decl: &ParamDecl<'a>, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_param_decl(decl) == FlowControl::Continue {
			self.walk_type(&decl.ty, visitor);
		}
	}

	fn walk_extension_decl(&self, decl: &ExtensionDecl<'a>, visitor: &mut dyn Visitor<'a>) {
		visitor.visit_extension_decl(decl);
	}

	fn walk_module_decl(&self, decl: &ModuleDecl<'a>, visitor: &mut dyn Visitor<'a>) {
		visitor.visit_module_decl(decl);
	}

	fn walk_type(&self, ty: &TypeDecl<'a>, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_type(ty) == FlowControl::Continue {
			if let Some(ref attributes) = ty.attributes {
				visitor.visit_attributes(attributes);
			}

			visitor.visit_ident_expr(&ty.name);
		}
	}

	fn walk_stmt(&self, stmt: &Stmt<'a>, visitor: &mut dyn Visitor<'a>) {
		use Stmt::*;

		if visitor.visit_stmt(stmt) == FlowControl::Continue {
			match stmt {
				Block(stmt) => {
					for stmt in stmt.stmts.iter() {
						self.walk_stmt(stmt, visitor);
					}
				}
				Return(stmt) => self.walk_return_stmt(stmt, visitor),
				If(stmt) => self.walk_if_stmt(stmt, visitor),
				Switch(stmt) => self.walk_switch_stmt(stmt, visitor),
				Loop(stmt) => self.walk_loop_stmt(stmt, visitor),
				Continuing(stmt) => self.walk_continuing_stmt(stmt, visitor),
				For(stmt) => self.walk_for_stmt(stmt, visitor),
				Var(decl) => self.walk_var_decl(decl, visitor),
				Expr(stmt) => self.walk_expr_stmt(stmt, visitor),
				_ => {}
			}
		}
	}

	fn walk_return_stmt(&self, stmt: &ReturnStmt<'a>, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_return_stmt(stmt) == FlowControl::Continue {
			if let Some(ref expr) = stmt.value {
				self.walk_expr(expr, visitor);
			}
		}
	}

	fn walk_if_stmt(&self, stmt: &IfStmt<'a>, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_if_stmt(stmt) == FlowControl::Continue {
			self.walk_expr(&stmt.condition, visitor);

			for stmt in stmt.then_branch.stmts.iter() {
				self.walk_stmt(stmt, visitor);
			}

			if let Some(ref stmt) = stmt.elseif_branch {
				self.walk_elseif_stmt(stmt, visitor);
			}

			if let Some(ref stmt) = stmt.else_branch {
				self.walk_else_stmt(stmt, visitor);
			}
		}
	}

	fn walk_elseif_stmt(&self, stmt: &IfStmt<'a>, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_elseif_stmt(stmt) == FlowControl::Continue {
			self.walk_expr(&stmt.condition, visitor);

			for stmt in stmt.then_branch.stmts.iter() {
				self.walk_stmt(stmt, visitor);
			}

			if let Some(ref stmt) = stmt.elseif_branch {
				self.walk_elseif_stmt(stmt, visitor);
			}

			if let Some(ref stmt) = stmt.else_branch {
				self.walk_else_stmt(stmt, visitor);
			}
		}
	}

	fn walk_else_stmt(&self, stmt: &ElseStmt<'a>, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_else_stmt(stmt) == FlowControl::Continue {
			for stmt in stmt.body.stmts.iter() {
				self.walk_stmt(stmt, visitor);
			}
		}
	}

	fn walk_switch_stmt(&self, stmt: &SwitchStmt<'a>, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_switch_stmt(stmt) == FlowControl::Continue {
			self.walk_expr(&stmt.subject, visitor);

			for stmt in stmt.body.cases.iter() {
				self.walk_case_stmt(stmt, visitor);
			}
		}
	}

	fn walk_case_stmt(&self, stmt: &CaseStmt<'a>, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_case_stmt(stmt) == FlowControl::Continue {
			for stmt in stmt.body.stmts.iter() {
				self.walk_stmt(stmt, visitor);
			}
		}
	}

	fn walk_loop_stmt(&self, stmt: &LoopStmt<'a>, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_loop_stmt(stmt) == FlowControl::Continue {
			for stmt in stmt.body.stmts.iter() {
				self.walk_stmt(stmt, visitor);
			}
		}
	}

	fn walk_continuing_stmt(&self, stmt: &ContinuingStmt<'a>, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_continuing_stmt(stmt) == FlowControl::Continue {
			for stmt in stmt.body.stmts.iter() {
				self.walk_stmt(stmt, visitor);
			}
		}
	}

	fn walk_for_stmt(&self, stmt: &ForStmt<'a>, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_for_stmt(stmt) == FlowControl::Continue {
			if let Some(ref stmt) = stmt.initializer {
				self.walk_stmt(stmt, visitor);
			}

			if let Some(ref expr) = stmt.condition {
				self.walk_stmt(expr.as_ref(), visitor);
			}

			if let Some(ref expr) = stmt.increment {
				self.walk_expr(expr, visitor);
			}

			for stmt in stmt.body.stmts.iter() {
				self.walk_stmt(stmt, visitor);
			}
		}
	}

	fn walk_expr_stmt(&self, stmt: &ExprStmt<'a>, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_expr_stmt(stmt) == FlowControl::Continue {
			self.walk_expr(&stmt.expr, visitor);
		}
	}

	fn walk_expr(&self, expr: &Expr<'a>, visitor: &mut dyn Visitor<'a>) {
		use Expr::*;

		if visitor.visit_expr(expr) == FlowControl::Continue {
			match expr {
				Unary(expr) => self.walk_unary_expr(expr, visitor),
				Binary(expr) => self.walk_binary_expr(expr, visitor),
				Assignment(expr) => self.walk_assignment_expr(expr, visitor),
				FnCall(expr) => self.walk_fn_call_expr(expr, visitor),
				TypeCtor(expr) => self.walk_type_ctor_expr(expr, visitor),
				Group(expr) => self.walk_group_expr(expr, visitor),
				Bitcast(expr) => self.walk_bitcast_expr(expr, visitor),
				Literal(expr) => visitor.visit_literal_expr(expr),
				Ident(expr) => visitor.visit_ident_expr(expr),
				Primary(expr) => self.walk_primary_expr(expr, visitor),
			};
		}
	}

	fn walk_unary_expr(&self, expr: &UnaryExpr<'a>, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_unary_expr(expr) == FlowControl::Continue {
			self.walk_expr(&expr.expr, visitor);
		}
	}

	fn walk_binary_expr(&self, expr: &BinaryExpr<'a>, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_binary_expr(expr) == FlowControl::Continue {
			self.walk_expr(&expr.lhs, visitor);
			self.walk_expr(&expr.rhs, visitor);
		}
	}

	fn walk_assignment_expr(&self, expr: &BinaryExpr<'a>, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_assignment_expr(expr) == FlowControl::Continue {
			self.walk_expr(&expr.lhs, visitor);
			self.walk_expr(&expr.rhs, visitor);
		}
	}

	fn walk_primary_expr(&self, expr: &PrimaryExpr<'a>, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_primary_expr(expr) == FlowControl::Continue {
			self.walk_expr(&expr.expr, visitor);

			if let Some(ref expr) = expr.postfix {
				self.walk_postfix_expr(expr, visitor);
			}
		}
	}

	fn walk_postfix_expr(&self, expr: &PostfixExpr<'a>, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_postfix_expr(expr) == FlowControl::Continue {
			self.walk_expr(&expr.expr, visitor);

			if let Some(ref expr) = expr.postfix {
				self.walk_postfix_expr(expr, visitor);
			}
		}
	}

	fn walk_type_ctor_expr(&self, expr: &TypeCtorExpr<'a>, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_type_ctor_expr(expr) == FlowControl::Continue {
			self.walk_type(&expr.ty, visitor);

			for arg in expr.arguments.arguments.iter() {
				self.walk_expr(arg, visitor);
			}
		}
	}

	fn walk_group_expr(&self, expr: &GroupExpr<'a>, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_group_expr(expr) == FlowControl::Continue {
			self.walk_expr(&expr.expr, visitor);
		}
	}

	fn walk_bitcast_expr(&self, expr: &BitcastExpr<'a>, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_bitcast_expr(expr) == FlowControl::Continue {
			self.walk_type(&expr.ty, visitor);
			self.walk_group_expr(&expr.expr, visitor);
		}
	}

	fn walk_fn_call_expr(&self, expr: &FnCallExpr<'a>, visitor: &mut dyn Visitor<'a>) {
		if visitor.visit_fn_call_expr(expr) == FlowControl::Continue {
			for arg in expr.arguments.arguments.iter() {
				self.walk_expr(arg, visitor);
			}
		}
	}
}
