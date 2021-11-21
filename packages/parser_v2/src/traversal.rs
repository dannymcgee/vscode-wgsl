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
		BlockStmt, CaseStmt, ContinuingStmt, ElseStmt, ExprStmt, ForStmt, IfStmt, LoopStmt,
		ReturnStmt, Stmt, SwitchStmt,
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
pub trait Visitor {
	#[must_use] fn visit_decl(&mut self, decl: &Decl) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_stmt(&mut self, stmt: &Stmt) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_expr(&mut self, expr: &Expr) -> FlowControl { FlowControl::Continue }

	#[must_use] fn visit_type(&mut self, decl: &TypeDecl) -> FlowControl { FlowControl::Continue }
	fn visit_attributes(&mut self, attr: &AttributeList) {}

	#[must_use] fn visit_var_decl(&mut self, decl: &VarDecl) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_type_alias_decl(&mut self, decl: &TypeAliasDecl) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_struct_decl(&mut self, decl: &StructDecl) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_field_decl(&mut self, decl: &FieldDecl) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_func_decl(&mut self, decl: &FunctionDecl) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_param_decl(&mut self, decl: &ParamDecl) -> FlowControl { FlowControl::Continue }
	fn visit_extension_decl(&mut self, decl: &ExtensionDecl) {}
	fn visit_module_decl(&mut self, decl: &ModuleDecl) {}

	#[must_use] fn visit_return_stmt(&mut self, stmt: &ReturnStmt) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_if_stmt(&mut self, stmt: &IfStmt) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_elseif_stmt(&mut self, stmt: &IfStmt) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_else_stmt(&mut self, stmt: &ElseStmt) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_switch_stmt(&mut self, stmt: &SwitchStmt) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_case_stmt(&mut self, stmt: &CaseStmt) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_loop_stmt(&mut self, stmt: &LoopStmt) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_continuing_stmt(&mut self, stmt: &ContinuingStmt) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_for_stmt(&mut self, stmt: &ForStmt) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_expr_stmt(&mut self, stmt: &ExprStmt) -> FlowControl { FlowControl::Continue }

	#[must_use] fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_assignment_expr(&mut self, expr: &BinaryExpr) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_fn_call_expr(&mut self, expr: &FnCallExpr) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_type_ctor_expr(&mut self, expr: &TypeCtorExpr) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_group_expr(&mut self, expr: &GroupExpr) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_bitcast_expr(&mut self, expr: &BitcastExpr) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_primary_expr(&mut self, expr: &PrimaryExpr) -> FlowControl { FlowControl::Continue }
	#[must_use] fn visit_postfix_expr(&mut self, expr: &PostfixExpr) -> FlowControl { FlowControl::Continue }
	fn visit_literal_expr(&mut self, expr: &Token) {}
	fn visit_ident_expr(&mut self, expr: &IdentExpr) {}
}

pub trait Walk {
	fn walk(&self, visitor: &mut dyn Visitor);
}

impl Walk for SyntaxTree {
	fn walk(&self, visitor: &mut dyn Visitor) {
		for decl in self.inner.iter() {
			decl.walk(visitor);
		}
	}
}

// --- Declarations ----------------------------------------------------------------------

impl Walk for Decl {
	fn walk(&self, visitor: &mut dyn Visitor) {
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

impl Walk for VarDecl {
	fn walk(&self, visitor: &mut dyn Visitor) {
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

impl Walk for TypeAliasDecl {
	fn walk(&self, visitor: &mut dyn Visitor) {
		if visitor.visit_type_alias_decl(self) == FlowControl::Continue {
			self.value.walk(visitor);
		}
	}
}

impl Walk for StructDecl {
	fn walk(&self, visitor: &mut dyn Visitor) {
		if visitor.visit_struct_decl(self) == FlowControl::Continue {
			for field in self.body.fields.iter() {
				field.walk(visitor);
			}
		}
	}
}

impl Walk for FieldDecl {
	fn walk(&self, visitor: &mut dyn Visitor) {
		if visitor.visit_field_decl(self) == FlowControl::Continue {
			self.ty.walk(visitor);
		}
	}
}

impl Walk for FunctionDecl {
	fn walk(&self, visitor: &mut dyn Visitor) {
		if visitor.visit_func_decl(self) == FlowControl::Continue {
			for param in self.params.iter() {
				param.walk(visitor);
			}

			if let Some(ref ty) = self.return_ty {
				ty.walk(visitor);
			}

			self.body.walk(visitor);
		}
	}
}

impl Walk for ParamDecl {
	fn walk(&self, visitor: &mut dyn Visitor) {
		if visitor.visit_param_decl(self) == FlowControl::Continue {
			self.ty.walk(visitor);
		}
	}
}

impl Walk for ExtensionDecl {
	fn walk(&self, visitor: &mut dyn Visitor) {
		visitor.visit_extension_decl(self);
	}
}

impl Walk for ModuleDecl {
	fn walk(&self, visitor: &mut dyn Visitor) {
		visitor.visit_module_decl(self);
	}
}

// --- Statements ------------------------------------------------------------------------

impl Walk for Stmt {
	fn walk(&self, visitor: &mut dyn Visitor) {
		use Stmt::*;

		if visitor.visit_stmt(self) == FlowControl::Continue {
			match self {
				Block(stmt) => stmt.walk(visitor),
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

impl Walk for BlockStmt {
	fn walk(&self, visitor: &mut dyn Visitor) {
		for stmt in self.stmts.iter() {
			stmt.walk(visitor);
		}
	}
}

impl Walk for ReturnStmt {
	fn walk(&self, visitor: &mut dyn Visitor) {
		if visitor.visit_return_stmt(self) == FlowControl::Continue {
			if let Some(ref expr) = self.value {
				expr.walk(visitor);
			}
		}
	}
}

impl Walk for IfStmt {
	fn walk(&self, visitor: &mut dyn Visitor) {
		let visit_method = match self.keyword.lexeme().as_str() {
			"if" => Visitor::visit_if_stmt,
			"elseif" => Visitor::visit_elseif_stmt,
			_ => unreachable!(),
		};

		if visit_method(visitor, self) == FlowControl::Continue {
			self.condition.walk(visitor);
			self.then_branch.walk(visitor);

			if let Some(ref stmt) = self.elseif_branch {
				stmt.walk(visitor);
			}

			if let Some(ref stmt) = self.else_branch {
				stmt.walk(visitor);
			}
		}
	}
}

impl Walk for ElseStmt {
	fn walk(&self, visitor: &mut dyn Visitor) {
		if visitor.visit_else_stmt(self) == FlowControl::Continue {
			self.body.walk(visitor);
		}
	}
}

impl Walk for SwitchStmt {
	fn walk(&self, visitor: &mut dyn Visitor) {
		if visitor.visit_switch_stmt(self) == FlowControl::Continue {
			self.subject.walk(visitor);

			for stmt in self.body.cases.iter() {
				stmt.walk(visitor);
			}
		}
	}
}

impl Walk for CaseStmt {
	fn walk(&self, visitor: &mut dyn Visitor) {
		if visitor.visit_case_stmt(self) == FlowControl::Continue {
			self.body.walk(visitor);
		}
	}
}

impl Walk for LoopStmt {
	fn walk(&self, visitor: &mut dyn Visitor) {
		if visitor.visit_loop_stmt(self) == FlowControl::Continue {
			self.body.walk(visitor);
		}
	}
}

impl Walk for ContinuingStmt {
	fn walk(&self, visitor: &mut dyn Visitor) {
		if visitor.visit_continuing_stmt(self) == FlowControl::Continue {
			self.body.walk(visitor);
		}
	}
}

impl Walk for ForStmt {
	fn walk(&self, visitor: &mut dyn Visitor) {
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

			self.body.walk(visitor);
		}
	}
}

impl Walk for ExprStmt {
	fn walk(&self, visitor: &mut dyn Visitor) {
		if visitor.visit_expr_stmt(self) == FlowControl::Continue {
			self.expr.walk(visitor);
		}
	}
}

// --- Expressions -----------------------------------------------------------------------

impl Walk for Expr {
	fn walk(&self, visitor: &mut dyn Visitor) {
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

impl Walk for UnaryExpr {
	fn walk(&self, visitor: &mut dyn Visitor) {
		if visitor.visit_unary_expr(self) == FlowControl::Continue {
			self.expr.walk(visitor);
		}
	}
}

impl Walk for BinaryExpr {
	fn walk(&self, visitor: &mut dyn Visitor) {
		let visit_method = match self.op.lexeme().as_str() {
			"=" => Visitor::visit_assignment_expr,
			_ => Visitor::visit_binary_expr,
		};

		if visit_method(visitor, self) == FlowControl::Continue {
			self.lhs.walk(visitor);
			self.rhs.walk(visitor);
		}
	}
}

impl Walk for FnCallExpr {
	fn walk(&self, visitor: &mut dyn Visitor) {
		if visitor.visit_fn_call_expr(self) == FlowControl::Continue {
			visitor.visit_ident_expr(&self.ident);

			for arg in self.arguments.arguments.iter() {
				arg.walk(visitor);
			}
		}
	}
}

impl Walk for TypeCtorExpr {
	fn walk(&self, visitor: &mut dyn Visitor) {
		if visitor.visit_type_ctor_expr(self) == FlowControl::Continue {
			self.ty.walk(visitor);

			for arg in self.arguments.arguments.iter() {
				arg.walk(visitor);
			}
		}
	}
}

impl Walk for GroupExpr {
	fn walk(&self, visitor: &mut dyn Visitor) {
		if visitor.visit_group_expr(self) == FlowControl::Continue {
			self.expr.walk(visitor);
		}
	}
}

impl Walk for BitcastExpr {
	fn walk(&self, visitor: &mut dyn Visitor) {
		if visitor.visit_bitcast_expr(self) == FlowControl::Continue {
			self.ty.walk(visitor);
			self.expr.walk(visitor);
		}
	}
}

impl Walk for PrimaryExpr {
	fn walk(&self, visitor: &mut dyn Visitor) {
		if visitor.visit_primary_expr(self) == FlowControl::Continue {
			self.expr.walk(visitor);

			if let Some(ref expr) = self.postfix {
				expr.walk(visitor);
			}
		}
	}
}

impl Walk for PostfixExpr {
	fn walk(&self, visitor: &mut dyn Visitor) {
		if visitor.visit_postfix_expr(self) == FlowControl::Continue {
			self.expr.walk(visitor);

			if let Some(ref expr) = self.postfix {
				expr.walk(visitor);
			}
		}
	}
}

// --- Common ----------------------------------------------------------------------------

impl Walk for TypeDecl {
	fn walk(&self, visitor: &mut dyn Visitor) {
		if visitor.visit_type(self) == FlowControl::Continue {
			if let Some(ref attributes) = self.attributes {
				visitor.visit_attributes(attributes);
			}

			visitor.visit_ident_expr(&self.name);
		}
	}
}
