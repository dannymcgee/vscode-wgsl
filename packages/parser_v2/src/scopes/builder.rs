use std::sync::Arc;

use gramatika::{Position, Span, Spanned, Token as _};

use super::Scope;
use crate::{
	decl::{
		Decl, FieldDecl, FunctionDecl, ModuleDecl, ParamDecl, StructDecl, TypeAliasDecl, VarDecl,
	},
	stmt::{BlockStmt, CaseStmt, ContinuingStmt, ElseStmt, ForStmt, IfStmt, LoopStmt, Stmt},
	traversal::{FlowControl, Visitor, Walk},
	Token,
};

pub(super) struct ScopeBuilder<'a> {
	// We need to hold a persistent reference to the root, because each node holds only a
	// weak reference to its parent, but a strong reference to each of its children. If we
	// didn't keep a separate reference to the root, each parent would be dropped and de-
	// allocated as soon as we spawned and stepped into a new child.
	root: Arc<Scope<'a>>,
	current: Arc<Scope<'a>>,
}

impl<'a> ScopeBuilder<'a> {
	pub(super) fn new(span: Span) -> Self {
		let root = Arc::new(Scope::new(span));

		Self {
			root: Arc::clone(&root),
			current: root,
		}
	}

	pub(super) fn build(self) -> Arc<Scope<'a>> {
		self.root
	}

	fn spawn_for_decl(&self, start: Position) -> Arc<Scope<'a>> {
		let end = self.current.span.end;
		let span = Span { start, end };

		Scope::with_parent(span, Arc::clone(&self.current))
	}

	fn spawn_for_block(&self, block: &BlockStmt<'a>) -> Arc<Scope<'a>> {
		let start = block.brace_open.span().end;
		let end = block.brace_close.span().start;
		let span = Span { start, end };

		Scope::with_parent(span, Arc::clone(&self.current))
	}

	fn pop_scope(&mut self) {
		if let Some(parent) = self.current.parent() {
			self.current = parent;
		} else {
			eprintln!("WARNING: No parent scope!");
		}
	}
}

macro_rules! simple_decl {
	($self:ident, $variant:ident($decl:ident)) => {{
		let ident = $decl.name.lexeme();
		let start = $decl.name.span().start;
		let kind = $decl.name.kind();

		$self.current = $self.spawn_for_decl(start);
		$self
			.current
			.define((ident, kind), Decl::$variant($decl.clone()));
	}};
}

impl<'a> Visitor<'a> for ScopeBuilder<'a> {
	// --- Simple declarations ------------------------------------------------------------

	fn visit_module_decl(&mut self, decl: &'a ModuleDecl<'a>) {
		simple_decl!(self, Module(decl));
	}

	fn visit_type_alias_decl(&mut self, decl: &'a TypeAliasDecl<'a>) -> FlowControl {
		simple_decl!(self, TypeAlias(decl));

		FlowControl::Break
	}

	fn visit_var_decl(&mut self, decl: &'a VarDecl<'a>) -> FlowControl {
		match decl.storage.lexeme() {
			"let" => simple_decl!(self, Const(decl)),
			"var" => simple_decl!(self, Var(decl)),
			_ => unreachable!(),
		}

		FlowControl::Break
	}

	// --- Struct declaration -------------------------------------------------------------

	fn visit_struct_decl(&mut self, decl: &'a StructDecl<'a>) -> FlowControl {
		let ident = decl.name.lexeme();
		let start = decl.name.span().start;
		let kind = decl.name.kind();

		let starting_scope = self.spawn_for_decl(start);
		self.current = Arc::clone(&starting_scope);
		self.current
			.define((ident, kind), Decl::Struct(decl.clone()));

		for field in decl.body.fields.iter() {
			field.walk(self);
		}

		while self.current.span != starting_scope.span {
			self.pop_scope();
		}

		FlowControl::Break
	}

	fn visit_field_decl(&mut self, decl: &'a FieldDecl<'a>) -> FlowControl {
		let ident = decl.name.lexeme();
		let kind = decl.name.kind();
		self.current
			.define((ident, kind), Decl::Field(decl.clone()));

		FlowControl::Break
	}

	// --- Function declaration -----------------------------------------------------------

	fn visit_func_decl(&mut self, decl: &'a FunctionDecl<'a>) -> FlowControl {
		// Create an new scope from the opening brace through the end of the current scope,
		// and add the function declaration to it
		let ident = decl.name.lexeme();
		let start = decl.name.span().start;
		let kind = decl.name.kind();

		let starting_scope = self.spawn_for_decl(start);
		self.current = Arc::clone(&starting_scope);
		self.current
			.define((ident, kind), Decl::Function(decl.clone()));

		// Create a narrower scope enclosing just the function body
		let end = decl.body.brace_close.span().start;
		let span = Span { start, end };
		self.current = Scope::with_parent(span, Arc::clone(&self.current));

		// We need to be able to pop this scope before exiting the function body, so instead
		// of just returning control back to the walker, we'll manually traverse our children
		for param in decl.params.iter() {
			param.walk(self);
		}
		decl.body.walk(self);

		// Return back to the enclosing scope
		while self.current.span != starting_scope.span {
			self.pop_scope();
		}

		// Avoid re-visiting child nodes
		FlowControl::Break
	}

	fn visit_param_decl(&mut self, decl: &'a ParamDecl<'a>) -> FlowControl {
		let ident = decl.name.lexeme();
		let kind = decl.name.kind();
		self.current
			.define((ident, kind), Decl::Param(decl.clone()));

		FlowControl::Break
	}

	// --- If statement and friends -------------------------------------------------------

	fn visit_if_stmt(&mut self, stmt: &'a IfStmt<'a>) -> FlowControl {
		let starting_scope = self.spawn_for_block(&stmt.then_branch);
		self.current = Arc::clone(&starting_scope);

		stmt.then_branch.walk(self);

		while self.current.span != starting_scope.span {
			self.pop_scope();
		}
		self.pop_scope();

		FlowControl::Break
	}

	fn visit_elseif_stmt(&mut self, stmt: &'a IfStmt<'a>) -> FlowControl {
		let starting_scope = self.spawn_for_block(&stmt.then_branch);
		self.current = Arc::clone(&starting_scope);

		stmt.then_branch.walk(self);

		while self.current.span != starting_scope.span {
			self.pop_scope();
		}
		self.pop_scope();

		FlowControl::Break
	}

	fn visit_else_stmt(&mut self, stmt: &'a ElseStmt<'a>) -> FlowControl {
		let starting_scope = self.spawn_for_block(&stmt.body);
		self.current = Arc::clone(&starting_scope);

		stmt.body.walk(self);

		while self.current.span != starting_scope.span {
			self.pop_scope();
		}
		self.pop_scope();

		FlowControl::Break
	}

	// --- For statement ------------------------------------------------------------------

	fn visit_for_stmt(&mut self, stmt: &'a ForStmt<'a>) -> FlowControl {
		if let Some(init) = stmt.initializer.as_ref() {
			// Create a new scope from the initializer's semicolon through the end of the
			// statement body, and then walk the initializer
			let start = match init.as_ref() {
				Stmt::Expr(expr) => expr.semicolon.span().end,
				Stmt::Empty(semicolon @ Token::Punct(";", _)) => semicolon.span().end,
				// This will be unreachable with well-formed input, but to avoid crashing the
				// server unnecessarily we'll just bail instead of asserting
				_ => return FlowControl::Break,
			};
			let end = stmt.body.brace_close.span().start;
			let span = Span { start, end };

			let starting_scope = Scope::with_parent(span, Arc::clone(&self.current));
			self.current = Arc::clone(&starting_scope);

			stmt.walk(self);

			while self.current.span != starting_scope.span {
				self.pop_scope();
			}
			self.pop_scope();
		}

		// Create a new scope for the statement body and walk as normal
		let starting_scope = self.spawn_for_block(&stmt.body);
		self.current = Arc::clone(&starting_scope);

		stmt.body.walk(self);

		while self.current.span != starting_scope.span {
			self.pop_scope();
		}
		self.pop_scope();

		FlowControl::Break
	}

	// --- Loop / Continuing --------------------------------------------------------------

	fn visit_loop_stmt(&mut self, stmt: &'a LoopStmt<'a>) -> FlowControl {
		let starting_scope = self.spawn_for_block(&stmt.body);
		self.current = Arc::clone(&starting_scope);

		stmt.body.walk(self);

		while self.current.span != starting_scope.span {
			self.pop_scope();
		}
		self.pop_scope();

		FlowControl::Break
	}

	fn visit_continuing_stmt(&mut self, stmt: &'a ContinuingStmt<'a>) -> FlowControl {
		let starting_scope = self.spawn_for_block(&stmt.body);
		self.current = Arc::clone(&starting_scope);

		stmt.body.walk(self);

		while self.current.span != starting_scope.span {
			self.pop_scope();
		}
		self.pop_scope();

		FlowControl::Break
	}

	// --- Case statements ----------------------------------------------------------------

	fn visit_case_stmt(&mut self, stmt: &'a CaseStmt<'a>) -> FlowControl {
		let starting_scope = self.spawn_for_block(&stmt.body);
		self.current = Arc::clone(&starting_scope);

		stmt.body.walk(self);

		while self.current.span != starting_scope.span {
			self.pop_scope();
		}
		self.pop_scope();

		FlowControl::Break
	}
}
