//! This module is responsible for building a tree of scopes from an AST, according to the
//! following rules:
//!
//!  * Most declarations spawn a new child scope which is valid through the end of the
//!    scope which contains it. The starting point for declaration scopes depends on the
//!    type of declaration:
//!     - `var`, `let`, `type`, and `struct` bindings become active after their semicolon
//!     - `fn` bindings become active after their opening brace
//!  * Block statements (generally any chunk of code enclosed in curly braces, e.g.
//!    following an `if` statement's condition expression) spawn a new child scope from
//!    the end of the opening brace to the start of the closing brace
//!
//! There are a few special cases to be aware of:
//!
//!  * Parameter bindings are valid within their function body, but don't spawn new scopes
//!    of their own
//!  * Bindings declared in the initializer of a `for` statement are valid within the
//!    statement's body, and within the condition and increment expressions
//!  * Struct fields can only be accessed through a `dot` postfix attached to an
//!    identifier that binds to an instance of the struct, so their declarations are not
//!    directly represented in the scopes tree
//!
//! Once the `Scopes` object has been constructed, the declaration for any given
//! identifier binding can be found by querying the `Scopes` instance for the identifier's
//! `Token`. If the declaration is not found, either something has gone terribly wrong or
//! the identifier is invalid.

use std::{
	collections::HashMap,
	sync::{Arc, Weak},
};

use gramatika::{Position, Span, Spanned, Token as _};
use parking_lot::Mutex;

use crate::{
	decl::{Decl, FunctionDecl, ModuleDecl, ParamDecl, StructDecl, TypeAliasDecl, VarDecl},
	stmt::{BlockStmt, CaseStmt, ContinuingStmt, ElseStmt, ForStmt, IfStmt, LoopStmt, Stmt},
	traversal::{FlowControl, Visitor, Walk},
	SyntaxTree, Token,
};

/// A mapping of identifier names to their declarations
pub type Bindings<'a> = HashMap<&'a str, Arc<Decl<'a>>>;

pub struct Scope<'a> {
	span: Span,
	parent: Option<Weak<Scope<'a>>>,
	children: Mutex<Vec<Arc<Scope<'a>>>>,
	bindings: Arc<Mutex<Bindings<'a>>>,
}

pub fn build<'a>(syntax_tree: &'a SyntaxTree<'a>) -> Arc<Scope<'a>> {
	let span = syntax_tree.span();
	let mut builder = ScopeBuilder::new(span);

	syntax_tree.walk(&mut builder);
	builder.build()
}

impl<'a> Scope<'a> {
	pub fn new(span: Span) -> Self {
		Self {
			span,
			parent: None,
			children: Mutex::new(vec![]),
			bindings: Arc::new(Mutex::new(HashMap::default())),
		}
	}

	pub fn with_parent(span: Span, parent: Arc<Scope<'a>>) -> Arc<Self> {
		let mut this = Self::new(span);
		this.parent = Some(Arc::downgrade(&parent));

		let arc_this = Arc::new(this);
		parent.children.lock().push(Arc::clone(&arc_this));

		arc_this
	}

	pub fn parent(&self) -> Option<Arc<Scope<'a>>> {
		self.parent.as_ref().and_then(|env| env.upgrade())
	}

	pub fn find(&self, token: &Token<'a>) -> Option<Arc<Decl<'a>>> {
		let (lexeme, span) = token.as_inner();

		if !self.span.contains(span) {
			None
		} else if self.bindings.lock().contains_key(lexeme) {
			self.bindings
				.lock()
				.get(lexeme)
				.map(|decl| Arc::clone(decl))
		} else {
			self.children
				.lock()
				.iter()
				.find_map(|env| env.find(token))
				.as_ref()
				.map(|decl| Arc::clone(decl))
		}
	}

	fn define(&self, ident: &'a str, decl: Decl<'a>) {
		self.bindings.lock().insert(ident, Arc::new(decl));
	}
}

struct ScopeBuilder<'a> {
	// We need to hold a persistent reference to the root, because each node holds only a
	// weak reference to its parent, but a strong reference to each of its children. If we
	// didn't keep a separate reference to the root, each parent would be dropped and de-
	// allocated as soon as we spawned and stepped into a new child.
	root: Arc<Scope<'a>>,
	current: Arc<Scope<'a>>,
}

impl<'a> ScopeBuilder<'a> {
	fn new(span: Span) -> Self {
		let root = Arc::new(Scope::new(span));

		Self {
			root: Arc::clone(&root),
			current: root,
		}
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

	fn build(self) -> Arc<Scope<'a>> {
		self.root
	}
}

macro_rules! simple_decl {
	($self:ident, $variant:ident($decl:ident)) => {{
		let ident = $decl.name.lexeme();
		let start = $decl.semicolon.span().end;

		$self.current = $self.spawn_for_decl(start);
		$self.current.define(ident, Decl::$variant($decl.clone()));
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
		simple_decl!(self, Var(decl));

		FlowControl::Break
	}

	fn visit_struct_decl(&mut self, decl: &'a StructDecl<'a>) -> FlowControl {
		simple_decl!(self, Struct(decl));

		FlowControl::Break
	}

	// --- Function declaration -----------------------------------------------------------

	fn visit_func_decl(&mut self, decl: &'a FunctionDecl<'a>) -> FlowControl {
		// Create an new scope from the opening brace through the end of the current scope,
		// and add the function declaration to it
		let ident = decl.name.lexeme();
		let start = decl.body.brace_open.span().end;

		let starting_scope = self.spawn_for_decl(start);
		self.current = Arc::clone(&starting_scope);
		self.current.define(ident, Decl::Function(decl.clone()));

		// Create a narrower scope enclosing just the function body
		self.current = self.spawn_for_block(&decl.body);

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
		self.current.define(ident, Decl::Param(decl.clone()));

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

		FlowControl::Break
	}

	fn visit_elseif_stmt(&mut self, stmt: &'a IfStmt<'a>) -> FlowControl {
		let starting_scope = self.spawn_for_block(&stmt.then_branch);
		self.current = Arc::clone(&starting_scope);

		stmt.then_branch.walk(self);

		while self.current.span != starting_scope.span {
			self.pop_scope();
		}

		FlowControl::Break
	}

	fn visit_else_stmt(&mut self, stmt: &'a ElseStmt<'a>) -> FlowControl {
		let starting_scope = self.spawn_for_block(&stmt.body);
		self.current = Arc::clone(&starting_scope);

		stmt.body.walk(self);

		while self.current.span != starting_scope.span {
			self.pop_scope();
		}

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
		}

		// Create a new scope for the statement body and walk as normal
		let starting_scope = self.spawn_for_block(&stmt.body);
		self.current = Arc::clone(&starting_scope);

		stmt.body.walk(self);

		while self.current.span != starting_scope.span {
			self.pop_scope();
		}

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

		FlowControl::Break
	}

	fn visit_continuing_stmt(&mut self, stmt: &'a ContinuingStmt<'a>) -> FlowControl {
		let starting_scope = self.spawn_for_block(&stmt.body);
		self.current = Arc::clone(&starting_scope);

		stmt.body.walk(self);

		while self.current.span != starting_scope.span {
			self.pop_scope();
		}

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

		FlowControl::Break
	}
}
