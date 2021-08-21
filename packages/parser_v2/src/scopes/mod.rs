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

use gramatika::{Span, Spanned};
use parking_lot::Mutex;

use crate::{decl::Decl, traversal::Walk, SyntaxTree, Token};

mod builder;
use builder::ScopeBuilder;

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

	// FIXME - This currently searches from the outermost scope inwards, which means:
	//   1. shadowed declarations won't be correctly discovered
	//   2. it's way less efficient than it should be
	pub fn find_decl(&self, token: Token<'a>) -> Option<Arc<Decl<'a>>> {
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
				.find_map(|env| env.find_decl(token))
				.as_ref()
				.map(|decl| Arc::clone(decl))
		}
	}

	pub fn find(&self, token: Token<'a>) -> Option<Arc<Scope<'a>>> {
		let (lexeme, span) = token.as_inner();

		if !self.span.contains(span) {
			return None;
		}

		self.children
			.lock()
			.iter()
			.find_map(|scope| {
				if scope.span.contains(span) && scope.bindings.lock().contains_key(lexeme) {
					Some(Arc::clone(scope))
				} else {
					None
				}
			})
			.or_else(|| {
				self.children
					.lock()
					.iter()
					.find_map(|scope| scope.find(token))
			})
	}

	fn define(&self, ident: &'a str, decl: Decl<'a>) {
		self.bindings.lock().insert(ident, Arc::new(decl));
	}
}
