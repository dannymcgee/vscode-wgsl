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
	fmt::{self, Write},
	sync::{Arc, Weak},
};

use gramatika::{Span, Spanned, Token as _};
use parking_lot::RwLock;

use crate::{decl::Decl, traversal::Walk, SyntaxTree, Token, TokenKind};

mod builder;
use builder::ScopeBuilder;

/// A mapping of identifier names to their declarations
pub type Bindings<'a> = HashMap<(&'a str, TokenKind), Arc<Decl<'a>>>;

pub struct Scope<'a> {
	span: Span,
	parent: Option<Weak<Scope<'a>>>,
	children: RwLock<Vec<Arc<Scope<'a>>>>,
	bindings: Arc<RwLock<Bindings<'a>>>,
}

impl<'a> Spanned for Scope<'a> {
	fn span(&self) -> Span {
		self.span
	}
}

trait PrintKeys {
	fn print_keys(&self) -> Result<String, fmt::Error>;
}

impl<'a> PrintKeys for Bindings<'a> {
	fn print_keys(&self) -> Result<String, fmt::Error> {
		let mut out = String::new();
		let f = &mut out;

		for (lexeme, kind) in self.keys() {
			write!(f, "`{}` ({:?}), ", lexeme, kind)?;
		}
		out.pop();
		out.pop();

		Ok(out)
	}
}

impl<'a> fmt::Debug for Scope<'a> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.print(0)?)
	}
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
			children: RwLock::new(vec![]),
			bindings: Arc::new(RwLock::new(HashMap::default())),
		}
	}

	pub fn with_parent(span: Span, parent: Arc<Scope<'a>>) -> Arc<Self> {
		let mut this = Self::new(span);
		this.parent = Some(Arc::downgrade(&parent));

		let arc_this = Arc::new(this);
		parent.children.write().push(Arc::clone(&arc_this));

		arc_this
	}

	pub fn parent(&self) -> Option<Arc<Scope<'a>>> {
		self.parent.as_ref().and_then(|env| env.upgrade())
	}

	pub fn find_decl(&self, token: Token<'a>) -> Option<Arc<Decl<'a>>> {
		// FIXME: This comment block seems like it should work, but it breaks references
		//
		// let lexeme = token.lexeme();
		// let kind = token.kind();
		//
		// let scope = self.find(token)?;
		// let decl = scope
		// 	.bindings
		// 	.read()
		// 	.get(&(lexeme, kind))
		// 	.map(|decl| Arc::clone(decl))?;
		//
		// Some(decl)

		let (lexeme, span) = token.as_inner();
		let kind = token.kind();

		if !self.span.contains(span) {
			None
		} else if self.bindings.read().contains_key(&(lexeme, kind)) {
			self.bindings
				.read()
				.get(&(lexeme, kind))
				.map(|decl| Arc::clone(decl))
		} else {
			self.children
				.read()
				.iter()
				.find_map(|env| env.find_decl(token))
				.as_ref()
				.map(|decl| Arc::clone(decl))
		}
	}

	pub fn find_field_decl(
		&self,
		token: Token<'a>,
		struct_name: Token<'a>,
	) -> Option<Arc<Decl<'a>>> {
		let (lexeme, span) = token.as_inner();
		let field_key = &(lexeme, token.kind());
		let struct_key = &(struct_name.lexeme(), TokenKind::Ident);

		if !self.span.contains(span) {
			None
		} else if self.bindings.read().contains_key(field_key)
			&& self.bindings.read().contains_key(struct_key)
		{
			self.bindings
				.read()
				.get(field_key)
				.map(|decl| Arc::clone(decl))
		} else {
			self.children
				.read()
				.iter()
				.find_map(|env| env.find_field_decl(token, struct_name))
				.as_ref()
				.map(|decl| Arc::clone(decl))
		}
	}

	pub fn find(&self, token: Token<'a>) -> Option<Arc<Scope<'a>>> {
		let (lexeme, span) = token.as_inner();
		let kind = token.kind();

		if !self.span.contains(span) {
			return None;
		}

		// FIXME: This comment block seems like it should work, but it breaks references
		//
		// let child = self.children.read().iter().find_map(|scope| {
		// 	if scope.span.contains(span) && scope.bindings.read().contains_key(&(lexeme, kind)) {
		// 		Some(Arc::clone(scope))
		// 	} else {
		// 		None
		// 	}
		// })?;
		//
		// child.find(token).or(Some(child))

		self.children
			.read()
			.iter()
			.find_map(|scope| {
				if scope.span.contains(span) && scope.bindings.read().contains_key(&(lexeme, kind))
				{
					Some(Arc::clone(scope))
				} else {
					None
				}
			})
			.or_else(|| {
				self.children
					.read()
					.iter()
					.find_map(|scope| scope.find(token))
			})
	}

	fn define(&self, ident: (&'a str, TokenKind), decl: Decl<'a>) {
		self.bindings.write().insert(ident, Arc::new(decl));
	}

	fn print(&self, indent: usize) -> Result<String, fmt::Error> {
		let mut out = String::new();
		let spacing = "   ".repeat(indent);

		writeln!(
			&mut out,
			"{}({:?}) {}",
			spacing,
			self.span,
			self.bindings.read().print_keys()?
		)?;

		for child in self.children.read().iter() {
			write!(&mut out, "{}", child.print(indent + 1)?)?;
		}

		Ok(out)
	}
}
