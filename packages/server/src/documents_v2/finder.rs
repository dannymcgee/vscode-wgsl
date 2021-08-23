use std::{collections::HashMap, sync::Arc};

use gramatika::{Spanned, Token as _};
use parser_v2::{
	decl::{Decl, FieldDecl, StructDecl, VarDecl},
	expr::{Expr, IdentExpr, PrimaryExpr},
	scopes::Scope,
	stmt::Stmt,
	traversal::{FlowControl, Visitor},
	Token,
};

use super::Document;

pub struct FindDeclResult<'a> {
	pub source_module: Option<Arc<Decl<'a>>>,
	pub decl: Arc<Decl<'a>>,
	pub parent: Option<Arc<Decl<'a>>>,
}

pub(super) struct DeclFinder<'a> {
	scope: Arc<Scope<'a>>,
	deps: HashMap<&'a str, &'a Document<'a>>,
	needle: Token<'a>,
	result: Option<FindDeclResult<'a>>,
}

impl<'a> DeclFinder<'a> {
	pub fn new(
		scope: Arc<Scope<'a>>,
		deps: HashMap<&'a str, &'a Document<'a>>,
		needle: Token<'a>,
	) -> Self {
		Self {
			scope,
			deps,
			needle,
			result: None,
		}
	}

	pub fn result(&mut self) -> Option<FindDeclResult<'a>> {
		self.result.take()
	}
}

impl<'a> Visitor<'a> for DeclFinder<'a> {
	fn visit_decl(&mut self, decl: &'a Decl<'a>) -> FlowControl {
		if decl.name().span() == self.needle.span() {
			// The needle is the declaration itself
			if let Some(decl) = self.scope.find_decl(self.needle) {
				self.result = Some(FindDeclResult {
					source_module: None,
					decl,
					parent: None,
				});

				FlowControl::Break
			} else {
				FlowControl::Continue
			}
		} else if decl.span().contains(self.needle.span()) {
			FlowControl::Continue
		} else {
			FlowControl::Break
		}
	}

	fn visit_var_decl(&mut self, decl: &'a VarDecl<'a>) -> FlowControl {
		if decl.name.span() == self.needle.span() {
			if let Some(decl) = self.scope.find_decl(self.needle) {
				self.result = Some(FindDeclResult {
					source_module: None,
					decl,
					parent: None,
				});
			}

			FlowControl::Break
		} else if decl.span().contains(self.needle.span()) {
			FlowControl::Continue
		} else {
			FlowControl::Break
		}
	}

	fn visit_struct_decl(&mut self, decl: &'a StructDecl<'a>) -> FlowControl {
		for field in decl.body.fields.iter() {
			if field.name().span() == self.needle.span() {
				let parent = self.scope.find_decl(decl.name);
				self.result = self
					.scope
					.find_field_decl(self.needle, decl.name)
					.map(|decl| FindDeclResult {
						source_module: None,
						decl,
						parent,
					});

				return FlowControl::Break;
			} else if field.span().contains(self.needle.span()) {
				return FlowControl::Continue;
			}
		}

		FlowControl::Break
	}

	fn visit_field_decl(&mut self, decl: &'a FieldDecl<'a>) -> FlowControl {
		if decl.name.span() == self.needle.span() {
			self.result = Some(FindDeclResult {
				source_module: None,
				decl: Arc::new(Decl::Field(decl.clone())),
				parent: None,
			});

			FlowControl::Break
		} else if decl.span().contains(self.needle.span()) {
			FlowControl::Continue
		} else {
			FlowControl::Break
		}
	}

	fn visit_stmt(&mut self, stmt: &'a Stmt<'a>) -> FlowControl {
		if stmt.span().contains(self.needle.span()) {
			FlowControl::Continue
		} else {
			FlowControl::Break
		}
	}

	fn visit_expr(&mut self, expr: &'a Expr<'a>) -> FlowControl {
		if expr.span().contains(self.needle.span()) {
			FlowControl::Continue
		} else {
			FlowControl::Break
		}
	}

	// Special handling for dot-accessed fields of a struct instance
	fn visit_primary_expr(&mut self, expr: &'a PrimaryExpr<'a>) -> FlowControl {
		// If this isn't an ident expression, or if it's a namespaced ident, return early
		// for processing by other `visit` methods. Otherwise, capture the ident name.
		let var_name = if let Expr::Ident(ref inner) = expr.expr.as_ref() {
			if inner.namespace.is_some() {
				return FlowControl::Continue;
			}
			inner.name
		} else {
			return FlowControl::Continue;
		};

		#[rustfmt::skip]
		let (field_name, struct_name) = match expr
			.postfix
			.as_ref()
			// If the postfix contains the needle, then the needle is our field name
			.and_then(|postfix| {
				if postfix.span().contains(self.needle.span())
					&& postfix.expr.span().start == self.needle.span().start
				{
					Some(self.needle)
				} else {
					None
				}
			})
			// Find the struct name by looking up the declaration of the variable being dot-
			// accessed and reading its type
			.and_then(|field_name| {
				let struct_name =
					self.scope
						.find_decl(var_name)
						.and_then(|decl| match decl.as_ref() {
							Decl::Var(inner) | Decl::Const(inner) => {
								inner.ty.as_ref().map(|ty| ty.name.clone())
							}
							Decl::Param(inner) => Some(inner.ty.name.clone()),
							_ => None,
						})?;

				Some((field_name, struct_name))
			})
		{
			// This is basically an `unwrap_or` with an early-return for the `None` case
			Some((field_name, struct_name)) => (field_name, struct_name),
			None => return FlowControl::Continue,
		};

		if let Some(namespace) = struct_name.namespace {
			// Look up the module declaration for inclusion in the result
			let module = self
				.scope
				.find_decl(namespace)
				.and_then(|decl| match decl.as_ref() {
					Decl::Module(_) => Some(decl),
					_ => None,
				});

			// Look up the struct declaration
			let name = struct_name.name;
			if let Some(document) = self.deps.get(namespace.lexeme()) {
				if let Some(struct_decl) = document
					.ast
					.inner
					.iter()
					.find(|decl| decl.name().lexeme() == name.lexeme())
				{
					// Look up the matching field name
					self.result = match struct_decl {
						Decl::Struct(inner) => inner
							.body
							.fields
							.iter()
							.find(|decl| decl.name().lexeme() == field_name.lexeme()),
						_ => None,
					}
					// Map to the final result
					.map(|field_decl| FindDeclResult {
						source_module: module,
						decl: Arc::new(field_decl.clone()),
						parent: Some(Arc::new(struct_decl.clone())),
					});
				}
			}
		} else {
			// The struct is local to this module, so we can look it up directly
			let parent = self.scope.find_decl(struct_name.name);

			self.result = self
				.scope
				.find_field_decl(field_name, struct_name.name)
				.map(|decl| FindDeclResult {
					source_module: None,
					decl,
					parent,
				});
		}

		FlowControl::Break
	}

	fn visit_ident_expr(&mut self, expr: &'a IdentExpr<'a>) {
		// Early return if we've already found the needle or if the needle isn't in this node
		if self.result.is_some() || !expr.span().contains(self.needle.span()) {
			return;
		}

		// Handle namespaced idents
		if let Some(namespace) = expr.namespace {
			// The namespace is the needle
			if namespace.span() == self.needle.span() {
				self.result = self.scope.find_decl(namespace).map(|decl| FindDeclResult {
					source_module: None,
					decl: Arc::clone(&decl),
					parent: None,
				});
			}
			// The name is the needle, so we need to look up the source module
			else if expr.name.span() == self.needle.span() {
				self.result = self.deps.get(namespace.lexeme()).and_then(|document| {
					// Look up the matching declaration in the source module's AST

					// NOTE: This will find the matching declaration even if it isn't `export`
					// -- this is probably desirable behavior for this module, but it does
					// represent an error in the user's code which should be flagged by
					// diagnostics
					document.ast.inner.iter().find_map(|decl| {
						if decl.name().lexeme() == self.needle.lexeme() {
							let module =
								self.scope.find_decl(namespace).and_then(|decl| {
									match decl.as_ref() {
										Decl::Module(_) => Some(decl),
										_ => None,
									}
								});
							Some(FindDeclResult {
								source_module: module,
								decl: Arc::new(decl.clone()),
								parent: None,
							})
						} else {
							None
						}
					})
				});
			}
		}
		// Happy path -- Look up the declaration and return it
		else if expr.name.span() == self.needle.span() {
			self.result = self.scope.find_decl(expr.name).map(|decl| FindDeclResult {
				source_module: None,
				decl,
				parent: None,
			});
		}
	}
}
