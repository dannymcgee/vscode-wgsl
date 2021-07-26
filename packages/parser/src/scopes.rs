use std::{collections::HashMap, slice::Iter, sync::Arc};

use lsp_types::{Range, Url};

use crate::{ast::*, AstNode, FindTypeDecl, GetRange, IsWithin, ParentGranularity, ParentOfRange};

pub type Scope = HashMap<String, Arc<Decl>>;

#[derive(Debug)]
pub struct Scopes {
	pub ast: Arc<Vec<Decl>>,
	pub namespaces: HashMap<String, NamespaceScope>,
	pub inner: Vec<(Range, Arc<Scope>)>,
}

#[derive(Debug)]
pub struct ScopeDecl {
	pub uri: Option<Url>,
	pub decl: Arc<Decl>,
}

#[derive(Debug)]
pub struct NamespaceScope {
	pub uri: Url,
	pub scope: Scope,
}

impl Scopes {
	pub fn find_decl(&self, token: &Token) -> Option<ScopeDecl> {
		let (name, token_range) = token.borrow_inner();
		let parent = self
			.ast
			.parent_of(&token_range, ParentGranularity::Expr)
			.unwrap();

		if let AstNode::Expr(Expr::Singular(ref expr)) = &parent {
			// Special handling for dot-accessed fields of variables that bind to structs
			if let Some(PostfixExpr::Dot { ident, range, .. }) = &expr.postfix {
				let (postfix_ident, _) = ident.borrow_inner();

				if token_range.is_within(range) && postfix_ident == name {
					let var_name = if let PrimaryExpr::Identifier(ref token) = expr.expr {
						token
					} else {
						return None;
					};
					let struct_name = &self.find_decl(var_name)?.decl.type_decl()?.name;

					return self.find_decl(struct_name).and_then(|decl| {
						let uri = decl.uri;
						let decl = match decl.decl.as_ref() {
							Decl::Struct(struct_decl) => {
								struct_decl.body.iter().find_map(|field| {
									if field.name.borrow_inner().0 == name {
										Some(Arc::new(Decl::Field(field.clone())))
									} else {
										None
									}
								})
							}
							_ => None,
						};

						decl.map(|decl| ScopeDecl { uri, decl })
					});
				}
			}

			// Special handling for namespaced function calls
			if let PrimaryExpr::FunctionCall(ref expr) = expr.expr {
				if expr.ident.range() == token_range && expr.namespace.is_some() {
					let (namespace, _) = expr.namespace.as_ref().unwrap().borrow_inner();

					return self
						.namespaces
						.get(namespace)
						.and_then(|scope| scope.get(name));
				}
			}
		}

		// Special handling for namespaced types
		if let Some(ty) = &parent.type_decl() {
			if ty.name.range() == token_range && ty.namespace.is_some() {
				let (namespace, _) = ty.namespace.as_ref().unwrap().borrow_inner();

				return self
					.namespaces
					.get(namespace)
					.and_then(|scope| scope.get(name));
			}
		}

		// Find the inner-most scope that encapsulates the token's range, and look up the
		// declaration of the token's identifier
		self.inner.iter().find_map(|(scope_range, scope)| {
			if token_range.is_within(scope_range) {
				scope.get(name).map(|decl| ScopeDecl::new(Arc::clone(decl)))
			} else {
				None
			}
		})
	}

	pub fn iter(&self) -> Iter<'_, (Range, Arc<Scope>)> {
		self.inner.iter()
	}
}

impl ScopeDecl {
	pub fn new(decl: Arc<Decl>) -> Self {
		ScopeDecl { uri: None, decl }
	}

	pub fn namespace(uri: Url, decl: Arc<Decl>) -> Self {
		ScopeDecl {
			uri: Some(uri),
			decl,
		}
	}
}

impl NamespaceScope {
	pub fn get(&self, key: &str) -> Option<ScopeDecl> {
		self.scope
			.get(key)
			.map(|decl| ScopeDecl::namespace(self.uri.clone(), Arc::clone(decl)))
	}
}
