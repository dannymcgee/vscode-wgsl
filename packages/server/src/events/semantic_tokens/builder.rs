use std::{collections::HashMap, sync::Arc};

use gramatika::Token as _;
use parser_v2::{
	common::TypeDecl,
	decl::{Decl, VarDecl},
	expr::{Accessor, Expr, FnCallExpr, IdentExpr, PostfixExpr},
	scopes::Scope,
	traversal::{FlowControl, Visitor, Walk},
	Token,
};

use super::{TokenMod, TokenType};
use crate::documents_v2::Document;

pub struct SemanticTokensBuilder<'a> {
	scopes: Arc<Scope<'a>>,
	deps: HashMap<&'a str, &'a Document<'a>>,
	result: HashMap<Token<'a>, (TokenType, TokenMod)>,
}

impl<'a> SemanticTokensBuilder<'a> {
	pub fn new(scopes: Arc<Scope<'a>>, deps: HashMap<&'a str, &'a Document<'a>>) -> Self {
		Self {
			scopes,
			deps,
			result: HashMap::new(),
		}
	}

	pub fn build(self) -> HashMap<Token<'a>, (TokenType, TokenMod)> {
		self.result
	}
}

impl<'a> Visitor<'a> for SemanticTokensBuilder<'a> {
	fn visit_decl(&mut self, decl: &'a Decl<'a>) -> FlowControl {
		let (ttype, tmod): (TokenType, TokenMod) = match decl {
			Decl::Var(_) => (TokenType::Variable, TokenMod::DECL),
			Decl::Const(_) => (TokenType::Variable, TokenMod::DECL | TokenMod::READONLY),
			Decl::TypeAlias(_) => (TokenType::Type, TokenMod::DECL),
			Decl::Struct(_) => (TokenType::Struct, TokenMod::DECL),
			Decl::Field(_) => (TokenType::Property, TokenMod::DECL),
			Decl::Function(_) => (TokenType::Function, TokenMod::DECL),
			Decl::Param(_) => (TokenType::Parameter, TokenMod::DECL),
			Decl::Extension(_) => (TokenType::Namespace, TokenMod::DECL),
			Decl::Module(_) => (TokenType::Namespace, TokenMod::DECL),
		};
		self.result.insert(decl.name(), (ttype, tmod));

		FlowControl::Continue
	}

	fn visit_type(&mut self, decl: &'a TypeDecl<'a>) -> FlowControl {
		// If this is a generic type (e.g. `vec3<f32>`), we need to register the component
		// type here. We also mark the parent type as a built-in struct (struct because it
		// has fields, and built-in because it's not currently possible for users to define
		// generic types).
		if let Some(token) = decl.child_ty {
			self.result
				.insert(decl.name.name, (TokenType::Struct, TokenMod::BUILTIN));
			self.result
				.insert(token, (TokenType::Type, TokenMod::BUILTIN));

			// Avoid trying to re-visit the ident expression
			FlowControl::Break
		} else {
			FlowControl::Continue
		}
	}

	fn visit_var_decl(&mut self, decl: &'a VarDecl<'a>) -> FlowControl {
		// This implementation is necessary for variable statements, which wrap a `VarDecl`
		// struct directly, but if it's a top-level declaration then `visit_decl` will have
		// already handled it.
		if self.result.contains_key(&decl.name) {
			return FlowControl::Continue;
		}

		// Without the `Decl` enum wrapper, we need to check the storage keyword to
		// determine the mutability modifier.
		let tmod = match decl.storage.lexeme() {
			"let" => TokenMod::DECL | TokenMod::READONLY,
			"var" => TokenMod::DECL,
			_ => unreachable!(),
		};
		self.result.insert(decl.name, (TokenType::Variable, tmod));

		FlowControl::Continue
	}

	fn visit_fn_call_expr(&mut self, expr: &'a FnCallExpr<'a>) -> FlowControl {
		self.visit_ident_expr(&expr.ident);
		self.result
			.entry(expr.ident.name)
			// If `visit_ident_expr` didn't register a token for this, it means it couldn't
			// find a declaration in scope for the identifier -- that means it's either
			// invalid or a built-in function, so we'll assume the latter.
			.or_insert((TokenType::Function, TokenMod::BUILTIN));

		// We break here to avoid re-visiting the ident, so walk the arguments manually.
		for arg in expr.arguments.arguments.iter() {
			arg.walk(self);
		}

		FlowControl::Break
	}

	fn visit_postfix_expr(&mut self, expr: &'a PostfixExpr<'a>) -> FlowControl {
		// A bracketed postfix ident will be correctly handled by `visit_ident_expr`.
		if matches!(expr.accessor, Accessor::Index(_)) {
			return FlowControl::Continue;
		}

		// If the postfix expression is a function call, it'll be handled by
		// `visit_fn_call_expr`. Otherwise, it's a field. Looking up a field declaration is
		// tricky with the current implementation, so we can skip a few extra steps by
		// registering it here instead.
		if let Expr::Primary(ref inner) = expr.expr.as_ref() {
			if let Expr::Ident(ref inner) = inner.expr.as_ref() {
				self.result
					.insert(inner.name, (TokenType::Property, TokenMod::NONE));
			}
		}

		FlowControl::Continue
	}

	fn visit_ident_expr(&mut self, expr: &'a IdentExpr<'a>) {
		// Avoid overwriting our work if one of the previous special-case handlers already
		// handled this expression.
		if self.result.contains_key(&expr.name) {
			return;
		}

		// If it's a namespaced identifier, we need to look up the declaration in the
		// dependency's source document.
		if let Some(ref namespace) = expr.namespace {
			self.result
				.insert(*namespace, (TokenType::Namespace, TokenMod::NONE));

			let name = expr.name;
			if let Some(document) = self.deps.get(namespace.lexeme()) {
				if let Some(decl) = document
					.ast
					.inner
					.iter()
					.find(|decl| decl.name().lexeme() == name.lexeme())
				{
					let ttype = match decl {
						Decl::Struct(_) => TokenType::Struct,
						Decl::TypeAlias(_) => TokenType::Type,
						Decl::Function(_) => TokenType::Function,
						// Only the above declarations support being exported
						_ => unreachable!(),
					};

					self.result.insert(name, (ttype, TokenMod::NONE));
				}
			}
		}
		// Otherwise, try looking up the declaration in the document's scope tree.
		else if let token @ Token::Ident(_, _) = expr.name {
			let sem_tok = self
				.scopes
				.find_decl(token)
				.map(|decl| match decl.as_ref() {
					Decl::Struct(_) => (TokenType::Struct, TokenMod::NONE),
					Decl::TypeAlias(_) => (TokenType::Type, TokenMod::NONE),
					Decl::Function(_) => (TokenType::Function, TokenMod::NONE),
					Decl::Var(_) => (TokenType::Variable, TokenMod::NONE),
					Decl::Const(_) => (TokenType::Variable, TokenMod::READONLY),
					Decl::Field(_) => unreachable!(),
					Decl::Param(_) => (TokenType::Parameter, TokenMod::NONE),
					Decl::Extension(_) => (TokenType::Namespace, TokenMod::NONE),
					Decl::Module(_) => (TokenType::Namespace, TokenMod::NONE),
				});

			if let Some((ttype, tmod)) = sem_tok {
				self.result.insert(token, (ttype, tmod));
			}
		}
		// I'm pretty sure this block is only actually handling primitive types via the
		// `Type` token right now. See the TODO note in `parser_v2::tokens`.
		else {
			let sem_tok = match expr.name {
				Token::Type(_, _) => Some((TokenType::Type, TokenMod::BUILTIN)),
				Token::Attribute(_, _) => Some((TokenType::Macro, TokenMod::NONE)),
				Token::Function(_, _) => Some((TokenType::Function, TokenMod::BUILTIN)),
				Token::Field(_, _) => Some((TokenType::Property, TokenMod::NONE)),
				_ => None,
			};

			if let Some((ttype, tmod)) = sem_tok {
				self.result.insert(expr.name, (ttype, tmod));
			}
		}
	}
}