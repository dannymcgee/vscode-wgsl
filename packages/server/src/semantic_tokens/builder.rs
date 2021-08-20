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
	result: HashMap<Token<'a>, (TokenType, u32)>,
}

impl<'a> SemanticTokensBuilder<'a> {
	pub fn new(scopes: Arc<Scope<'a>>, deps: HashMap<&'a str, &'a Document<'a>>) -> Self {
		Self {
			scopes,
			deps,
			result: HashMap::new(),
		}
	}

	pub fn build(self) -> HashMap<Token<'a>, (TokenType, u32)> {
		self.result
	}
}

impl<'a> Visitor<'a> for SemanticTokensBuilder<'a> {
	fn visit_decl(&mut self, decl: &'a Decl<'a>) -> FlowControl {
		let (ttype, mut tmod): (TokenType, u32) = match decl {
			Decl::Var(_) => (TokenType::Variable, 0),
			Decl::Const(_) => (TokenType::Variable, TokenMod::Readonly as _),
			Decl::TypeAlias(_) => (TokenType::Type, 0),
			Decl::Struct(_) => (TokenType::Struct, 0),
			Decl::Field(_) => (TokenType::Property, 0),
			Decl::Function(_) => (TokenType::Function, 0),
			Decl::Param(_) => (TokenType::Parameter, 0),
			Decl::Extension(_) => (TokenType::Namespace, 0),
			Decl::Module(_) => (TokenType::Namespace, 0),
		};
		tmod |= TokenMod::Decl as u32;

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
				.insert(decl.name.name, (TokenType::Struct, TokenMod::Builtin as _));
			self.result
				.insert(token, (TokenType::Type, TokenMod::Builtin as _));

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
			"let" => TokenMod::Decl as u32 | TokenMod::Readonly as u32,
			"var" => TokenMod::Decl as u32,
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
			.or_insert((TokenType::Function, TokenMod::Builtin as u32));

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
				self.result.insert(inner.name, (TokenType::Property, 0));
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
			self.result.insert(*namespace, (TokenType::Namespace, 0));

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

					self.result.insert(name, (ttype, 0));
				}
			}
		}
		// Otherwise, try looking up the declaration in the document's scope tree.
		else if let token @ Token::Ident(_, _) = expr.name {
			let sem_tok = self.scopes.find(token).map(|decl| match decl.as_ref() {
				Decl::Struct(_) => (TokenType::Struct, 0),
				Decl::TypeAlias(_) => (TokenType::Type, 0),
				Decl::Function(_) => (TokenType::Function, 0),
				Decl::Var(_) => (TokenType::Variable, 0),
				Decl::Const(_) => (TokenType::Variable, TokenMod::Readonly as u32),
				Decl::Field(_) => unreachable!(),
				Decl::Param(_) => (TokenType::Parameter, 0),
				Decl::Extension(_) => (TokenType::Namespace, 0),
				Decl::Module(_) => (TokenType::Namespace, 0),
			});

			if let Some((ttype, tmod)) = sem_tok {
				self.result.insert(token, (ttype, tmod));
			}
		}
		// I'm pretty sure this block is only actually handling primitive types via the
		// `Type` token right now. See the TODO note in `parser_v2::tokens`.
		else {
			let sem_tok = match expr.name {
				Token::Type(_, _) => Some((TokenType::Type, TokenMod::Builtin as u32)),
				Token::Attribute(_, _) => Some((TokenType::Macro, 0)),
				Token::Function(_, _) => Some((TokenType::Function, TokenMod::Builtin as u32)),
				Token::Field(_, _) => Some((TokenType::Property, 0)),
				_ => None,
			};

			if let Some((ttype, tmod)) = sem_tok {
				self.result.insert(expr.name, (ttype, tmod));
			}
		}
	}
}
