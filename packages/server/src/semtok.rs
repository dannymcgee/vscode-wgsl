use std::{collections::HashMap, sync::Arc};

use gramatika::{Span, Spanned, Token as _};
use itertools::Itertools;
use lsp_server::{Message, RequestId, Response};
use lsp_types::{
	SemanticToken, SemanticTokenModifier as TMod, SemanticTokenType as TType, SemanticTokens,
	SemanticTokensLegend, SemanticTokensParams, Url,
};
use parser_v2::{
	common::TypeDecl,
	decl::{Decl, FunctionDecl, ParamDecl, StructDecl, VarDecl},
	expr::{Accessor, Expr, FnCallExpr, IdentExpr, PostfixExpr, PrimaryExpr},
	scopes::Scope,
	traversal::{FlowControl, Visitor, Walk},
	utils::{self, SyntaxNode, ToRange},
	SyntaxTree, Token,
};
use serde_json as json;

use crate::documents_v2::{Document, Documents};

pub fn handle(id: RequestId, params: SemanticTokensParams, docs: &Documents) -> Message {
	let tokens = get_semantic_tokens(params.text_document.uri, docs);
	let result = Some(json::to_value(&tokens).unwrap());

	Message::Response(Response {
		id,
		result,
		error: None,
	})
}

fn get_semantic_tokens(uri: Url, docs: &Documents) -> SemanticTokens {
	let document = if let Some(document) = docs.get(&uri) {
		document
	} else {
		return empty_result();
	};

	let tree = document.ast.as_ref();
	let scopes = Arc::clone(&document.scopes);
	let deps = Arc::clone(&document.deps)
		.iter()
		.map(|(name, uri)| (*name, docs.get(uri).unwrap()))
		.collect::<HashMap<_, _>>();

	let mut builder = SemanticTokensBuilder::new(scopes, deps);
	tree.walk(&mut builder);
	let tokens = builder.build();

	let tokens = tokens
		.iter()
		.sorted_by_key(|(token, _)| token.span())
		.collect_vec();

	let data = tokens
		.iter()
		.enumerate()
		.map(|(idx, (token, (ttype, tmod)))| {
			let delta = if idx == 0 {
				TokenDelta {
					delta_line: token.span().start.line as u32,
					delta_start_char: token.span().start.character as u32,
				}
			} else {
				let (prev, _) = tokens[idx - 1];
				token.delta(prev)
			};

			SemanticToken {
				delta_line: delta.delta_line,
				delta_start: delta.delta_start_char,
				length: token.lexeme().len() as u32,
				token_type: (*ttype) as u32,
				token_modifiers_bitset: *tmod,
			}
		})
		.collect_vec();

	SemanticTokens {
		data,
		result_id: None,
	}
}

fn empty_result() -> SemanticTokens {
	SemanticTokens {
		data: vec![],
		result_id: None,
	}
}

#[derive(Debug)]
struct TokenDelta {
	delta_line: u32,
	delta_start_char: u32,
}

#[derive(Debug, Clone, Copy)]
#[repr(u32)]
#[allow(dead_code)]
enum TokenType {
	Namespace,
	Type,
	Struct,
	Parameter,
	Variable,
	Property,
	Function,
	Macro,
	Keyword,
	Modifier,
	Comment,
	Number,
	String,
	Operator,
}

#[derive(Debug, Clone, Copy)]
#[repr(u32)]
#[allow(dead_code)]
#[rustfmt::skip]
enum TokenMod {
	None     = 0b00000000,
	Decl     = 0b00000001,
	Readonly = 0b00000010,
	Mod      = 0b00000100,
	Doc      = 0b00001000,
	Builtin  = 0b00010000,
}

pub fn legend() -> SemanticTokensLegend {
	SemanticTokensLegend {
		token_types: vec![
			TType::NAMESPACE,
			TType::TYPE,
			TType::STRUCT,
			TType::PARAMETER,
			TType::VARIABLE,
			TType::PROPERTY,
			TType::FUNCTION,
			TType::MACRO,
			TType::KEYWORD,
			TType::MODIFIER,
			TType::COMMENT,
			TType::NUMBER,
			TType::STRING,
			TType::OPERATOR,
		],
		token_modifiers: vec![
			TMod::DECLARATION,
			TMod::READONLY,
			TMod::MODIFICATION,
			TMod::DOCUMENTATION,
			TMod::DEFAULT_LIBRARY,
		],
	}
}

trait Delta {
	fn delta(&self, prev: &Self) -> TokenDelta;
}

impl<'a> Delta for Token<'a> {
	fn delta(&self, prev: &Self) -> TokenDelta {
		let cur_span = self.span();
		let prev_span = prev.span();

		let delta_line = (cur_span.start.line - prev_span.start.line) as u32;
		let delta_start_char =
			if delta_line == 0 && cur_span.start.character >= prev_span.start.character {
				cur_span.start.character - prev_span.start.character
			} else {
				cur_span.start.character
			} as u32;

		TokenDelta {
			delta_line,
			delta_start_char,
		}
	}
}

struct SemanticTokensBuilder<'a> {
	scopes: Arc<Scope<'a>>,
	deps: HashMap<&'a str, &'a Document<'a>>,
	result: HashMap<Token<'a>, (TokenType, u32)>,
}

impl<'a> SemanticTokensBuilder<'a> {
	fn new(scopes: Arc<Scope<'a>>, deps: HashMap<&'a str, &'a Document<'a>>) -> Self {
		Self {
			scopes,
			deps,
			result: HashMap::new(),
		}
	}

	fn build(self) -> HashMap<Token<'a>, (TokenType, u32)> {
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
		if let Some(token) = decl.child_ty {
			self.result
				.insert(decl.name.name, (TokenType::Struct, TokenMod::Builtin as _));
			self.result
				.insert(token, (TokenType::Type, TokenMod::Builtin as _));
		}

		FlowControl::Continue
	}

	fn visit_var_decl(&mut self, decl: &'a VarDecl<'a>) -> FlowControl {
		if self.result.contains_key(&decl.name) {
			return FlowControl::Continue;
		}

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
			// invalid or a built-in function, so we'll assume the latter
			.or_insert((TokenType::Function, TokenMod::Builtin as u32));

		for arg in expr.arguments.arguments.iter() {
			arg.walk(self);
		}

		FlowControl::Break
	}

	fn visit_postfix_expr(&mut self, expr: &'a PostfixExpr<'a>) -> FlowControl {
		if matches!(expr.accessor, Accessor::Index(_)) {
			return FlowControl::Continue;
		}

		if let Expr::Primary(ref inner) = expr.expr.as_ref() {
			if let Expr::Ident(ref inner) = inner.expr.as_ref() {
				self.result.insert(inner.name, (TokenType::Property, 0));
			}
		}

		FlowControl::Continue
	}

	fn visit_ident_expr(&mut self, expr: &'a IdentExpr<'a>) {
		if self.result.contains_key(&expr.name) {
			return;
		}

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
						_ => unreachable!(),
					};

					self.result.insert(name, (ttype, 0));
				}
			}
		} else if let token @ Token::Ident(_, _) = expr.name {
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
		} else {
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
