use std::{sync::Arc, thread};

use crossbeam_channel::Sender;
use lsp_server::{Message, RequestId, Response};
use lsp_types::{
	request::{Request, SemanticTokensFullRequest},
	SemanticToken, SemanticTokenModifier as TMod, SemanticTokenType as TType, SemanticTokens,
	SemanticTokensLegend, SemanticTokensParams,
};
use parser::{ast::Token, GetRange, IsWithin};
use serde_json as json;

use crate::documents::{self, Scopes};

pub fn handle(req: lsp_server::Request, tx: Sender<Message>) {
	thread::spawn(move || {
		let (id, params) = req
			.extract::<SemanticTokensParams>(SemanticTokensFullRequest::METHOD)
			.unwrap();

		let tokens = match documents::tokens(&params.text_document.uri) {
			Some(tokens) => tokens,
			None => {
				let _ = tx.send(null_result(id));
				return;
			}
		};
		let scopes = match documents::scopes(&params.text_document.uri) {
			Some(scopes) => scopes,
			None => {
				let _ = tx.send(null_result(id));
				return;
			}
		};

		let deltas = get_deltas(tokens.clone());
		let data = tokens
			.iter()
			.enumerate()
			.map(|(idx, token)| {
				let (token_type, token_modifiers_bitset) = scopes.get_token_type(&token);
				let (text, _) = token.borrow_inner();

				SemanticToken {
					delta_line: deltas[idx].delta_line,
					delta_start: deltas[idx].delta_start_char,
					length: text.len() as u32,
					token_type: token_type as u32,
					token_modifiers_bitset,
				}
			})
			.collect::<Vec<_>>();

		let result = SemanticTokens {
			data,
			result_id: None,
		};

		let _ = tx.send(Message::Response(Response {
			id,
			result: Some(json::to_value(&result).unwrap()),
			error: None,
		}));
	});
}

#[derive(Debug)]
struct TokenDelta {
	delta_line: u32,
	delta_start_char: u32,
}

#[derive(Debug)]
#[repr(u32)]
#[allow(dead_code)]
enum TokenType {
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
	Operator,
}

#[derive(Debug)]
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

fn get_deltas(tokens: Arc<Vec<Token>>) -> Vec<TokenDelta> {
	tokens
		.iter()
		.map(|token| token.range())
		.enumerate()
		.map(|(idx, range)| {
			if idx == 0 {
				TokenDelta {
					delta_line: range.start.line,
					delta_start_char: range.start.character,
				}
			} else {
				let prev = tokens[idx - 1].range();

				let delta_line = range.start.line - prev.start.line;
				let delta_start_char =
					if delta_line == 0 && range.start.character >= prev.start.character {
						range.start.character - prev.start.character
					} else {
						range.start.character
					};

				TokenDelta {
					delta_line,
					delta_start_char,
				}
			}
		})
		.collect()
}

trait GetTokenType {
	fn get_token_type(&self, token: &Token) -> (TokenType, u32);
}

impl GetTokenType for Scopes {
	fn get_token_type(&self, token: &Token) -> (TokenType, u32) {
		use TokenType::*;

		let (lexical_token, name, token_range) = match token {
			Token::Ident(text, range) => (Variable, text, range),
			Token::Attr(text, range) => (Macro, text, range),
			Token::Field(text, range) => (Property, text, range),
			Token::Type(text, range) => (Type, text, range),
			Token::Function(text, range) => (Function, text, range),
			Token::Keyword(text, range) => (Keyword, text, range),
			Token::Op(text, range) => (Operator, text, range),
			Token::Literal(text, range) => (Number, text, range),
			_ => unreachable!(),
		};

		match lexical_token {
			Type | Variable | Function | Parameter => {
				use parser::ast::Decl;

				let found = self.iter().find_map(|(scope_range, scope)| {
					if token_range.is_within(scope_range) {
						scope.get(name).map(|decl| {
							let decl_mod = if decl.ident().range() == *token_range {
								TokenMod::Decl
							} else {
								TokenMod::None
							} as u32;

							match *decl.as_ref() {
								Decl::Var(ref decl) => {
									let (storage, _) = decl.storage.borrow_inner();
									let mods = if storage == "let" {
										decl_mod | TokenMod::Readonly as u32
									} else {
										decl_mod
									};
									(Variable, mods)
								}
								Decl::Const(_) => (Variable, decl_mod | TokenMod::Readonly as u32),
								Decl::TypeAlias(_) => (Type, decl_mod),
								Decl::Struct(_) => (Struct, decl_mod),
								Decl::Field(_) => (Property, decl_mod),
								Decl::Function(_) => (Function, decl_mod),
								Decl::Param(_) => (Parameter, decl_mod),
							}
						})
					} else {
						None
					}
				});

				found.unwrap_or((lexical_token, 0))
			}
			_ => (lexical_token, 0),
		}
	}
}

fn null_result(id: RequestId) -> Message {
	let result = SemanticTokens::default();

	Message::Response(Response {
		id,
		result: Some(json::to_value(&result).unwrap()),
		error: None,
	})
}
