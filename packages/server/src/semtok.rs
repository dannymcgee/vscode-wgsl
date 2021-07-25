use std::{sync::Arc, thread};

use crossbeam::channel::Sender;
use itertools::Itertools;
use lsp_server::{Message, Response};
use lsp_types::{
	request::{Request, SemanticTokensFullRequest},
	SemanticToken, SemanticTokenModifier as TMod, SemanticTokenType as TType, SemanticTokens,
	SemanticTokensLegend, SemanticTokensParams, Url,
};
use parser::{ast::Token, GetRange, Scopes};
use serde_json as json;

use crate::documents;

pub fn handle(req: lsp_server::Request, tx: Sender<Message>) {
	thread::spawn(move || {
		let (id, params) = req
			.extract::<SemanticTokensParams>(SemanticTokensFullRequest::METHOD)
			.unwrap();

		let tokens = get_semantic_tokens(params.text_document.uri);
		let result = Some(json::to_value(&tokens).unwrap());

		tx.send(Message::Response(Response {
			id,
			result,
			error: None,
		}))
		.unwrap();
	});
}

fn get_semantic_tokens(uri: Url) -> SemanticTokens {
	let tokens = match documents::tokens(&uri) {
		Some(tokens) => tokens,
		None => return empty_result(),
	};

	let scopes = match documents::scopes(&uri) {
		Some(scopes) => scopes,
		None => return empty_result(),
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

#[derive(Debug)]
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

		let (lexical_token, token_range) = match token {
			Token::Ident(_, range) => (Variable, range),
			Token::Attr(_, range) => (Macro, range),
			Token::Field(_, range) => (Property, range),
			Token::Type(_, range) => (Type, range),
			Token::Function(_, range) => (Function, range),
			Token::Keyword(_, range) => (Keyword, range),
			Token::Op(_, range) => (Operator, range),
			Token::Module(_, range) => (Namespace, range),
			Token::Literal(text, range) => {
				if text.starts_with('"') {
					(TokenType::String, range)
				} else {
					(Number, range)
				}
			}
			_ => unreachable!(),
		};

		match lexical_token {
			Type | Variable | Function | Parameter => {
				use parser::ast::Decl;

				self.find_decl(token)
					.map(|decl| {
						let decl_mod = if decl.decl.ident().range() == *token_range {
							TokenMod::Decl
						} else {
							TokenMod::None
						} as u32;

						match *decl.decl.as_ref() {
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
							Decl::Module(_) => (Namespace, decl_mod),
							Decl::Extension(_) => unreachable!(),
						}
					})
					.unwrap_or((lexical_token, 0))
			}
			_ => (lexical_token, 0),
		}
	}
}
