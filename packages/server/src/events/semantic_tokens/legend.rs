use bitflags::bitflags;
use lsp_types::{SemanticTokenModifier as TMod, SemanticTokenType as TType, SemanticTokensLegend};

#[derive(Debug, Clone, Copy)]
#[repr(u32)]
#[allow(dead_code)]
pub enum TokenType {
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

bitflags! {
	pub struct TokenMod: u32 {
		const NONE     = 0b00000;
		const DECL     = 0b00001;
		const READONLY = 0b00010;
		const MOD      = 0b00100;
		const DOC      = 0b01000;
		const BUILTIN  = 0b10000;
	}
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
