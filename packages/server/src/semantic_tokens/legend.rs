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

#[derive(Debug, Clone, Copy)]
#[repr(u32)]
#[allow(dead_code)]
#[rustfmt::skip]
pub enum TokenMod {
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
