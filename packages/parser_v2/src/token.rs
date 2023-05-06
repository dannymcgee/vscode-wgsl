use core::fmt;
use std::hash::Hash;

use gramatika::{Span, Spanned, Substr, Token as _};

#[derive(DebugLispToken, Token, Lexer)]
pub enum Token {
	#[pattern = r"[\[\](){}]"]
	Brace(Substr, Span),

	#[discard]
	#[pattern = "//.*"]
	Comment(Substr, Span),

	#[discard]
	#[pattern = "#.*"]
	Pragma(Substr, Span),

	#[subset_of(Ident)]
	#[pattern = r"(array|atomic|bool|[fiu]32|mat[2-4]x[2-4][fh]?|ptr|sampler(_comparison)?|vec[2-4][iufh]?)"]
	#[pattern = r"(texture_multisampled_2d)"]
	#[pattern = r"(texture_external)"]
	#[pattern = r"(texture_depth_(2d|cube)(_array)?)"]
	#[pattern = r"(texture_(1d|2d(_array)?|3d|cube(_array)?))"]
	#[pattern = r"(texture_storage_(1d|2d(_array)?|3d))"]
	Type(Substr, Span),

	#[subset_of(Ident)]
	#[pattern = r"(const|fn|let|struct|type|var|export)"]
	#[pattern = r"(function|private|read(_write)?|storage|uniform|workgroup|write)"]
	#[pattern = r"(break|case|continu(e|ing)|default|else|fallthrough|for|if|loop|return|switch|from)"]
	#[pattern = r"(true|false)"]
	#[pattern = r"(bitcast|discard|enable|import)"]
	Keyword(Substr, Span),

	#[pattern = "[a-zA-Z][0-9a-zA-Z_]*"]
	Ident(Substr, Span),

	#[pattern = r"-?[0-9]*\.[0-9]+([eE][-+]?[0-9]+)?"]
	#[pattern = r"-?[0-9]+\.[0-9]*([eE][-+]?[0-9]+)?"]
	#[pattern = r"-?0x[0-9a-fA-F]+\.[0-9a-fA-F]*([pP][-+]?[0-9]+)?"]
	#[pattern = r"-?0x[0-9a-fA-F]*\.[0-9a-fA-F]+([pP][-+]?[0-9]+)?"]
	#[pattern = r"-?0x[0-9a-fA-F]+[pP][-+]?[0-9]+"]
	FloatLiteral(Substr, Span),

	#[pattern = "0x[0-9a-fA-F]+u"]
	#[pattern = "(0|[1-9][0-9]*)u"]
	UintLiteral(Substr, Span),

	#[pattern = "-?0x[0-9a-fA-F]+"]
	#[pattern = "0|-?[1-9][0-9]*"]
	IntLiteral(Substr, Span),

	#[pattern = r#""[-_ ./a-zA-Z0-9]+""#]
	Path(Substr, Span),

	#[pattern = "->"]
	#[pattern = r"&&?|\|\|?|--?|\+\+?|>>|<<"]
	#[pattern = "[=!<>]=?"]
	#[pattern = "[%*/~^]"]
	Operator(Substr, Span),

	#[pattern = r"::?|[,.;@]"]
	Punct(Substr, Span),

	// Tokens without patterns -- need to be upgraded
	Attribute(Substr, Span),
	Function(Substr, Span),
	Param(Substr, Span),
	Struct(Substr, Span),
	Field(Substr, Span),
	Module(Substr, Span),
}

impl fmt::Debug for Token {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		<Self as gramatika::DebugLisp>::fmt(self, f, 0)
	}
}

impl fmt::Display for Token {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", self.lexeme())
	}
}

impl Hash for Token {
	fn hash<H>(&self, state: &mut H)
	where H: std::hash::Hasher {
		self.kind().hash(state);
		self.lexeme().hash(state);
		self.span().hash(state);
	}
}

impl PartialEq for Token {
	fn eq(&self, other: &Self) -> bool {
		self.kind() == other.kind()
			&& self.lexeme() == other.lexeme()
			&& self.span() == other.span()
	}
}

impl Eq for Token {}
