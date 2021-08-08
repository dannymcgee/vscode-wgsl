use core::fmt;

use gramatika::Span;

#[derive(Clone, Copy, DebugLispToken, PartialEq, Token, Lexer)]
pub enum Token<'a> {
	#[pattern = r"\[\[?|\]\]?|[(){}]"]
	Brace(&'a str, Span),

	#[pattern = "//.*"]
	Comment(&'a str, Span),

	#[pattern = r"(array|atomic|bool|[fiu]32|mat[2-4]x[2-4]|ptr|sampler(_comparison)?|vec[2-4])\b"]
	#[pattern = r"(texture_multisampled_2d)\b"]
	#[pattern = r"(texture_external)\b"]
	#[pattern = r"(texture_depth_(2d|cube)(_array)?)\b"]
	#[pattern = r"(texture_(1d|2d(_array)?|3d|cube(_array)?))\b"]
	#[pattern = r"(texture_storage_(1d|2d(_array)?|3d))\b"]
	Type(&'a str, Span),

	#[pattern = r"(fn|let|struct|type|var|export)\b"]
	#[pattern = r"(function|private|read(_write)?|storage|uniform|workgroup|write)\b"]
	#[pattern = r"(break|case|continu(e|ing)|default|else(if)?|fallthrough|for|if|loop|return|switch|from)\b"]
	#[pattern = r"(true|false)\b"]
	#[pattern = r"(bitcast|discard|enable|import)\b"]
	Keyword(&'a str, Span),

	#[pattern = "[a-zA-Z][0-9a-zA-Z_]*"]
	Ident(&'a str, Span),

	#[pattern = r"-?[0-9]*\.[0-9]+([eE][-+]?[0-9]+)?"]
	#[pattern = r"-?[0-9]+\.[0-9]*([eE][-+]?[0-9]+)?"]
	#[pattern = r"-?0x[0-9a-fA-F]+\.[0-9a-fA-F]*([pP][-+]?[0-9]+)?"]
	#[pattern = r"-?0x[0-9a-fA-F]*\.[0-9a-fA-F]+([pP][-+]?[0-9]+)?"]
	#[pattern = r"-?0x[0-9a-fA-F]+[pP][-+]?[0-9]+"]
	FloatLiteral(&'a str, Span),

	#[pattern = "0x[0-9a-fA-F]+u"]
	#[pattern = "(0|[1-9][0-9]*)u"]
	UintLiteral(&'a str, Span),

	#[pattern = "-?0x[0-9a-fA-F]+"]
	#[pattern = "0|-?[1-9][0-9]*"]
	IntLiteral(&'a str, Span),

	#[pattern = r#""[-_ ./a-zA-Z0-9]+""#]
	Path(&'a str, Span),

	#[pattern = "->"]
	#[pattern = r"&&?|\|\|?|--?|\+\+?|>>|<<"]
	#[pattern = "[=!<>]=?"]
	#[pattern = "[%*/~^]"]
	Operator(&'a str, Span),

	#[pattern = r"::?|[,.;]"]
	Punct(&'a str, Span),
}
// Attr(&'a str, Span),
// Field(&'a str, Span),
// Function(&'a str, Span),
// Module(&'a str, Span),

impl<'a> fmt::Debug for Token<'a> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		<Self as gramatika::DebugLisp>::fmt(self, f, 0)
	}
}
