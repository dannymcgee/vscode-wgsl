use gramatika::{Lexer as _, Span, Token as _};

use crate::{token, Token, TokenKind};

macro_rules! match_tokens {
	($ttype:ident, [$($input:literal),+$(,)?]) => {{
		let inputs = &[$($input),+];

		for input in inputs {
			assert_eq!(
				lex(*input)
					.iter()
					.map(|tok| tok.as_matchable())
					.collect::<Vec<_>>(),
				vec![(
					TokenKind::$ttype,
					*input,
					Span::new((0,0), (0,input.len()))
				)],
			);
		}
	}}
}

#[test]
fn two_char_tokens() {
	match_tokens!(Operator, [
		"&&", "||", "--", "++", "==", ">>", "<<", "!=", "<=", ">=", "->",
	]);
	match_tokens!(Brace, ["[[", "]]"]);
	match_tokens!(Punct, ["::"]);
}

#[test]
fn one_char_tokens() {
	match_tokens!(Operator, [
		"&", "|", "-", "+", "=", ">", "<", "!", "%", "*", "/", "~", "^"
	]);
	match_tokens!(Brace, ["[", "]", "{", "}", "(", ")"]);
	match_tokens!(Punct, [";", ",", ".", ";"]);
}

#[test]
fn comment() {
	match_tokens!(Comment, ["// This is a line comment"]);
}

#[test]
fn comment_with_follow() {
	use Token::*;

	let input = r#"
// This is a line comment
;
"#;

	assert_eq!(lex(input), vec![
		Comment(
			literal_substr!("// This is a line comment"),
			span![1:0...1:25]
		),
		Punct(literal_substr!(";"), span![2:0...2:1]),
	])
}

#[test]
fn decimal_float_literal() {
	match_tokens!(FloatLiteral, [
		"42.42",
		"-42.42",
		".42",
		"42.",
		"-42.",
		"-.42",
		"42.42e10",
		"42.42e-10",
		"42.42e+10",
		"42.e10",
		"42.e-10",
		"42.e+10",
		".42e10",
		".42e-10",
		".42e+10",
	]);
}

#[test]
fn hex_float_literal() {
	match_tokens!(FloatLiteral, [
		"0x42a.42",
		"-0x42a.42",
		"0x.42a",
		"0x42a.",
		"-0x42a.",
		"-0x.42a",
		"-0x42a.42p10",
		"0x42a.42p-10",
		"0x42a.42p+10",
		"0x42a.p10",
		"0x42a.p-10",
		"0x42a.p+10",
		"0x.42Ap10",
		"0x.42Ap-10",
		"0x.42Ap+10",
		"0x42Ap10",
		"0x42Ap-10",
		"0x42Ap+10",
	]);
}

#[test]
fn int_literal() {
	match_tokens!(IntLiteral, ["0", "42", "-42", "0x42a", "-0x42a"]);
}

#[test]
fn uint_literal() {
	match_tokens!(UintLiteral, ["0u", "42u", "0x42Au"]);
}

#[test]
fn path() {
	match_tokens!(Path, [
		r#""foo""#,
		r#""./foo""#,
		r#""./foo/bar/baz/foo-bar""#,
		r#""./My Documents/Lorem Ipsum""#,
	]);
}

#[test]
fn types() {
	match_tokens!(Type, [
		"array",
		"atomic",
		"bool",
		"f32",
		"i32",
		"u32",
		"mat2x2",
		"mat2x3",
		"mat2x4",
		"mat3x2",
		"mat3x3",
		"mat3x4",
		"mat4x2",
		"mat4x3",
		"mat4x4",
		"ptr",
		"sampler",
		"sampler_comparison",
		"vec2",
		"vec3",
		"vec4",
		"texture_multisampled_2d",
		"texture_external",
		"texture_depth_2d",
		"texture_depth_2d_array",
		"texture_depth_cube",
		"texture_depth_cube_array",
		"texture_1d",
		"texture_2d",
		"texture_2d_array",
		"texture_3d",
		"texture_cube",
		"texture_cube_array",
		"texture_storage_1d",
		"texture_storage_2d",
		"texture_storage_2d_array",
		"texture_storage_3d",
	]);
}

#[test]
fn keywords() {
	match_tokens!(Keyword, [
		"fn",
		"let",
		"struct",
		"type",
		"var",
		"export",
		"function",
		"private",
		"read",
		"read_write",
		"storage",
		"uniform",
		"workgroup",
		"write",
		"break",
		"case",
		"continue",
		"continuing",
		"default",
		"else",
		"elseif",
		"fallthrough",
		"for",
		"if",
		"loop",
		"return",
		"switch",
		"from",
		"true",
		"false",
		"bitcast",
		"discard",
		"enable",
		"import",
	]);
}

#[test]
fn identifiers() {
	match_tokens!(Ident, [
		"foo",
		"bar",
		"baz",
		"foo_bar",
		"foo2",
		"loremIpsumDolor",
		"SitAmet",
		"LOUD_NOISES",
	]);
}

#[test]
fn kitchen_sink() {
	use Token::*;

	let input = "var<uniform> uniforms: Uniforms;";
	#[rustfmt::skip]
	assert_eq!(lex(input), vec![
		Keyword( literal_substr!("var"),      span![0:0...0:3]),
		Operator(literal_substr!("<"),        span![0:3...0:4]),
		Keyword( literal_substr!("uniform"),  span![0:4...0:11]),
		Operator(literal_substr!(">"),        span![0:11...0:12]),
		Ident(   literal_substr!("uniforms"), span![0:13...0:21]),
		Punct(   literal_substr!(":"),        span![0:21...0:22]),
		Ident(   literal_substr!("Uniforms"), span![0:23...0:31]),
		Punct(   literal_substr!(";"),        span![0:31...0:32]),
	]);

	let input = "[[group(0), binding(0)]] var t_diffuse: texture_2d<f32>;";
	#[rustfmt::skip]
	assert_eq!(lex(input), vec![
		Brace(     literal_substr!("[["),         span![0:0...0:2]),
		Ident(     literal_substr!("group"),      span![0:2...0:7]),
		Brace(     literal_substr!("("),          span![0:7...0:8]),
		IntLiteral(literal_substr!("0"),          span![0:8...0:9]),
		Brace(     literal_substr!(")"),          span![0:9...0:10]),
		Punct(     literal_substr!(","),          span![0:10...0:11]),
		Ident(     literal_substr!("binding"),    span![0:12...0:19]),
		Brace(     literal_substr!("("),          span![0:19...0:20]),
		IntLiteral(literal_substr!("0"),          span![0:20...0:21]),
		Brace(     literal_substr!(")"),          span![0:21...0:22]),
		Brace(     literal_substr!("]]"),         span![0:22...0:24]),
		Keyword(   literal_substr!("var"),        span![0:25...0:28]),
		Ident(     literal_substr!("t_diffuse"),  span![0:29...0:38]),
		Punct(     literal_substr!(":"),          span![0:38...0:39]),
		Type(      literal_substr!("texture_2d"), span![0:40...0:50]),
		Operator(  literal_substr!("<"),          span![0:50...0:51]),
		Type(      literal_substr!("f32"),        span![0:51...0:54]),
		Operator(  literal_substr!(">"),          span![0:54...0:55]),
		Punct(     literal_substr!(";"),          span![0:55...0:56]),
	]);

	let input = r#"
// Here's a comment describing the next line
[[group(0), binding(0)]]
var t_diffuse: texture_2d<f32>;
	"#;
	#[rustfmt::skip]
	assert_eq!(lex(input), vec![
		// Line 1
		Comment(literal_substr!("// Here's a comment describing the next line"), span![1:0...1:44]),
		// Line 2
		Brace(     literal_substr!("[["),      span![2:0...2:2]),
		Ident(     literal_substr!("group"),   span![2:2...2:7]),
		Brace(     literal_substr!("("),       span![2:7...2:8]),
		IntLiteral(literal_substr!("0"),       span![2:8...2:9]),
		Brace(     literal_substr!(")"),       span![2:9...2:10]),
		Punct(     literal_substr!(","),       span![2:10...2:11]),
		Ident(     literal_substr!("binding"), span![2:12...2:19]),
		Brace(     literal_substr!("("),       span![2:19...2:20]),
		IntLiteral(literal_substr!("0"),       span![2:20...2:21]),
		Brace(     literal_substr!(")"),       span![2:21...2:22]),
		Brace(     literal_substr!("]]"),      span![2:22...2:24]),
		// Line 3
		Keyword( literal_substr!("var"),        span![3:0...3:3]),
		Ident(   literal_substr!("t_diffuse"),  span![3:4...3:13]),
		Punct(   literal_substr!(":"),          span![3:13...3:14]),
		Type(    literal_substr!("texture_2d"), span![3:15...3:25]),
		Operator(literal_substr!("<"),          span![3:25...3:26]),
		Type(    literal_substr!("f32"),        span![3:26...3:29]),
		Operator(literal_substr!(">"),          span![3:29...3:30]),
		Punct(   literal_substr!(";"),          span![3:30...3:31]),
	]);
}

fn lex(input: &str) -> Vec<Token> {
	token::Lexer::new(input.into()).scan()
}
