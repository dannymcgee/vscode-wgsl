use gramatika::ParseStreamer;

use crate::{scopes, ParseStream, SyntaxTree, Token};

const PROGRAM: &str = include_str!("../../test-files/shader.wgsl");

#[test]
fn scopes() {
	let tree = ParseStream::from(PROGRAM).parse::<SyntaxTree>().unwrap();
	let root = scopes::build(&tree);

	let test_token = Token::Ident("ambient_strength", span![77:35...77:51]);
	if let Some(found) = root.find(&test_token) {
		eprintln!("{:#?}", found.as_ref());
	} else {
		panic!();
	}
}
