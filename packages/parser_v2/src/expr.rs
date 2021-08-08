use gramatika::{Parse, ParseStreamer};

use crate::{ParseStream, Token, *};

#[derive(DebugLisp)]
pub enum Expr<'a> {
	TODO(Vec<Token<'a>>),
}

impl<'a> Parse<'a> for Expr<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		let mut tokens = vec![];
		while !input.check(punct![;]) {
			tokens.push(input.next().unwrap());
		}

		Ok(Expr::TODO(tokens))
	}
}
