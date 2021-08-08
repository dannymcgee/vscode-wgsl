use gramatika::{Parse, Span, Token as _};

use crate::{ParseStream, Token};

#[derive(DebugLisp)]
pub enum Stmt<'a> {
	TODO(Vec<Token<'a>>),
}

impl<'a> Stmt<'a> {
	pub fn span(&self) -> Span {
		match self {
			Stmt::TODO(tokens) => {
				let start = tokens[0].span();
				let end = tokens[tokens.len() - 1].span();

				start.through(end)
			}
		}
	}
}

impl<'a> Parse<'a> for Stmt<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		use Token::*;

		let mut braces = vec![];
		let mut tokens = vec![];

		for token in input {
			match token {
				Brace("{", _) => {
					braces.push(token);
					tokens.push(token);
				}
				Brace("}", _) => {
					braces.pop().unwrap();
					tokens.push(token);
				}
				_ => {
					tokens.push(token);
				}
			}

			if braces.is_empty() {
				break;
			}
		}

		Ok(Stmt::TODO(tokens))
	}
}
