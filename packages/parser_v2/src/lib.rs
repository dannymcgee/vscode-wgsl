#[macro_use]
extern crate gramatika;

pub mod common;
pub mod decl;
pub mod expr;
pub mod fmt;
pub mod scopes;
pub mod stmt;
pub mod token;
pub mod traversal;
pub mod utils;

pub type ParseStream = gramatika::ParseStream<Token, Lexer>;

use decl::Decl;
pub use gramatika::{Parse, ParseStreamer, Result, Span, Spanned};
pub use token::{Lexer, Token, TokenKind, *};

#[derive(DebugLisp)]
pub struct SyntaxTree {
	pub inner: Vec<Decl>,
}

impl Parse for SyntaxTree {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
		let mut inner = vec![];
		let mut recovering = false;

		while !input.is_empty() {
			match input.peek().unwrap() {
				Token::Comment(_, _) => input.discard(),
				_ => match input.parse::<Decl>() {
					Ok(decl) => {
						if recovering {
							recovering = false;
						}
						inner.push(decl);
					}
					Err(err) => {
						if recovering {
							input.discard();
						} else {
							inner.push(Decl::Error(err));
							recovering = true;
						}
					}
				},
			}
		}

		Ok(Self { inner })
	}
}

impl Spanned for SyntaxTree {
	fn span(&self) -> Span {
		self.inner
			.first()
			.map(|first| first.span().through(self.inner.last().unwrap().span()))
			.unwrap_or_else(|| span![0:0...0:0])
	}
}

#[cfg(test)]
mod tests;
