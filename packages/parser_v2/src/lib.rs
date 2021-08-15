#[macro_use]
extern crate gramatika;

pub mod common;
pub mod decl;
pub mod expr;
pub mod scopes;
pub mod stmt;
pub mod token;
pub mod traversal;
pub mod utils;

pub type ParseStream<'a> = gramatika::ParseStream<'a, Token<'a>, Lexer<'a>>;

use decl::Decl;
pub use gramatika::{Parse, ParseStreamer, Result, Span, Spanned};
pub use token::{Lexer, Token, TokenKind, *};

#[derive(DebugLisp)]
pub struct SyntaxTree<'a> {
	inner: Vec<Decl<'a>>,
}

impl<'a> Parse<'a> for SyntaxTree<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		let mut inner = vec![];
		while !input.is_empty() {
			match input.peek().unwrap() {
				Token::Comment(_, _) => input.discard(),
				_ => inner.push(input.parse::<Decl>()?),
			}
		}

		Ok(Self { inner })
	}
}

impl<'a> Spanned for SyntaxTree<'a> {
	fn span(&self) -> Span {
		self.inner
			.first()
			.unwrap()
			.span()
			.through(self.inner.last().unwrap().span())
	}
}

#[cfg(test)]
mod tests;
