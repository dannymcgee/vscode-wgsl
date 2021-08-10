#[macro_use]
extern crate gramatika;

pub mod common;
pub mod decl;
pub mod expr;
pub mod stmt;
pub mod token;
pub mod traversal;

type ParseStream<'a> = gramatika::ParseStream<'a, Token<'a>, Lexer<'a>>;

use decl::Decl;
use gramatika::{Parse, ParseStreamer};
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
			inner.push(input.parse::<Decl>()?);
		}

		Ok(Self { inner })
	}
}

#[cfg(test)]
mod tests;
