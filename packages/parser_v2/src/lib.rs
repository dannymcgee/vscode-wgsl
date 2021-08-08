#[macro_use]
extern crate gramatika;

pub mod common;
pub mod decl;
pub mod expr;
pub mod stmt;
pub mod token;

type ParseStream<'a> = gramatika::ParseStream<'a, Token<'a>, Lexer<'a>>;

pub use token::*;

#[cfg(test)]
mod tests;
