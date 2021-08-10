mod common;
mod decl;
mod expr;
mod lexer;
mod stmt;

use gramatika::{Parse, ParseStream, ParseStreamer};
use std::fmt;

fn parse<'a, P>(input: &'a str)
where
	P: Parse<'a, Stream = crate::ParseStream<'a>> + fmt::Debug,
{
	match ParseStream::from(input).parse::<P>() {
		Ok(tree) => eprintln!("{:#?}", tree),
		Err(err) => {
			eprintln!("{}", err);
			panic!();
		}
	}
}
