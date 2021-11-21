mod common;
mod decl;
mod e2e;
mod expr;
mod lexer;
mod scopes;
mod stmt;
mod traversal;
mod utils;

use gramatika::{Parse, ParseStream, ParseStreamer};
use similar::{ChangeTag, TextDiff};
use std::fmt;

fn parse<P>(input: &str)
where P: Parse<Stream = crate::ParseStream> + fmt::Debug {
	match ParseStream::from(input).parse::<P>() {
		Ok(tree) => eprintln!("{:#?}", tree),
		Err(err) => {
			eprintln!("{}", err);
			panic!();
		}
	}
}

fn diff(a: &str, b: &str) -> String {
	let diff = TextDiff::from_lines(a, b);

	diff.iter_all_changes()
		.map(|change| {
			let sign = match change.tag() {
				ChangeTag::Insert => "+",
				ChangeTag::Delete => "-",
				ChangeTag::Equal => " ",
			};

			std::format!("{}{}", sign, change)
		})
		.collect()
}
