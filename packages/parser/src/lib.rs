#[macro_use]
extern crate pest_derive;

use pest::{error::Error, iterators::Pairs, Parser as PestParser};
use std::result;

pub use pest;

#[derive(Parser)]
#[grammar = "wgsl.pest"]
pub(crate) struct Parser;

pub type Result<'a> = result::Result<Pairs<'a, Rule>, Error<Rule>>;

pub fn parse(input: &str) -> Result {
	Parser::parse(Rule::program, input)
}

#[cfg(test)]
mod tests;
