#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate derive_builder;

use anyhow::anyhow;
use lsp_types::{Position, Range};
use pest::{
	error::Error,
	iterators::{Pair, Pairs},
	Parser as PestParser, Span,
};
use std::result;

pub mod ast;
pub use pest;

use ast::{
	AstNode, Attribute, AttributeBuilder, Decl, PrimaryExpr, TypeDecl, TypeDeclBuilder,
	VarDeclBuilder,
};

use crate::ast::{Expr, SingularExpr, Token};

#[derive(Parser)]
#[grammar = "wgsl.pest"]
pub(crate) struct Parser;

pub type Result<T> = result::Result<T, Error<Rule>>;

pub fn parse(input: &str) -> Result<Pairs<Rule>> {
	Parser::parse(Rule::program, input)
}

pub fn parse_ast(input: &str) -> anyhow::Result<Vec<AstNode>> {
	let program = parse(input)?;

	program.parse()
}

trait AstParser {
	fn parse(self) -> anyhow::Result<Vec<AstNode>>;
}

trait AstNodeParser {
	fn parse(self) -> Option<AstNode>;
	fn parse_global_var_decl(self) -> Decl;
	fn parse_attribute_list(self) -> Vec<Attribute>;
	fn parse_type_decl(self) -> TypeDecl;
	fn parse_const_expr(self) -> PrimaryExpr;
}

impl<'a> AstParser for Pairs<'a, Rule> {
	fn parse(mut self) -> anyhow::Result<Vec<AstNode>> {
		let program = self
			.find(|pair| pair.as_rule() == Rule::program)
			.ok_or_else(|| anyhow!("Expected `program` rule"))?;

		Ok(program
			.into_inner()
			.filter_map(|pair| pair.parse())
			.collect())
	}
}

impl<'a> AstNodeParser for Pair<'a, Rule> {
	fn parse(self) -> Option<AstNode> {
		use Rule::*;

		let rule = self.as_rule();

		match rule {
			global_variable_decl => Some(AstNode::Decl(self.parse_global_var_decl())),
			_ => None,
		}
	}

	fn parse_global_var_decl(self) -> Decl {
		use Rule::*;

		let span = self.as_span();

		Decl::Var(
			self.into_inner()
				.fold(&mut VarDeclBuilder::default(), |decl, pair| {
					match pair.as_rule() {
						attribute_list => {
							let attrs = pair.parse_attribute_list();
							decl.attributes(attrs)
						}
						variable_decl => {
							pair.into_inner()
								.fold(decl, |decl, pair| match pair.as_rule() {
									VAR => decl.storage(Token::keyword(pair)),
									variable_qualifier => {
										let qualifiers = pair
											.into_inner()
											.filter_map(|pair| match pair.as_rule() {
												storage_class => Some(Token::keyword(pair)),
												access_mode => Some(Token::keyword(pair)),
												_ => None,
											})
											.collect::<Vec<_>>();

										decl.storage_qualifiers(qualifiers)
									}
									variable_ident_decl => {
										pair.into_inner().fold(decl, |decl, pair| {
											match pair.as_rule() {
												IDENT => decl.name(Token::ident(pair)),
												type_decl => {
													decl.type_decl(Box::new(pair.parse_type_decl()))
												}
												_ => decl,
											}
										})
									}
									_ => decl,
								})
						}
						global_const_initializer => {
							pair.into_inner()
								.fold(decl, |decl, pair| match pair.as_rule() {
									const_expr => decl.assignment(Box::new(Expr::Singular(
										SingularExpr::Primary(pair.parse_const_expr()),
										None,
									))),
									_ => decl,
								})
						}
						_ => decl,
					}
				})
				.range(span.into_range())
				.build()
				.unwrap(),
		)
	}

	fn parse_attribute_list(self) -> Vec<Attribute> {
		use Rule::*;

		self.into_inner()
			.filter_map(|pair| match pair.as_rule() {
				attribute => {
					let span = pair.as_span();

					Some(
						pair.into_inner()
							.fold(&mut AttributeBuilder::default(), |attr, pair| {
								match pair.as_rule() {
									IDENT => attr.name(Token::ident(pair)),
									literal_or_ident => {
										let inner = pair.into_inner().last().unwrap();

										attr.value(match inner.as_rule() {
											FLOAT_LITERAL | INT_LITERAL | UINT_LITERAL => {
												Token::literal(inner)
											}
											IDENT => Token::ident(inner),
											_ => unreachable!(),
										})
									}
									_ => attr,
								}
							})
							.range(span.into_range())
							.build()
							.unwrap(),
					)
				}
				_ => None,
			})
			.collect()
	}

	fn parse_type_decl(self) -> TypeDecl {
		use Rule::*;

		let span = self.as_span();

		self.into_inner()
			.fold(&mut TypeDeclBuilder::default(), |ty, pair| {
				match pair.as_rule() {
					attribute_list => ty.attributes(pair.parse_attribute_list()),
					BOOL | FLOAT32 | INT32 | UINT32 => ty.name(Token::keyword(pair)),
					VEC | PTR | ARRAY | MATRIX | IDENT => ty.name(Token::ident(pair)),
					texture_sampler_type => {
						pair.into_inner().fold(ty, |ty, pair| match pair.as_rule() {
							SAMPLED_TEXTURE_TYPE
							| STORAGE_TEXTURE_TYPE
							| MULTISAMPLED_TEXTURE_TYPE
							| DEPTH_TEXTURE_TYPE
							| SAMPLER_TYPE => ty.name(Token::ident(pair)),
							texel_format | type_decl => {
								ty.component_type(Box::new(pair.parse_type_decl()))
							}
							access_mode => ty.access_mode(Token::keyword(pair)),
							_ => ty,
						})
					}
					type_decl => ty.component_type(Box::new(pair.parse_type_decl())),
					storage_class => ty.storage_class(Token::keyword(pair)),
					access_mode => ty.access_mode(Token::keyword(pair)),
					_ => ty,
				}
			})
			.range(span.into_range())
			.build()
			.unwrap()
	}

	fn parse_const_expr(self) -> PrimaryExpr {
		todo!()
	}
}

impl<'a> Token {
	fn ident(pair: Pair<'a, Rule>) -> Token {
		let text = pair.as_str().to_string();
		let range = pair.as_span().into_range();

		Token::Ident(text, range)
	}

	fn keyword(pair: Pair<'a, Rule>) -> Token {
		let text = pair.as_str().to_string();
		let range = pair.as_span().into_range();

		Token::Keyword(text, range)
	}

	fn punct(pair: Pair<'a, Rule>) -> Token {
		let text = pair.as_str().to_string();
		let range = pair.as_span().into_range();

		Token::Punct(text, range)
	}

	fn op(pair: Pair<'a, Rule>) -> Token {
		let text = pair.as_str().to_string();
		let range = pair.as_span().into_range();

		Token::Op(text, range)
	}

	fn literal(pair: Pair<'a, Rule>) -> Token {
		let text = pair.as_str().to_string();
		let range = pair.as_span().into_range();

		Token::Literal(text, range)
	}
}

trait IntoRange {
	fn into_range(self) -> Range;
}

impl<'a> IntoRange for Span<'a> {
	fn into_range(self) -> Range {
		let (start_line, start_col) = self.start_pos().line_col();
		let (end_line, end_col) = self.end_pos().line_col();

		Range {
			start: Position {
				line: (start_line - 1) as u32,
				character: (start_col - 1) as u32,
			},
			end: Position {
				line: (end_line - 1) as u32,
				character: (end_col - 1) as u32,
			},
		}
	}
}

#[cfg(test)]
mod tests;
