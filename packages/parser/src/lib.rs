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
#[macro_use]
mod macros;

pub use pest;

use ast::{
	AstNode, Attribute, AttributeBuilder, Decl, ElseStmt, ElseStmtBuilder, ElseifStmt,
	ElseifStmtBuilder, FunctionDeclBuilder, FunctionParam, FunctionParamBuilder,
	FunctionSignatureBuilder, IfStmt, IfStmtBuilder, PrimaryExpr, ReturnStmt, ReturnStmtBuilder,
	Stmt, StructDeclBuilder, StructField, StructFieldBuilder, TypeCtorExprBuilder, TypeDecl,
	TypeDeclBuilder, VarDeclBuilder,
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
	fn parse_global_const_decl(self) -> Decl;
	fn parse_type_alias_decl(self) -> Decl;
	fn parse_struct_decl(self) -> Decl;
	fn parse_struct_body(self) -> Vec<StructField>;
	fn parse_func_decl(self) -> Decl;
	fn parse_param_list(self) -> Vec<FunctionParam>;
	fn parse_attribute_list(self) -> Vec<Attribute>;
	fn parse_type_decl(self) -> TypeDecl;
	fn parse_const_expr(self) -> PrimaryExpr;
	fn parse_compound_stmt(self) -> Vec<Stmt>;
	fn parse_statement(self) -> Stmt;
	fn parse_return_stmt(self) -> ReturnStmt;
	fn parse_if_stmt(self) -> IfStmt;
	fn parse_elseif_stmt(self) -> ElseifStmt;
	fn parse_else_stmt(self) -> ElseStmt;
	fn parse_expression(self) -> Expr;
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
			global_constant_decl => Some(AstNode::Decl(self.parse_global_const_decl())),
			struct_decl => Some(AstNode::Decl(self.parse_struct_decl())),
			func_decl => Some(AstNode::Decl(self.parse_func_decl())),
			_ => None,
		}
	}

	fn parse_global_var_decl(self) -> Decl {
		use Rule::*;

		let span = self.as_span();
		let decl = &mut VarDeclBuilder::default();

		let decl = fold_children!(self, decl, pair {
			attribute_list => decl.attributes(pair.parse_attribute_list()),
			variable_decl => fold_children!(pair, decl, pair {
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
				},
				variable_ident_decl => fold_children!(pair, decl, pair {
					IDENT => decl.name(Token::ident(pair)),
					type_decl => decl.type_decl(Box::new(pair.parse_type_decl())),
					_ => decl,
				}),
				_ => decl,
			}),
			global_const_initializer => fold_children!(pair, decl, pair {
				const_expr => decl.assignment(Box::new(Expr::Singular(
					SingularExpr::Primary(pair.parse_const_expr()),
					None,
				))),
				_ => decl,
			}),
			_ => decl,
		})
		.range(span.into_range())
		.build()
		.unwrap();

		Decl::Var(decl)
	}

	fn parse_global_const_decl(self) -> Decl {
		use Rule::*;

		let span = self.as_span();
		let decl = &mut VarDeclBuilder::default();

		Decl::Const(
			fold_children!(self, decl, pair {
				attribute_list => decl.attributes(pair.parse_attribute_list()),
				LET => decl.storage(Token::keyword(pair)),
				variable_ident_decl => fold_children!(pair, decl, pair {
					IDENT => decl.name(Token::ident(pair)),
					type_decl => decl.type_decl(Box::new(pair.parse_type_decl())),
					_ => decl,
				}),
				global_const_initializer => fold_children!(pair, decl, pair {
					const_expr => decl.assignment(Box::new(Expr::Singular(
						SingularExpr::Primary(pair.parse_const_expr()),
						None,
					))),
					_ => decl,
				}),
				_ => decl,
			})
			.range(span.into_range())
			.build()
			.unwrap(),
		)
	}

	fn parse_type_alias_decl(self) -> Decl {
		todo!()
	}

	fn parse_struct_decl(self) -> Decl {
		use Rule::*;

		let span = self.as_span();
		let decl = &mut StructDeclBuilder::default();

		Decl::Struct(
			fold_children!(self, decl, pair {
				attribute_list => decl.attributes(pair.parse_attribute_list()),
				STRUCT => decl.storage(Token::keyword(pair)),
				IDENT => decl.name(Token::ident(pair)),
				struct_body_decl => decl.body(pair.parse_struct_body()),
				_ => decl,
			})
			.range(span.into_range())
			.build()
			.unwrap(),
		)
	}

	fn parse_struct_body(self) -> Vec<StructField> {
		use Rule::*;

		self.into_inner()
			.filter_map(|pair| {
				let span = pair.as_span();

				match pair.as_rule() {
					struct_member => {
						let field = &mut StructFieldBuilder::default();
						Some(
							fold_children!(pair, field, pair {
								attribute_list => field.attributes(pair.parse_attribute_list()),
								variable_ident_decl => fold_children!(pair, field, pair {
									IDENT => field.name(Token::ident(pair)),
									attribute_list => field.attributes(pair.parse_attribute_list()),
									type_decl => field.type_decl(Box::new(pair.parse_type_decl())),
									_ => field,
								}),
								_ => field,
							})
							.range(span.into_range())
							.build()
							.unwrap(),
						)
					}
					_ => None,
				}
			})
			.collect()
	}

	fn parse_func_decl(self) -> Decl {
		use Rule::*;

		let span = self.as_span();
		let func = &mut FunctionDeclBuilder::default();

		Decl::Function(
			fold_children!(self, func, pair {
				attribute_list => func.attributes(pair.parse_attribute_list()),
				func_header => {
					let sig = &mut FunctionSignatureBuilder::default();
					let span = pair.as_span();

					for pair in pair.into_inner() {
						match pair.as_rule() {
							FN => {
								func.storage(Token::keyword(pair));
							}
							IDENT => {
								func.name(Token::ident(pair));
							}
							param_list => {
								sig.params(pair.parse_param_list());
							}
							func_return_type => {
								let return_type = pair
									.into_inner()
									.find(|pair| pair.as_rule() == type_decl)
									.unwrap()
									.parse_type_decl();
								sig.return_type(Box::new(return_type));
							}
							_ => {}
						}
					}

					let signature = sig.range(span.into_range()).build().unwrap();

					func.signature(signature)
				},
				compound_stmt => func.body(pair.parse_compound_stmt()),
				_ => func,
			})
			.range(span.into_range())
			.build()
			.unwrap(),
		)
	}

	fn parse_param_list(self) -> Vec<FunctionParam> {
		self.into_inner()
			.filter_map(|pair| match pair.as_rule() {
				Rule::param => {
					let span = pair.as_span();
					let param = &mut FunctionParamBuilder::default();

					Some(
						fold_children!(pair, param, pair {
							Rule::attribute_list => param.attributes(pair.parse_attribute_list()),
							Rule::variable_ident_decl => fold_children!(pair, param, pair {
								Rule::IDENT => param.name(Token::ident(pair)),
								Rule::type_decl => param.type_decl(Box::new(pair.parse_type_decl())),
								_ => param,
							}),
							_ => param,
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

	fn parse_attribute_list(self) -> Vec<Attribute> {
		use Rule::*;

		self.into_inner()
			.filter_map(|pair| match pair.as_rule() {
				attribute => {
					let span = pair.as_span();
					let attr = &mut AttributeBuilder::default();

					Some(
						fold_children!(pair, attr, pair {
							IDENT => attr.name(Token::ident(pair)),
							literal_or_ident => {
								let inner = pair.into_inner().last().unwrap();

								attr.value(match inner.as_rule() {
									FLOAT_LITERAL
									| INT_LITERAL
									| UINT_LITERAL => Token::literal(inner),
									IDENT => Token::ident(inner),
									_ => unreachable!(),
								})
							},
							_ => attr,
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
		use Rule::*;

		let span = self.as_span();
		let mut inner = self.into_inner();

		if inner.clone().any(|pair| pair.as_rule() == type_decl) {
			let mut args = vec![];
			let ctor = &mut TypeCtorExprBuilder::default();

			let type_ctor = inner
				.fold(ctor, |ctor, pair| match pair.as_rule() {
					type_decl => ctor.ty(Box::new(pair.parse_type_decl())),
					const_expr => {
						args.push(pair.parse_const_expr());
						ctor
					}
					_ => ctor,
				})
				.args(args)
				.range(span.into_range())
				.build()
				.unwrap();

			PrimaryExpr::TypeCtor(type_ctor)
		} else {
			let literal = inner
				.find(|pair| pair.as_rule() == const_literal)
				.unwrap()
				.into_inner()
				.last()
				.unwrap();

			PrimaryExpr::Literal(Token::literal(literal))
		}
	}

	fn parse_compound_stmt(self) -> Vec<Stmt> {
		self.into_inner()
			.filter_map(|pair| {
				if pair.as_rule() == Rule::statement {
					Some(pair.parse_statement())
				} else {
					None
				}
			})
			.collect()
	}

	fn parse_statement(self) -> Stmt {
		use Rule::*;

		let inner = self
			.into_inner()
			.find(|pair| pair.as_rule() != SEMICOLON)
			.unwrap();

		match inner.as_rule() {
			return_stmt => Stmt::Return(inner.parse_return_stmt()),
			if_stmt => Stmt::If(Box::new(inner.parse_if_stmt())),
			switch_stmt => todo!(),
			loop_stmt => todo!(),
			for_stmt => todo!(),
			func_call_stmt => todo!(),
			variable_stmt => todo!(),
			BREAK => todo!(),
			CONTINUE => todo!(),
			DISCARD => todo!(),
			assignment_stmt => todo!(),
			_ => unreachable!(),
		}
	}

	fn parse_return_stmt(self) -> ReturnStmt {
		use Rule::*;

		let span = self.as_span();
		let stmt = &mut ReturnStmtBuilder::default();

		fold_children!(self, stmt, pair {
			RETURN => stmt.keyword(Token::keyword(pair)),
			expression => stmt.expr(Box::new(pair.parse_expression())),
			_ => unreachable!(),
		})
		.range(span.into_range())
		.build()
		.unwrap()
	}

	fn parse_if_stmt(self) -> IfStmt {
		use Rule::*;

		let span = self.as_span();
		let stmt = &mut IfStmtBuilder::default();

		fold_children!(self, stmt, pair {
			IF => stmt.keyword(Token::keyword(pair)),
			paren_expr => {
				let expr = pair
					.into_inner()
					.find(|pair| pair.as_rule() == expression)
					.unwrap()
					.parse_expression();

				stmt.condition(Box::new(expr))
			},
			compound_stmt => stmt.body(pair.parse_compound_stmt()),
			elseif_stmt => stmt.elseif(pair.parse_elseif_stmt()),
			else_stmt => stmt.else_stmt(pair.parse_else_stmt()),
			_ => unreachable!(),
		})
		.range(span.into_range())
		.build()
		.unwrap()
	}

	fn parse_elseif_stmt(self) -> ElseifStmt {
		use Rule::*;

		let span = self.as_span();
		let stmt = &mut ElseifStmtBuilder::default();

		fold_children!(self, stmt, pair {
			ELSE_IF => stmt.keyword(Token::keyword(pair)),
			compound_stmt => stmt.body(pair.parse_compound_stmt()),
			elseif_stmt => panic!("Please un-nest the elseif stmts, it makes me sad"),
			_ => unreachable!(),
		})
		.range(span.into_range())
		.build()
		.unwrap()
	}

	fn parse_else_stmt(self) -> ElseStmt {
		use Rule::*;

		let span = self.as_span();
		let stmt = &mut ElseStmtBuilder::default();

		fold_children!(self, stmt, pair {
			ELSE => stmt.keyword(Token::keyword(pair)),
			compound_stmt => stmt.body(pair.parse_compound_stmt()),
			_ => unreachable!(),
		})
		.range(span.into_range())
		.build()
		.unwrap()
	}

	fn parse_expression(self) -> Expr {
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
