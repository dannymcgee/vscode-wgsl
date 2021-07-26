#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate derive_builder;

use lsp_types::{Position, Range};
use pest::{
	error::{Error, ErrorVariant},
	iterators::{Pair, Pairs},
	Parser as PestParser, Span,
};
use std::result;

pub mod ast;
mod error;
mod find_type_decl;
mod flat_tokens;
mod range_utils;
mod rename;
mod scopes;
#[macro_use]
mod macros;

pub use find_type_decl::*;
pub use flat_tokens::*;
pub use pest;
pub use range_utils::*;
pub use rename::*;
pub use scopes::*;

use ast::*;

#[derive(Parser)]
#[grammar = "wgsl.pest"]
pub(crate) struct Parser;

pub type Result<T> = result::Result<T, Error<Rule>>;

pub fn parse(input: &str) -> Result<Pairs<Rule>> {
	Parser::parse(Rule::program, input)
}

pub fn parse_ast(input: &str) -> Result<Vec<Decl>> {
	let program = parse(input)?;
	program.parse()
}

pub fn parse_stmt(input: &str) -> Result<Stmt> {
	let mut stmt = Parser::parse(Rule::statement, input)?;
	Ok(stmt.next().unwrap().parse_statement())
}

pub fn parse_expr(input: &str) -> Result<Expr> {
	let mut expr = Parser::parse(Rule::expression, input)?;
	Ok(expr.next().unwrap().parse_expression())
}

trait AstParser {
	fn parse(self) -> Result<Vec<Decl>>;
}

trait AstNodeParser<'a> {
	fn parse(self) -> Option<Decl>;
	fn parse_directive(self) -> Decl;
	fn parse_enable_directive(self) -> Decl;
	fn parse_plus_import_directive(self) -> Decl;
	fn parse_global_var_decl(self) -> Decl;
	fn parse_variable_qualifier(self) -> Vec<Token>;
	fn parse_global_const_decl(self) -> Decl;
	fn parse_type_alias_decl(self) -> Decl;
	fn parse_struct_decl(self) -> Decl;
	fn parse_struct_body(self) -> Vec<StructField>;
	fn parse_func_decl(self) -> Decl;
	fn parse_param_list(self) -> Vec<FunctionParam>;
	fn parse_attribute_list(self) -> Vec<Attribute>;
	fn parse_variable_ident_decl<F>(self, token_ctor: F) -> (Token, TypeDecl)
	where F: Fn(Pair<'a, Rule>) -> Token;
	fn parse_type_decl(self) -> TypeDecl;
	fn parse_const_expr(self) -> PrimaryExpr;
	fn parse_compound_stmt(self) -> Vec<Stmt>;
	fn parse_statement(self) -> Stmt;
	fn parse_return_stmt(self) -> ReturnStmt;
	fn parse_if_stmt(self) -> IfStmt;
	fn parse_elseif_stmt(self) -> ElseifStmt;
	fn parse_else_stmt(self) -> ElseStmt;
	fn parse_switch_stmt(self) -> SwitchStmt;
	fn parse_switch_body(self) -> Vec<CaseStmt>;
	fn parse_loop_stmt(self) -> LoopStmt;
	fn parse_continuing_stmt(self) -> ContinuingStmt;
	fn parse_for_stmt(self) -> ForStmt;
	fn parse_for_header(self) -> ForHeader;
	fn parse_variable_stmt(self) -> VarDecl;
	fn parse_assignment_stmt(self) -> AssignmentStmt;
	fn parse_expression(self) -> Expr;
	fn parse_singular_expr(self) -> SingularExpr;
	fn parse_primary_expr(self) -> PrimaryExpr;
	fn parse_postfix_expr(self) -> PostfixExpr;
	fn parse_type_ctor(self) -> TypeCtorExpr;
	fn parse_bitcast_expr(self) -> BitcastExpr;
	fn parse_function_call_expr(self) -> FunctionCallExpr;
}

impl<'a> AstParser for Pairs<'a, Rule> {
	fn parse(mut self) -> Result<Vec<Decl>> {
		let program = self
			.find(|pair| pair.as_rule() == Rule::program)
			.ok_or_else(|| {
				Error::new_from_span(
					ErrorVariant::CustomError {
						message: "Unable to find parser entry rule".into(),
					},
					Span::new("", 0, 0).unwrap(),
				)
			})?;

		Ok(program
			.into_inner()
			.filter_map(|pair| pair.parse())
			.collect())
	}
}

impl<'a> AstNodeParser<'a> for Pair<'a, Rule> {
	fn parse(self) -> Option<Decl> {
		use Rule::*;

		match self.as_rule() {
			directive => Some(self.parse_directive()),
			type_alias => Some(self.parse_type_alias_decl()),
			global_variable_decl => Some(self.parse_global_var_decl()),
			global_constant_decl => Some(self.parse_global_const_decl()),
			struct_decl => Some(self.parse_struct_decl()),
			func_decl => Some(self.parse_func_decl()),
			_ => None,
		}
	}

	fn parse_directive(self) -> Decl {
		use Rule::*;

		let pair = self.into_inner().next().unwrap();

		match pair.as_rule() {
			enable_directive => pair.parse_enable_directive(),
			plus_import_directive => pair.parse_plus_import_directive(),
			_ => unreachable!(),
		}
	}

	fn parse_enable_directive(self) -> Decl {
		use Rule::*;

		let span = self.as_span();
		let decl = &mut ExtensionDeclBuilder::default();

		let decl = fold_children!(self, decl, pair {
			ENABLE => decl.keyword(Token::keyword(pair)),
			IDENT => decl.name(Token::module(pair)),
			_ => decl,
		})
		.range(span.into_range())
		.build()
		.unwrap();

		Decl::Extension(decl)
	}

	fn parse_plus_import_directive(self) -> Decl {
		use Rule::*;

		let span = self.as_span();
		let decl = &mut ModuleDeclBuilder::default();

		let decl = fold_children!(self, decl, pair {
			IMPORT => decl.import_keyword(Token::keyword(pair)),
			IDENT => decl.name(Token::module(pair)),
			FROM => decl.from_keyword(Token::keyword(pair)),
			PATH_LITERAL => decl.path(Token::literal(pair)),
			_ => decl,
		})
		.range(span.into_range())
		.build()
		.unwrap();

		Decl::Module(decl)
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
					decl.storage_qualifiers(pair.parse_variable_qualifier())
				},
				variable_ident_decl => {
					let (name, ty) = pair.parse_variable_ident_decl(Token::ident);
					decl.name(name).type_decl(Box::new(ty))
				},
				_ => decl,
			}),
			global_const_initializer => fold_children!(pair, decl, pair {
				const_expr => {
					let span = pair.as_span();
					decl.assignment(Box::new(Expr::Singular(SingularExpr::new(
						pair.parse_const_expr(),
						span.into_range(),
					))))
				},
				_ => decl,
			}),
			_ => decl,
		})
		.range(span.into_range())
		.build()
		.unwrap();

		Decl::Var(decl)
	}

	fn parse_variable_qualifier(self) -> Vec<Token> {
		use Rule::*;

		self.into_inner()
			.filter_map(|pair| match pair.as_rule() {
				storage_class => Some(Token::keyword(pair)),
				access_mode => Some(Token::keyword(pair)),
				_ => None,
			})
			.collect::<Vec<_>>()
	}

	fn parse_global_const_decl(self) -> Decl {
		use Rule::*;

		let span = self.as_span();
		let decl = &mut VarDeclBuilder::default();

		let decl = fold_children!(self, decl, pair {
			attribute_list => decl.attributes(pair.parse_attribute_list()),
			LET => decl.storage(Token::keyword(pair)),
			variable_ident_decl => {
				let (name, ty) = pair.parse_variable_ident_decl(Token::ident);
				decl.name(name).type_decl(Box::new(ty))
			},
			global_const_initializer => fold_children!(pair, decl, pair {
				const_expr => {
					let span = pair.as_span();
					decl.assignment(Box::new(Expr::Singular(SingularExpr::new(
						pair.parse_const_expr(),
						span.into_range(),
					))))
				},
				_ => decl,
			}),
			_ => decl,
		})
		.range(span.into_range())
		.build()
		.unwrap();

		Decl::Const(decl)
	}

	fn parse_type_alias_decl(self) -> Decl {
		use Rule::*;

		let span = self.as_span();
		let decl = &mut TypeAliasDeclBuilder::default();

		let decl = fold_children!(self, decl, pair {
			TYPE => decl.storage(Token::keyword(pair)),
			IDENT => decl.name(Token::typename(pair)),
			type_decl => decl.value(pair.parse_type_decl()),
			_ => decl,
		})
		.range(span.into_range())
		.build()
		.unwrap();

		Decl::TypeAlias(decl)
	}

	fn parse_struct_decl(self) -> Decl {
		use Rule::*;

		let span = self.as_span();
		let decl = &mut StructDeclBuilder::default();

		let decl = fold_children!(self, decl, pair {
			attribute_list => decl.attributes(pair.parse_attribute_list()),
			STRUCT => decl.storage(Token::keyword(pair)),
			plus_export_modifier => {
				let mut inner = pair.into_inner();
				let _ = inner.next().unwrap(); // `<`
				decl.storage_modifier(Token::keyword(inner.next().unwrap()))
			},
			IDENT => decl.name(Token::ident(pair)),
			struct_body_decl => decl.body(pair.parse_struct_body()),
			_ => decl,
		})
		.range(span.into_range())
		.build()
		.unwrap();

		Decl::Struct(decl)
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
								variable_ident_decl => {
									let (name, ty) = pair.parse_variable_ident_decl(Token::field);
									field.name(name).type_decl(Box::new(ty))
								},
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

		let decl = fold_children!(self, func, pair {
			attribute_list => func.attributes(pair.parse_attribute_list()),
			func_header => {
				let sig = &mut FunctionSignatureBuilder::default();
				let span = pair.as_span();

				for pair in pair.into_inner() {
					match pair.as_rule() {
						FN => {
							func.storage(Token::keyword(pair));
						}
						plus_export_modifier => {
							let mut inner = pair.into_inner();
							let _ = inner.next().unwrap(); // `<`
							func.storage_modifier(Token::keyword(inner.next().unwrap()));
						}
						IDENT => {
							func.name(Token::function(pair));
						}
						param_list => {
							sig.params(pair.parse_param_list());
						}
						func_return_type => {
							let mut range_start = None;
							let mut attributes = None;
							let mut ty = None;

							for pair in pair.into_inner() {
								match pair.as_rule() {
									attribute_list => {
										range_start = Some(pair.as_span().into_range().start);
										attributes = Some(pair.parse_attribute_list());
									}
									type_decl => {
										ty = Some(pair.parse_type_decl());
									}
									_ => {}
								}
							}

							let mut ty = ty.unwrap();
							ty.attributes = attributes;

							if let Some(start) = range_start {
								ty.range.start = start;
							}

							sig.return_type(Box::new(ty));
						}
						_ => {}
					}
				}

				let signature = sig.range(span.into_range()).build().unwrap();

				func.signature(signature)
			},
			compound_stmt => {
				let range = pair.as_span().into_range();
				let stmts = pair.parse_compound_stmt();

				func.body((range, stmts))
			},
			_ => func,
		})
		.range(span.into_range())
		.build()
		.unwrap();

		Decl::Function(decl)
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
							Rule::variable_ident_decl => {
								let (name, ty) = pair.parse_variable_ident_decl(Token::ident);
								param.name(name).type_decl(Box::new(ty))
							},
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
							IDENT => attr.name(Token::attr(pair)),
							literal_or_ident => {
								let inner = pair.into_inner().last().unwrap();

								attr.value(match inner.as_rule() {
									FLOAT_LITERAL
									| INT_LITERAL
									| UINT_LITERAL => Token::literal(inner),
									IDENT => Token::attr(inner),
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

	fn parse_variable_ident_decl<F>(self, token_ctor: F) -> (Token, TypeDecl)
	where F: Fn(Pair<'a, Rule>) -> Token {
		use Rule::*;

		let mut ident_token = None;
		let mut ty_range_start = None;
		let mut attributes = None;
		let mut ty = None;

		for pair in self.into_inner() {
			match pair.as_rule() {
				IDENT => {
					ident_token = Some(token_ctor(pair));
				}
				attribute_list => {
					ty_range_start = Some(pair.as_span().into_range().start);
					attributes = Some(pair.parse_attribute_list());
				}
				type_decl => {
					ty = Some(pair.parse_type_decl());
				}
				_ => {}
			}
		}

		let mut ty = ty.unwrap();
		ty.attributes = attributes;

		if let Some(start) = ty_range_start {
			ty.range.start = start;
		}

		(ident_token.unwrap(), ty)
	}

	fn parse_type_decl(self) -> TypeDecl {
		use Rule::*;

		let span = self.as_span();

		self.into_inner()
			.fold(&mut TypeDeclBuilder::default(), |ty, pair| {
				match pair.as_rule() {
					attribute_list => ty.attributes(pair.parse_attribute_list()),
					BOOL | FLOAT32 | INT32 | UINT32 => ty.name(Token::keyword(pair)),
					VEC | PTR | ARRAY | MATRIX | IDENT => ty.name(Token::typename(pair)),
					texture_sampler_type => fold_children!(pair, ty, pair {
						SAMPLED_TEXTURE_TYPE
						| STORAGE_TEXTURE_TYPE
						| MULTISAMPLED_TEXTURE_TYPE
						| DEPTH_TEXTURE_TYPE
						| SAMPLER_TYPE => ty.name(Token::typename(pair)),
						texel_format | type_decl => {
							ty.component_type(Box::new(pair.parse_type_decl()))
						},
						access_mode => ty.access_mode(Token::keyword(pair)),
						_ => ty,
					}),
					plus_namespaced_type => fold_children!(pair, ty, pair {
						namespace => ty.namespace(Token::module(pair)),
						IDENT => ty.name(Token::ident(pair)),
						_ => ty,
					}),
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

		let mut inner = self.clone().into_inner();

		if inner.clone().any(|pair| pair.as_rule() == type_decl) {
			PrimaryExpr::TypeCtor(self.parse_type_ctor())
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
			switch_stmt => Stmt::Switch(inner.parse_switch_stmt()),
			loop_stmt => Stmt::Loop(inner.parse_loop_stmt()),
			continuing_stmt => Stmt::Continuing(inner.parse_continuing_stmt()),
			for_stmt => Stmt::For(inner.parse_for_stmt()),
			func_call_stmt => Stmt::FunctionCall(inner.parse_function_call_expr()),
			variable_stmt => Stmt::Variable(inner.parse_variable_stmt()),
			BREAK => Stmt::Break(Token::keyword(inner)),
			CONTINUE => Stmt::Continue(Token::keyword(inner)),
			DISCARD => Stmt::Discard(Token::keyword(inner)),
			FALLTHROUGH => Stmt::Fallthrough(Token::keyword(inner)),
			assignment_stmt => Stmt::Assignment(inner.parse_assignment_stmt()),
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
			paren_expr => {
				let expr = pair
					.into_inner()
					.find(|pair| pair.as_rule() == expression)
					.unwrap()
					.parse_expression();

				stmt.condition(Box::new(expr))
			},
			compound_stmt => stmt.body(pair.parse_compound_stmt()),
			elseif_stmt => stmt.elseif(Box::new(pair.parse_elseif_stmt())),
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

	fn parse_switch_stmt(self) -> SwitchStmt {
		use Rule::*;

		let span = self.as_span();
		let stmt = &mut SwitchStmtBuilder::default();

		fold_children!(self, stmt, pair {
			SWITCH => stmt.keyword(Token::keyword(pair)),
			paren_expr => {
				let inner = pair
					.into_inner()
					.find(|pair| pair.as_rule() == expression)
					.unwrap();

				stmt.subject(Box::new(inner.parse_expression()))
			},
			switch_body => stmt.body(pair.parse_switch_body()),
			_ => stmt,
		})
		.range(span.into_range())
		.build()
		.unwrap()
	}

	fn parse_switch_body(self) -> Vec<CaseStmt> {
		use Rule::*;

		self.into_inner()
			.map(|pair| {
				let span = pair.as_span();
				let stmt = &mut CaseStmtBuilder::default();
				let mut body = vec![];

				fold_children!(pair, stmt, pair {
					CASE | DEFAULT => stmt.keyword(Token::keyword(pair)),
					case_selectors => {
						let selectors = pair
							.into_inner()
							.filter_map(|pair| match pair.as_rule() {
								const_literal => Some(Token::literal(pair)),
								_ => None,
							})
							.collect::<Vec<_>>();

						stmt.selectors(selectors)
					},
					case_body => {
						for pair in pair.into_inner() {
							match pair.as_rule() {
								FALLTHROUGH => body.push(Stmt::Fallthrough(Token::keyword(pair))),
								statement => body.push(pair.parse_statement()),
								_ => {}
							}
						}

						stmt
					},
					_ => stmt,
				})
				.body(body)
				.range(span.into_range())
				.build()
				.unwrap()
			})
			.collect()
	}

	fn parse_loop_stmt(self) -> LoopStmt {
		use Rule::*;

		let span = self.as_span();
		let stmt = &mut LoopStmtBuilder::default();
		let mut body = vec![];

		fold_children!(self, stmt, pair {
			LOOP => stmt.keyword(Token::keyword(pair)),
			statement => {
				body.push(pair.parse_statement());
				stmt
			},
			continuing_stmt => stmt.continuing(pair.parse_continuing_stmt()),
			_ => stmt,
		})
		.body(body)
		.range(span.into_range())
		.build()
		.unwrap()
	}

	fn parse_continuing_stmt(self) -> ContinuingStmt {
		use Rule::*;

		let span = self.as_span();
		let stmt = &mut ContinuingStmtBuilder::default();

		fold_children!(self, stmt, pair {
			CONTINUING => stmt.keyword(Token::keyword(pair)),
			compound_stmt => stmt.body(pair.parse_compound_stmt()),
			_ => unreachable!(),
		})
		.range(span.into_range())
		.build()
		.unwrap()
	}

	fn parse_for_stmt(self) -> ForStmt {
		use Rule::*;

		let span = self.as_span();
		let stmt = &mut ForStmtBuilder::default();

		fold_children!(self, stmt, pair {
			FOR => stmt.keyword(Token::keyword(pair)),
			for_header => stmt.header(pair.parse_for_header()),
			compound_stmt => stmt.body(pair.parse_compound_stmt()),
			_ => stmt,
		})
		.range(span.into_range())
		.build()
		.unwrap()
	}

	fn parse_for_header(self) -> ForHeader {
		use Rule::*;

		let span = self.as_span();
		let header = &mut ForHeaderBuilder::default();
		let mut processed = 0;

		fold_children!(self, header, pair {
			SEMICOLON => {
				processed += 1;
				header
			},
			variable_stmt => header.init(Box::new(Stmt::Variable(pair.parse_variable_stmt()))),
			assignment_stmt => header.init(Box::new(Stmt::Assignment(pair.parse_assignment_stmt()))),
			func_call_stmt => header.init(Box::new(Stmt::FunctionCall(pair.parse_function_call_expr()))),
			expression => {
				let expr = Box::new(pair.parse_expression());
				if processed < 2 {
					header.condition(expr)
				} else {
					header.iterator(expr)
				}
			},
			_ => unreachable!(),
		})
		.range(span.into_range())
		.build()
		.unwrap()
	}

	fn parse_variable_stmt(self) -> VarDecl {
		use Rule::*;

		let span = self.as_span();
		let decl = &mut VarDeclBuilder::default();

		fold_children!(self, decl, pair {
			variable_decl => fold_children!(pair, decl, pair {
				VAR => decl.storage(Token::keyword(pair)),
				variable_qualifier => {
					decl.storage_qualifiers(pair.parse_variable_qualifier())
				},
				variable_ident_decl => {
					let (name, ty) = pair.parse_variable_ident_decl(Token::ident);
					decl.name(name).type_decl(Box::new(ty))
				},
				_ => unreachable!(),
			}),
			expression => decl.assignment(Box::new(pair.parse_expression())),
			LET => decl.storage(Token::keyword(pair)),
			variable_ident_decl => {
				let (name, ty) = pair.parse_variable_ident_decl(Token::ident);
				decl.name(name).type_decl(Box::new(ty))
			},
			IDENT => decl.name(Token::ident(pair)),
			_ => decl,
		})
		.range(span.into_range())
		.build()
		.unwrap()
	}

	fn parse_assignment_stmt(self) -> AssignmentStmt {
		use Rule::*;

		let span = self.as_span();
		let stmt = &mut AssignmentStmtBuilder::default();

		fold_children!(self, stmt, pair {
			singular_expr => stmt.lhs(Box::new(pair.parse_singular_expr())),
			EQUAL => stmt.op(Token::op(pair)),
			expression => stmt.rhs(Box::new(pair.parse_expression())),
			_ => unreachable!(),
		})
		.range(span.into_range())
		.build()
		.unwrap()
	}

	fn parse_expression(self) -> Expr {
		use Rule::*;

		let span = self.as_span();
		let mut lhs = None;
		let mut binop = None;
		let mut rhs = None;

		for pair in self.into_inner() {
			match pair.as_rule() {
				singular_expr => {
					lhs = Some(pair.parse_singular_expr());
				}
				expression => {
					rhs = Some(pair.parse_expression());
				}
				BINOP => {
					binop = Some(Token::op(pair));
				}
				_ => unreachable!(),
			}
		}

		if let Some(op) = binop {
			Expr::Binary(BinaryExpr {
				lhs: Box::new(lhs.unwrap()),
				op,
				rhs: Box::new(rhs.unwrap()),
				range: span.into_range(),
			})
		} else {
			Expr::Singular(lhs.unwrap())
		}
	}

	fn parse_singular_expr(self) -> SingularExpr {
		use Rule::*;

		let span = self.as_span();
		let expr = &mut SingularExprBuilder::default();

		self.into_inner()
			.fold(expr, |expr, pair| match pair.as_rule() {
				unary_expr => {
					let mut prefix = vec![];
					for pair in pair.into_inner().flatten() {
						match pair.as_rule() {
							MINUS | BANG | TILDE | STAR | AND => {
								prefix.push(Token::op(pair));
							}
							primary_expr => {
								expr.expr(pair.parse_primary_expr());
							}
							postfix_expr => {
								expr.postfix(pair.parse_postfix_expr());
							}
							_ => {}
						}
					}
					expr.prefix(prefix)
				}
				primary_expr => expr.expr(pair.parse_primary_expr()),
				postfix_expr => expr.postfix(pair.parse_postfix_expr()),
				_ => unreachable!(),
			})
			.range(span.into_range())
			.build()
			.unwrap()
	}

	fn parse_primary_expr(self) -> PrimaryExpr {
		use Rule::*;

		let next = self.clone().into_inner().next().unwrap();
		match next.as_rule() {
			builtin_type_decl => PrimaryExpr::TypeCtor(self.parse_type_ctor()),
			const_literal => PrimaryExpr::Literal(Token::literal(next)),
			paren_expr => {
				let inner = next
					.into_inner()
					.find(|pair| pair.as_rule() == expression)
					.unwrap();

				PrimaryExpr::Paren(Box::new(inner.parse_expression()))
			}
			BITCAST => PrimaryExpr::Bitcast(self.parse_bitcast_expr()),
			func_call_expr => PrimaryExpr::FunctionCall(next.parse_function_call_expr()),
			IDENT => PrimaryExpr::Identifier(Token::ident(next)),
			_ => unreachable!(),
		}
	}

	fn parse_postfix_expr(self) -> PostfixExpr {
		use Rule::*;

		let range = self.as_span().into_range();
		let mut inner = None;
		let mut ident = None;
		let mut postfix = None;
		let mut hint = None;

		for pair in self.into_inner() {
			match pair.as_rule() {
				LBRACK | DOT => hint = Some(pair.as_rule()),
				IDENT => ident = Some(Token::field(pair)),
				expression => inner = Some(Box::new(pair.parse_expression())),
				postfix_expr => postfix = Some(Box::new(pair.parse_postfix_expr())),
				_ => {}
			}
		}

		if hint.unwrap() == LBRACK {
			PostfixExpr::Bracket {
				expr: inner.unwrap(),
				postfix,
				range,
			}
		} else {
			PostfixExpr::Dot {
				ident: ident.unwrap(),
				postfix,
				range,
			}
		}
	}

	fn parse_type_ctor(self) -> TypeCtorExpr {
		use Rule::*;

		let span = self.as_span();
		let mut args = vec![];
		let ctor = &mut TypeCtorExprBuilder::default();

		fold_children!(self, ctor, pair {
			builtin_type_decl | type_decl => ctor.ty(Box::new(pair.parse_type_decl())),
			const_expr => {
				let range = pair.as_span().into_range();
				let expr = Expr::Singular(SingularExpr::new(
					pair.parse_const_expr(),
					range,
				));

				args.push(expr);
				ctor
			},
			argument_expr_list => fold_children!(pair, ctor, pair {
				expression => {
					args.push(pair.parse_expression());
					ctor
				},
				_ => ctor,
			}),
			expression => {
				args.push(pair.parse_expression());
				ctor
			},
			_ => ctor,
		})
		.args(args)
		.range(span.into_range())
		.build()
		.unwrap()
	}

	fn parse_bitcast_expr(self) -> BitcastExpr {
		use Rule::*;

		let span = self.as_span();
		let expr = &mut BitcastExprBuilder::default();

		fold_children!(self, expr, pair {
			BITCAST => expr.keyword(Token::keyword(pair)),
			type_decl => expr.ty(Box::new(pair.parse_type_decl())),
			paren_expr => {
				let inner = pair
					.into_inner()
					.find(|pair| pair.as_rule() == expression)
					.unwrap()
					.parse_expression();

				expr.expr(Box::new(inner))
			},
			_ => expr,
		})
		.range(span.into_range())
		.build()
		.unwrap()
	}

	fn parse_function_call_expr(self) -> FunctionCallExpr {
		use Rule::*;

		let span = self.as_span();
		let expr = &mut FunctionCallExprBuilder::default();

		fold_children!(self, expr, pair {
			namespace => expr.namespace(Token::module(pair)),
			IDENT => expr.ident(Token::function(pair)),
			argument_expr_list => {
				let args = pair
					.into_inner()
					.filter_map(|pair| if pair.as_rule() == expression {
						Some(pair.parse_expression())
					} else {
						None
					})
					.collect();

				expr.args(args)
			},
			_ => expr,
		})
		.range(span.into_range())
		.build()
		.unwrap()
	}
}

impl<'a> Token {
	fn ident(pair: Pair<'a, Rule>) -> Token {
		let text = pair.as_str().to_string();
		let range = pair.as_span().into_range();

		Token::Ident(text, range)
	}

	fn attr(pair: Pair<'a, Rule>) -> Token {
		let text = pair.as_str().to_string();
		let range = pair.as_span().into_range();

		Token::Attr(text, range)
	}

	fn field(pair: Pair<'a, Rule>) -> Token {
		let text = pair.as_str().to_string();
		let range = pair.as_span().into_range();

		Token::Field(text, range)
	}

	fn typename(pair: Pair<'a, Rule>) -> Token {
		let text = pair.as_str().to_string();
		let range = pair.as_span().into_range();

		Token::Type(text, range)
	}

	fn function(pair: Pair<'a, Rule>) -> Token {
		let text = pair.as_str().to_string();
		let range = pair.as_span().into_range();

		Token::Function(text, range)
	}

	fn keyword(pair: Pair<'a, Rule>) -> Token {
		let text = pair.as_str().to_string();
		let range = pair.as_span().into_range();

		Token::Keyword(text, range)
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

	fn module(pair: Pair<'a, Rule>) -> Token {
		let text = pair.as_str().to_string();
		let range = pair.as_span().into_range();

		Token::Module(text, range)
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
