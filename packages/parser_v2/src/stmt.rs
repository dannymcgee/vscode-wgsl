#![allow(unused_variables)]

use gramatika::{Parse, ParseStreamer, Spanned, SpannedError};

use crate::{decl::VarDecl, expr::Expr, ParseStream, Token, *};

#[allow(clippy::large_enum_variant)] // TODO
#[derive(DebugLisp)]
pub enum Stmt<'a> {
	Block(BlockStmt<'a>),
	Return(ReturnStmt<'a>),
	If(IfStmt<'a>),
	Switch(SwitchStmt<'a>),
	Loop(LoopStmt<'a>),
	Continuing(ContinuingStmt<'a>),
	For(ForStmt<'a>),
	Var(VarDecl<'a>),
	Break(KeywordStmt<'a>),
	Continue(KeywordStmt<'a>),
	Discard(KeywordStmt<'a>),
	Fallthrough(KeywordStmt<'a>),
	Expr(ExprStmt<'a>),
	Empty(Token<'a>),
}

#[derive(DebugLisp)]
pub struct BlockStmt<'a> {
	pub brace_open: Token<'a>,
	pub stmts: Vec<Stmt<'a>>,
	pub brace_close: Token<'a>,
}

#[derive(DebugLisp)]
pub struct ReturnStmt<'a> {
	pub keyword: Token<'a>,
	pub value: Option<Expr<'a>>,
	pub semicolon: Token<'a>,
}

#[derive(DebugLisp)]
pub struct IfStmt<'a> {
	pub keyword: Token<'a>,
	pub condition: Expr<'a>,
	pub then_branch: BlockStmt<'a>,
	pub elseif_branch: Option<Box<IfStmt<'a>>>,
	pub else_branch: Option<ElseStmt<'a>>,
}

#[derive(DebugLisp)]
pub struct ElseStmt<'a> {
	pub keyword: Token<'a>,
	pub body: BlockStmt<'a>,
}

#[derive(DebugLisp)]
pub struct SwitchStmt<'a> {
	pub keyword: Token<'a>,
	pub subject: Expr<'a>,
	pub body: SwitchBody<'a>,
}

#[derive(DebugLisp)]
pub struct SwitchBody<'a> {
	pub brace_open: Token<'a>,
	pub cases: Vec<CaseStmt<'a>>,
	pub default: Option<DefaultStmt<'a>>,
	pub brace_close: Token<'a>,
}

#[derive(DebugLisp)]
pub struct CaseStmt<'a> {
	pub keyword: Token<'a>,
	pub selectors: Vec<Token<'a>>,
	pub colon: Token<'a>,
	pub body: BlockStmt<'a>,
}

#[derive(DebugLisp)]
pub struct DefaultStmt<'a> {
	pub keyword: Token<'a>,
	pub colon: Token<'a>,
	pub body: BlockStmt<'a>,
}

#[derive(DebugLisp)]
pub struct LoopStmt<'a> {
	pub keyword: Token<'a>,
	pub body: BlockStmt<'a>,
}

#[derive(DebugLisp)]
pub struct ContinuingStmt<'a> {
	pub keyword: Token<'a>,
	pub body: BlockStmt<'a>,
}

#[derive(DebugLisp)]
pub struct ForStmt<'a> {
	pub keyword: Token<'a>,
	pub initializer: Option<Box<Stmt<'a>>>,
	pub condition: Option<Box<Stmt<'a>>>,
	pub increment: Option<Expr<'a>>,
	pub body: BlockStmt<'a>,
}

#[derive(DebugLisp)]
pub struct KeywordStmt<'a> {
	pub keyword: Token<'a>,
	pub semicolon: Token<'a>,
}

#[derive(DebugLisp)]
pub struct ExprStmt<'a> {
	pub expr: Expr<'a>,
	pub semicolon: Token<'a>,
}

// -- Parse impl -------------------------------------------------------------------------

impl<'a> Parse<'a> for Stmt<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		use Token::*;

		match input.peek() {
			Some(Keyword("return", _)) => Ok(Stmt::Return(input.parse()?)),
			Some(Keyword("if", _)) => Ok(Stmt::If(input.parse()?)),
			Some(Keyword("switch", _)) => Ok(Stmt::Switch(input.parse()?)),
			Some(Keyword("loop", _)) => Ok(Stmt::Loop(input.parse()?)),
			Some(Keyword("continuing", _)) => Ok(Stmt::Continuing(input.parse()?)),
			Some(Keyword("for", _)) => Ok(Stmt::For(input.parse()?)),
			Some(Keyword("var" | "let", _)) => Ok(Stmt::Var(input.parse()?)),
			Some(Keyword("break", _)) => Ok(Stmt::Break(input.parse()?)),
			Some(Keyword("continue", _)) => Ok(Stmt::Continue(input.parse()?)),
			Some(Keyword("discard", _)) => Ok(Stmt::Discard(input.parse()?)),
			Some(Keyword("fallthrough", _)) => Ok(Stmt::Fallthrough(input.parse()?)),
			Some(Brace("{", _)) => Ok(Stmt::Block(input.parse()?)),
			Some(Punct(";", _)) => Ok(Stmt::Empty(input.next().unwrap())),
			Some(other) => Ok(Stmt::Expr(input.parse()?)),
			None => Err(SpannedError {
				message: "Unexpected end of input".into(),
				source: input.source(),
				span: None,
			}),
		}
	}
}

impl<'a> Parse<'a> for BlockStmt<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		let brace_open = input.consume(brace!["{"])?;

		let mut stmts = vec![];
		while !input.check(brace!["}"]) {
			match input.peek() {
				Some(Token::Comment(_, _)) => input.discard(),
				_ => stmts.push(input.parse::<Stmt>()?),
			}
		}

		let brace_close = input.consume(brace!["}"])?;

		Ok(Self {
			brace_open,
			stmts,
			brace_close,
		})
	}
}

impl<'a> Parse<'a> for ReturnStmt<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		let keyword = input.consume(keyword![return])?;

		if input.check(punct![;]) {
			let semicolon = input.consume(punct![;])?;

			Ok(Self {
				keyword,
				value: None,
				semicolon,
			})
		} else {
			let value = input.parse::<Expr>()?;
			let semicolon = input.consume(punct![;])?;

			Ok(Self {
				keyword,
				value: Some(value),
				semicolon,
			})
		}
	}
}

impl<'a> Parse<'a> for IfStmt<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		let keyword = input.consume_kind(TokenKind::Keyword)?;
		let condition = input.parse::<Expr>()?;
		let then_branch = input.parse::<BlockStmt>()?;

		let elseif_branch = if input.check(keyword![elseif]) {
			Some(Box::new(input.parse::<IfStmt>()?))
		} else {
			None
		};

		let else_branch = if input.check(keyword![else]) {
			Some(input.parse::<ElseStmt>()?)
		} else {
			None
		};

		Ok(Self {
			keyword,
			condition,
			then_branch,
			elseif_branch,
			else_branch,
		})
	}
}

impl<'a> Parse<'a> for ElseStmt<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		let keyword = input.consume(keyword![else])?;
		let body = input.parse::<BlockStmt>()?;

		Ok(Self { keyword, body })
	}
}

impl<'a> Parse<'a> for SwitchStmt<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		let keyword = input.consume(keyword![switch])?;
		let subject = input.parse::<Expr>()?;
		let body = input.parse::<SwitchBody>()?;

		Ok(Self {
			keyword,
			subject,
			body,
		})
	}
}

impl<'a> Parse<'a> for SwitchBody<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		use Token::*;

		let brace_open = input.consume(brace!["{"])?;

		let mut cases = vec![];
		let mut default = None;

		while !input.check(brace!["}"]) {
			match input.peek() {
				Some(Keyword("case", _)) => cases.push(input.parse::<CaseStmt>()?),
				Some(Keyword("default", _)) => default = Some(input.parse::<DefaultStmt>()?),
				Some(other) => {
					return Err(SpannedError {
						message: "Expected `case` or `default`".into(),
						span: Some(other.span()),
						source: input.source(),
					})
				}
				None => {
					return Err(SpannedError {
						message: "Unexpected end of input".into(),
						source: input.source(),
						span: None,
					})
				}
			};
		}

		let brace_close = input.consume(brace!["}"])?;

		Ok(Self {
			brace_open,
			cases,
			default,
			brace_close,
		})
	}
}

impl<'a> Parse<'a> for CaseStmt<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		use Token::*;

		let keyword = input.consume(keyword![case])?;

		let mut selectors = vec![];
		while !input.check(punct![:]) {
			match input.next() {
				Some(
					token @ IntLiteral(_, _)
					| token @ UintLiteral(_, _)
					| token @ FloatLiteral(_, _),
				) => {
					selectors.push(token);
				}
				Some(Punct(",", _)) => {}
				Some(other) => {
					return Err(SpannedError {
						message: "Expected case selector".into(),
						source: input.source(),
						span: Some(other.span()),
					})
				}
				None => {
					return Err(SpannedError {
						message: "Unexpected end of input".into(),
						source: input.source(),
						span: None,
					})
				}
			};
		}

		let colon = input.consume(punct![:])?;
		let body = input.parse::<BlockStmt>()?;

		Ok(Self {
			keyword,
			selectors,
			colon,
			body,
		})
	}
}

impl<'a> Parse<'a> for DefaultStmt<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		let keyword = input.consume(keyword![default])?;
		let colon = input.consume(punct![:])?;
		let body = input.parse::<BlockStmt>()?;

		Ok(Self {
			keyword,
			colon,
			body,
		})
	}
}

impl<'a> Parse<'a> for LoopStmt<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		let keyword = input.consume(keyword![loop])?;
		let body = input.parse::<BlockStmt>()?;

		Ok(Self { keyword, body })
	}
}

impl<'a> Parse<'a> for ContinuingStmt<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		let keyword = input.consume(keyword![continuing])?;
		let body = input.parse::<BlockStmt>()?;

		Ok(Self { keyword, body })
	}
}

impl<'a> Parse<'a> for ForStmt<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		let keyword = input.consume(keyword![for])?;
		input.consume(brace!["("])?;

		let mut initializer = None;
		let mut condition = None;
		let mut increment = None;

		let mut semis = 0u8;

		while !input.check(brace![")"]) {
			match input.peek() {
				Some(token) => match semis {
					0 => {
						initializer = Some(Box::new(input.parse::<Stmt>()?));
						semis += 1;
					}
					1 => {
						condition = Some(Box::new(input.parse::<Stmt>()?));
						semis += 1;
					}
					2 => increment = Some(input.parse::<Expr>()?),
					_ => {
						return Err(SpannedError {
							message: "Expected `)`".into(),
							span: Some(token.span()),
							source: input.source(),
						})
					}
				},
				None => {
					return Err(SpannedError {
						message: "Unexpected end of input".into(),
						source: input.source(),
						span: None,
					})
				}
			};
		}

		input.consume(brace![")"])?;
		let body = input.parse::<BlockStmt>()?;

		Ok(Self {
			keyword,
			initializer,
			condition,
			increment,
			body,
		})
	}
}

impl<'a> Parse<'a> for KeywordStmt<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		let keyword = input.consume_kind(TokenKind::Keyword)?;
		let semicolon = input.consume(punct![;])?;

		Ok(Self { keyword, semicolon })
	}
}

impl<'a> Parse<'a> for ExprStmt<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		let expr = input.parse::<Expr>()?;
		let semicolon = input.consume(punct![;])?;

		Ok(Self { expr, semicolon })
	}
}
