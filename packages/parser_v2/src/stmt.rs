#![allow(unused_variables)]

use std::sync::Arc;

use gramatika::{Parse, ParseStreamer, Spanned, SpannedError, Token as _};

use crate::{decl::VarDecl, expr::Expr, ParseStream, Token, *};

#[allow(clippy::large_enum_variant)] // TODO
#[derive(Clone, DebugLisp)]
pub enum Stmt {
	Block(BlockStmt),
	Return(ReturnStmt),
	If(IfStmt),
	Switch(SwitchStmt),
	Loop(LoopStmt),
	Continuing(ContinuingStmt),
	For(ForStmt),
	Var(VarDecl),
	Break(KeywordStmt),
	Continue(KeywordStmt),
	Discard(KeywordStmt),
	Fallthrough(KeywordStmt),
	Expr(ExprStmt),
	Empty(Token),
}

#[derive(Clone, DebugLisp)]
pub struct BlockStmt {
	pub brace_open: Token,
	pub stmts: Arc<[Stmt]>,
	pub brace_close: Token,
}

#[derive(Clone, DebugLisp)]
pub struct ReturnStmt {
	pub keyword: Token,
	pub value: Option<Expr>,
	pub semicolon: Token,
}

#[derive(Clone, DebugLisp)]
pub struct IfStmt {
	pub keyword: Token,
	pub condition: Expr,
	pub then_branch: BlockStmt,
	pub elseif_branch: Option<Arc<IfStmt>>,
	pub else_branch: Option<ElseStmt>,
}

#[derive(Clone, DebugLisp)]
pub struct ElseStmt {
	pub keyword: Token,
	pub body: BlockStmt,
}

#[derive(Clone, DebugLisp)]
pub struct SwitchStmt {
	pub keyword: Token,
	pub subject: Expr,
	pub body: SwitchBody,
}

#[derive(Clone, DebugLisp)]
pub struct SwitchBody {
	pub brace_open: Token,
	pub cases: Arc<[CaseStmt]>,
	pub default: Option<DefaultStmt>,
	pub brace_close: Token,
}

#[derive(Clone, DebugLisp)]
pub struct CaseStmt {
	pub keyword: Token,
	pub selectors: Arc<[Token]>,
	pub colon: Token,
	pub body: BlockStmt,
}

#[derive(Clone, DebugLisp)]
pub struct DefaultStmt {
	pub keyword: Token,
	pub colon: Token,
	pub body: BlockStmt,
}

#[derive(Clone, DebugLisp)]
pub struct LoopStmt {
	pub keyword: Token,
	pub body: BlockStmt,
}

#[derive(Clone, DebugLisp)]
pub struct ContinuingStmt {
	pub keyword: Token,
	pub body: BlockStmt,
}

#[derive(Clone, DebugLisp)]
pub struct ForStmt {
	pub keyword: Token,
	pub initializer: Option<Arc<Stmt>>,
	pub condition: Option<Arc<Stmt>>,
	pub increment: Option<Expr>,
	pub body: BlockStmt,
}

#[derive(Clone, DebugLisp)]
pub struct KeywordStmt {
	pub keyword: Token,
	pub semicolon: Token,
}

#[derive(Clone, DebugLisp)]
pub struct ExprStmt {
	pub expr: Expr,
	pub semicolon: Token,
}

// -- Parse impl -------------------------------------------------------------------------

impl Parse for Stmt {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
		use TokenKind::*;

		match input.peek() {
			#[rustfmt::skip]
			Some(token) => match token.as_matchable() {
				(Keyword, "return", _)      => Ok(Stmt::Return(input.parse()?)),
				(Keyword, "if", _)          => Ok(Stmt::If(input.parse()?)),
				(Keyword, "switch", _)      => Ok(Stmt::Switch(input.parse()?)),
				(Keyword, "loop", _)        => Ok(Stmt::Loop(input.parse()?)),
				(Keyword, "continuing", _)  => Ok(Stmt::Continuing(input.parse()?)),
				(Keyword, "for", _)         => Ok(Stmt::For(input.parse()?)),
				(Keyword, "var" | "let", _) => Ok(Stmt::Var(input.parse()?)),
				(Keyword, "break", _)       => Ok(Stmt::Break(input.parse()?)),
				(Keyword, "continue", _)    => Ok(Stmt::Continue(input.parse()?)),
				(Keyword, "discard", _)     => Ok(Stmt::Discard(input.parse()?)),
				(Keyword, "fallthrough", _) => Ok(Stmt::Fallthrough(input.parse()?)),
				(Brace, "{", _)             => Ok(Stmt::Block(input.parse()?)),
				(Punct, ";", _)             => Ok(Stmt::Empty(input.next().unwrap())),
				_ => Ok(Stmt::Expr(input.parse()?)),
			},
			None => Err(SpannedError {
				message: "Unexpected end of input".into(),
				source: input.source(),
				span: None,
			}),
		}
	}
}

impl Spanned for Stmt {
	fn span(&self) -> Span {
		use Stmt::*;

		match self {
			Block(inner) => inner.span(),
			Return(inner) => inner.span(),
			If(inner) => inner.span(),
			Switch(inner) => inner.span(),
			Loop(inner) => inner.span(),
			Continuing(inner) => inner.span(),
			For(inner) => inner.span(),
			Var(inner) => inner.span(),
			Break(inner) => inner.span(),
			Continue(inner) => inner.span(),
			Discard(inner) => inner.span(),
			Fallthrough(inner) => inner.span(),
			Expr(inner) => inner.span(),
			Empty(inner) => inner.span(),
		}
	}
}

impl Parse for BlockStmt {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
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
			stmts: stmts.into(),
			brace_close,
		})
	}
}

impl Spanned for BlockStmt {
	fn span(&self) -> Span {
		self.brace_open.span().through(self.brace_close.span())
	}
}

impl Parse for ReturnStmt {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
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

impl Spanned for ReturnStmt {
	fn span(&self) -> Span {
		self.keyword.span().through(self.semicolon.span())
	}
}

impl Parse for IfStmt {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
		let keyword = input.consume_kind(TokenKind::Keyword)?;
		let condition = input.parse::<Expr>()?;
		let then_branch = input.parse::<BlockStmt>()?;

		let elseif_branch = if input.check(keyword![elseif]) {
			Some(Arc::new(input.parse::<IfStmt>()?))
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

impl Spanned for IfStmt {
	fn span(&self) -> Span {
		let end_span = self
			.else_branch
			.as_ref()
			.map(|stmt| stmt.span())
			.or_else(|| self.elseif_branch.as_ref().map(|stmt| stmt.span()))
			.unwrap_or_else(|| self.then_branch.span());

		self.keyword.span().through(end_span)
	}
}

impl Parse for ElseStmt {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
		let keyword = input.consume(keyword![else])?;
		let body = input.parse::<BlockStmt>()?;

		Ok(Self { keyword, body })
	}
}

impl Spanned for ElseStmt {
	fn span(&self) -> Span {
		self.keyword.span().through(self.body.span())
	}
}

impl Parse for SwitchStmt {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
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

impl Spanned for SwitchStmt {
	fn span(&self) -> Span {
		self.keyword.span().through(self.body.span())
	}
}

impl Parse for SwitchBody {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
		use TokenKind::*;

		let brace_open = input.consume(brace!["{"])?;

		let mut cases = vec![];
		let mut default = None;

		while !input.check(brace!["}"]) {
			match input.peek() {
				Some(token) => match token.as_matchable() {
					(Keyword, "case", _) => cases.push(input.parse::<CaseStmt>()?),
					(Keyword, "default", _) => default = Some(input.parse::<DefaultStmt>()?),
					(_, _, span) => {
						return Err(SpannedError {
							message: "Expected `case` or `default`".into(),
							span: Some(span),
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

		let brace_close = input.consume(brace!["}"])?;

		Ok(Self {
			brace_open,
			cases: cases.into(),
			default,
			brace_close,
		})
	}
}

impl Spanned for SwitchBody {
	fn span(&self) -> Span {
		self.brace_open.span().through(self.brace_close.span())
	}
}

impl Parse for CaseStmt {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
		use TokenKind::*;

		let keyword = input.consume(keyword![case])?;

		let mut selectors = vec![];
		while !input.check(punct![:]) {
			match input.next() {
				Some(token) => match token.as_matchable() {
					(IntLiteral | UintLiteral | FloatLiteral, _, _) => {
						selectors.push(token);
					}
					(Punct, ",", _) => {}
					(_, _, span) => {
						return Err(SpannedError {
							message: "Expected case selector".into(),
							source: input.source(),
							span: Some(span),
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

		let colon = input.consume(punct![:])?;
		let body = input.parse::<BlockStmt>()?;

		Ok(Self {
			keyword,
			selectors: selectors.into(),
			colon,
			body,
		})
	}
}

impl Spanned for CaseStmt {
	fn span(&self) -> Span {
		self.keyword.span().through(self.body.span())
	}
}

impl Parse for DefaultStmt {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
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

impl Spanned for DefaultStmt {
	fn span(&self) -> Span {
		self.keyword.span().through(self.body.span())
	}
}

impl Parse for LoopStmt {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
		let keyword = input.consume(keyword![loop])?;
		let body = input.parse::<BlockStmt>()?;

		Ok(Self { keyword, body })
	}
}

impl Spanned for LoopStmt {
	fn span(&self) -> Span {
		self.keyword.span().through(self.body.span())
	}
}

impl Parse for ContinuingStmt {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
		let keyword = input.consume(keyword![continuing])?;
		let body = input.parse::<BlockStmt>()?;

		Ok(Self { keyword, body })
	}
}

impl Spanned for ContinuingStmt {
	fn span(&self) -> Span {
		self.keyword.span().through(self.body.span())
	}
}

impl Parse for ForStmt {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
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
						initializer = Some(Arc::new(input.parse::<Stmt>()?));
						semis += 1;
					}
					1 => {
						condition = Some(Arc::new(input.parse::<Stmt>()?));
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

impl Spanned for ForStmt {
	fn span(&self) -> Span {
		self.keyword.span().through(self.body.span())
	}
}

impl Parse for KeywordStmt {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
		let keyword = input.consume_kind(TokenKind::Keyword)?;
		let semicolon = input.consume(punct![;])?;

		Ok(Self { keyword, semicolon })
	}
}

impl Spanned for KeywordStmt {
	fn span(&self) -> Span {
		self.keyword.span().through(self.semicolon.span())
	}
}

impl Parse for ExprStmt {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
		let expr = input.parse::<Expr>()?;
		let semicolon = input.consume(punct![;])?;

		Ok(Self { expr, semicolon })
	}
}

impl Spanned for ExprStmt {
	fn span(&self) -> Span {
		self.expr.span().through(self.semicolon.span())
	}
}
