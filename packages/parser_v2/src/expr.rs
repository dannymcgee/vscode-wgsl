use std::{fmt, sync::Arc};

use gramatika::{debug, Parse, ParseStreamer, Result, Span, Spanned, SpannedError, Token as _};

use crate::{
	common::{ArgumentList, TypeDecl},
	ParseStream, Token, *,
};

#[derive(Clone, DebugLisp)]
pub enum Expr {
	Unary(UnaryExpr),
	Binary(BinaryExpr),
	Assignment(BinaryExpr),
	FnCall(FnCallExpr),
	TypeCtor(TypeCtorExpr),
	Group(GroupExpr),
	Bitcast(BitcastExpr),
	Literal(Token),
	Ident(IdentExpr),
	Primary(PrimaryExpr),
	Error(SpannedError),
}

#[derive(Clone, DebugLisp)]
pub struct UnaryExpr {
	pub op: Token,
	pub expr: Arc<Expr>,
}

#[derive(Clone, DebugLisp)]
pub struct BinaryExpr {
	pub lhs: Arc<Expr>,
	pub op: Token,
	pub rhs: Arc<Expr>,
}

#[derive(Clone, DebugLisp)]
pub struct FnCallExpr {
	pub ident: IdentExpr,
	pub arguments: ArgumentList,
}

#[derive(Clone, DebugLisp)]
pub struct TypeCtorExpr {
	pub ty: TypeDecl,
	pub arguments: ArgumentList,
}

#[derive(Clone, DebugLisp)]
pub struct GroupExpr {
	pub brace_open: Token,
	pub expr: Arc<Expr>,
	pub brace_close: Token,
}

#[derive(Clone, DebugLisp)]
pub struct BitcastExpr {
	pub keyword: Token,
	pub ty: TypeDecl,
	pub expr: GroupExpr,
}

#[derive(Clone, DebugLisp)]
pub struct IdentExpr {
	pub namespace: Option<Token>,
	pub name: Token,
}

#[derive(Clone, DebugLisp)]
pub struct PrimaryExpr {
	pub expr: Arc<Expr>,
	pub postfix: Option<PostfixExpr>,
}

struct PrimaryExprBuilder {
	pub expr: Box<Expr>,
	pub postfix: Option<PostfixExpr>,
}

#[derive(Clone, DebugLisp)]
pub struct PostfixExpr {
	pub accessor: Accessor,
	pub expr: Arc<Expr>,
	pub postfix: Option<Arc<PostfixExpr>>,
}

#[derive(Clone)]
pub enum Accessor {
	Dot(Token),
	Index([Token; 2]),
}

impl gramatika::DebugLisp for Accessor {
	fn fmt(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
		match self {
			Accessor::Dot(token) => write!(f, "(Accessor::Dot {:?})", token),
			Accessor::Index(tokens) => {
				writeln!(f, "(Accessor::Index")?;
				for token in tokens {
					write!(f, "{}", debug::INDENT.repeat(indent + 1))?;
					writeln!(f, "{:?},", token)?;
				}
				write!(f, "{})", debug::INDENT.repeat(indent))
			}
		}
	}
}

// -- Recursive descent trait ------------------------------------------------------------

trait RecursiveDescent {
	type Token: gramatika::Token;

	fn assignment(&mut self) -> Result<Expr>;
	fn short_circuit_or(&mut self) -> Result<Expr>;
	fn short_circuit_and(&mut self) -> Result<Expr>;
	fn inclusive_or(&mut self) -> Result<Expr>;
	fn exclusive_or(&mut self) -> Result<Expr>;
	fn and(&mut self) -> Result<Expr>;
	fn equality(&mut self) -> Result<Expr>;
	fn relational(&mut self) -> Result<Expr>;
	fn shift(&mut self) -> Result<Expr>;
	fn additive(&mut self) -> Result<Expr>;
	fn multiplicative(&mut self) -> Result<Expr>;
	fn unary(&mut self) -> Result<Expr>;
	fn binary(
		&mut self,
		operators: &[Self::Token],
		operand_method: fn(&mut Self) -> Result<Expr>,
	) -> Result<Expr>;
}

// --- gramatika impls -------------------------------------------------------------------

impl Spanned for Expr {
	fn span(&self) -> Span {
		match self {
			Expr::Unary(inner) => inner.op.span().through(inner.expr.span()),
			Expr::Binary(inner) => inner.lhs.span().through(inner.rhs.span()),
			Expr::Assignment(inner) => inner.lhs.span().through(inner.rhs.span()),
			Expr::FnCall(inner) => inner.span(),
			Expr::TypeCtor(inner) => inner.ty.span().through(inner.arguments.brace_close.span()),
			Expr::Group(inner) => inner.brace_open.span().through(inner.brace_close.span()),
			Expr::Bitcast(inner) => inner.keyword.span().through(inner.expr.brace_close.span()),
			Expr::Literal(inner) => inner.span(),
			Expr::Ident(inner) => inner.span(),
			Expr::Primary(inner) => inner.span(),
			Expr::Error(err) => err.span.unwrap_or_default(),
		}
	}
}

impl Spanned for PrimaryExpr {
	fn span(&self) -> Span {
		match self.postfix {
			Some(ref postfix) => self.expr.span().through(postfix.span()),
			None => self.expr.span(),
		}
	}
}

impl Spanned for PostfixExpr {
	fn span(&self) -> Span {
		match self.postfix {
			Some(ref postfix) => self.expr.span().through(postfix.span()),
			None => self.expr.span(),
		}
	}
}

impl Spanned for IdentExpr {
	fn span(&self) -> Span {
		match self.namespace {
			Some(ref token) => token.span().through(self.name.span()),
			None => self.name.span(),
		}
	}
}

impl Spanned for FnCallExpr {
	fn span(&self) -> Span {
		self.ident.span().through(self.arguments.brace_close.span())
	}
}

impl Parse for Expr {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> Result<Self> {
		match input.assignment() {
			Ok(expr) => Ok(expr),
			Err(err) => Ok(Expr::Error(err)),
		}
	}
}

impl Parse for PrimaryExprBuilder {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> Result<Self> {
		use TokenKind::*;

		let expr = match input.peek() {
			Some(token) => match token.as_matchable() {
				(Type, _, _) => Ok(Expr::TypeCtor(input.parse::<TypeCtorExpr>()?)),
				(Brace, "(", _) => Ok(Expr::Group(input.parse::<GroupExpr>()?)),
				(Keyword, "bitcast", _) => Ok(Expr::Bitcast(input.parse::<BitcastExpr>()?)),
				(IntLiteral, _, _)
				| (UintLiteral, _, _)
				| (FloatLiteral, _, _)
				| (Keyword, "true" | "false", _) => {
					let token = input.next().unwrap();
					Ok(Expr::Literal(token))
				}
				(Ident, _, _) => {
					let mut ident = input.parse::<IdentExpr>()?;

					if input.check(brace!["("]) {
						ident.name = input.upgrade_last(TokenKind::Ident, Token::function)?;
						let arguments = input.parse::<ArgumentList>()?;

						Ok(Expr::FnCall(FnCallExpr { ident, arguments }))
					} else {
						Ok(Expr::Ident(ident))
					}
				}
				(Comment, _, _) => {
					input.discard();
					input.parse::<Expr>()
				}
				(_, _, span) => Err(SpannedError {
					message: "Expected expression".into(),
					span: Some(span),
					source: input.source(),
				}),
			},
			None => Err(SpannedError {
				message: "Unexpected end of input".into(),
				source: input.source(),
				span: input.prev().map(|token| token.span()),
			}),
		}?;

		let postfix = match input.peek().map(|t| t.as_matchable()) {
			Some((Punct, ".", _) | (Brace, "[", _)) => Some(input.parse::<PostfixExpr>()?),
			_ => None,
		};

		Ok(PrimaryExprBuilder {
			expr: Box::new(expr),
			postfix,
		})
	}
}

impl PrimaryExprBuilder {
	fn build(self) -> PrimaryExpr {
		PrimaryExpr {
			expr: self.expr.into(),
			postfix: self.postfix,
		}
	}
}

impl Parse for FnCallExpr {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> Result<Self> {
		let mut ident = input.parse::<IdentExpr>()?;
		ident.name = input.upgrade_last(TokenKind::Ident, Token::function)?;
		let arguments = input.parse::<ArgumentList>()?;

		Ok(Self { ident, arguments })
	}
}

impl Parse for TypeCtorExpr {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> Result<Self> {
		let ty = input.parse::<TypeDecl>()?;
		let arguments = input.parse::<ArgumentList>()?;

		Ok(Self { ty, arguments })
	}
}

impl Parse for GroupExpr {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> Result<Self> {
		let brace_open = input.consume(brace!["("])?;
		let expr = input.parse::<Expr>()?;
		let brace_close = input.consume(brace![")"])?;

		Ok(Self {
			brace_open,
			expr: Arc::new(expr),
			brace_close,
		})
	}
}

impl Parse for BitcastExpr {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> Result<Self> {
		let keyword = input.consume(keyword![bitcast])?;
		input.consume(operator![<])?;
		let ty = input.parse::<TypeDecl>()?;
		input.consume(operator![>])?;
		let expr = input.parse::<GroupExpr>()?;

		Ok(Self { keyword, ty, expr })
	}
}

impl Parse for IdentExpr {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> Result<Self> {
		let mut ident = input.consume_kind(TokenKind::Ident)?;

		if input.check(punct![::]) {
			ident = input.upgrade_last(TokenKind::Ident, Token::module)?;
			input.consume(punct![::])?;
			let name = input.consume_kind(TokenKind::Ident)?;

			Ok(Self {
				namespace: Some(ident),
				name,
			})
		} else {
			Ok(Self {
				namespace: None,
				name: ident,
			})
		}
	}
}

impl Parse for PostfixExpr {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> Result<Self> {
		use TokenKind::*;

		if input.check(punct![.]) {
			let dot = input.next().unwrap();
			let accessor = Accessor::Dot(dot);

			let mut expr = input.parse::<PrimaryExprBuilder>()?;
			if let Expr::Ident(IdentExpr { name, .. }) = expr.expr.as_mut() {
				*name = input.upgrade(name.clone(), Token::field)?;
			}

			let postfix = match input.peek().map(|t| t.as_matchable()) {
				Some((Punct, ".", _) | (Brace, "[", _)) => {
					Some(Arc::new(input.parse::<PostfixExpr>()?))
				}
				_ => None,
			};

			Ok(Self {
				accessor,
				expr: Arc::new(Expr::Primary(expr.build())),
				postfix,
			})
		} else {
			let brace_open = input.consume(brace!["["])?;
			let expr = input.parse::<Expr>()?;
			let brace_close = input.consume(brace!["]"])?;

			let postfix = match input.peek().map(|t| t.as_matchable()) {
				Some((Punct, ".", _) | (Brace, "[", _)) => {
					Some(Arc::new(input.parse::<PostfixExpr>()?))
				}
				_ => None,
			};

			Ok(Self {
				accessor: Accessor::Index([brace_open, brace_close]),
				expr: Arc::new(expr),
				postfix,
			})
		}
	}
}

// --- RecursiveDescent impl -------------------------------------------------------------

impl RecursiveDescent for ParseStream {
	type Token = Token;

	fn assignment(&mut self) -> Result<Expr> {
		let lhs = self.short_circuit_or()?;

		if self.check(operator![=]) {
			let eq = self.consume(operator![=])?;
			let value = self.assignment()?;

			match &lhs {
				Expr::Primary(PrimaryExpr { expr, .. })
					if matches!(expr.as_ref(), Expr::Ident(_)) =>
				{
					Ok(Expr::Assignment(BinaryExpr {
						lhs: Arc::new(lhs),
						op: eq,
						rhs: Arc::new(value),
					}))
				}
				other => Err(SpannedError {
					message: "Invalid assignment target".into(),
					source: self.source(),
					span: Some(other.span()),
				}),
			}
		} else {
			Ok(lhs)
		}
	}

	fn short_circuit_or(&mut self) -> Result<Expr> {
		self.binary(&[operator![||]], Self::short_circuit_and)
	}

	fn short_circuit_and(&mut self) -> Result<Expr> {
		self.binary(&[operator![&&]], Self::inclusive_or)
	}

	fn inclusive_or(&mut self) -> Result<Expr> {
		self.binary(&[operator![|]], Self::exclusive_or)
	}

	fn exclusive_or(&mut self) -> Result<Expr> {
		self.binary(&[operator![^]], Self::and)
	}

	fn and(&mut self) -> Result<Expr> {
		self.binary(&[operator![&]], Self::equality)
	}

	fn equality(&mut self) -> Result<Expr> {
		self.binary(&[operator![==], operator![!=]], Self::relational)
	}

	fn relational(&mut self) -> Result<Expr> {
		self.binary(
			&[operator![<=], operator![>=], operator![<], operator![>]],
			Self::shift,
		)
	}

	fn shift(&mut self) -> Result<Expr> {
		self.binary(&[operator![<<], operator![>>]], Self::additive)
	}

	fn additive(&mut self) -> Result<Expr> {
		self.binary(&[operator!["+"], operator!["-"]], Self::multiplicative)
	}

	fn multiplicative(&mut self) -> Result<Expr> {
		self.binary(&[operator![*], operator![/], operator![%]], Self::unary)
	}

	fn unary(&mut self) -> Result<Expr> {
		if matches!(
			self.peek().map(|t| t.as_matchable()),
			Some((TokenKind::Operator, "-" | "!" | "~" | "*" | "&", _))
		) {
			let op = self.next().unwrap();
			let expr = self.unary()?;

			Ok(Expr::Unary(UnaryExpr {
				op,
				expr: Arc::new(expr),
			}))
		} else {
			Ok(Expr::Primary(self.parse::<PrimaryExprBuilder>()?.build()))
		}
	}

	fn binary(
		&mut self,
		operators: &[Self::Token],
		operand_method: fn(&mut Self) -> Result<Expr>,
	) -> Result<Expr> {
		let mut expr = operand_method(self)?;

		while operators.iter().any(|op| self.check(op.clone())) {
			let op = self.next().unwrap();
			let rhs = operand_method(self)?;

			expr = Expr::Binary(BinaryExpr {
				lhs: Arc::new(expr),
				op,
				rhs: Arc::new(rhs),
			});
		}

		Ok(expr)
	}
}
