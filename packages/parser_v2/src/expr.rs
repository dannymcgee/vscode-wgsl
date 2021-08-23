use std::fmt;

use gramatika::{debug, Parse, ParseStreamer, Result, Span, Spanned, SpannedError};

use crate::{
	common::{ArgumentList, TypeDecl},
	ParseStream, Token, *,
};

#[derive(Clone, DebugLisp)]
pub enum Expr<'a> {
	Unary(UnaryExpr<'a>),
	Binary(BinaryExpr<'a>),
	Assignment(BinaryExpr<'a>),
	FnCall(FnCallExpr<'a>),
	TypeCtor(TypeCtorExpr<'a>),
	Group(GroupExpr<'a>),
	Bitcast(BitcastExpr<'a>),
	Literal(Token<'a>),
	Ident(IdentExpr<'a>),
	Primary(PrimaryExpr<'a>),
}

#[derive(Clone, DebugLisp)]
pub struct UnaryExpr<'a> {
	pub op: Token<'a>,
	pub expr: Box<Expr<'a>>,
}

#[derive(Clone, DebugLisp)]
pub struct BinaryExpr<'a> {
	pub lhs: Box<Expr<'a>>,
	pub op: Token<'a>,
	pub rhs: Box<Expr<'a>>,
}

#[derive(Clone, DebugLisp)]
pub struct FnCallExpr<'a> {
	pub ident: IdentExpr<'a>,
	pub arguments: ArgumentList<'a>,
}

#[derive(Clone, DebugLisp)]
pub struct TypeCtorExpr<'a> {
	pub ty: TypeDecl<'a>,
	pub arguments: ArgumentList<'a>,
}

#[derive(Clone, DebugLisp)]
pub struct GroupExpr<'a> {
	pub brace_open: Token<'a>,
	pub expr: Box<Expr<'a>>,
	pub brace_close: Token<'a>,
}

#[derive(Clone, DebugLisp)]
pub struct BitcastExpr<'a> {
	pub keyword: Token<'a>,
	pub ty: TypeDecl<'a>,
	pub expr: GroupExpr<'a>,
}

#[derive(Clone, DebugLisp)]
pub struct IdentExpr<'a> {
	pub namespace: Option<Token<'a>>,
	pub name: Token<'a>,
}

#[derive(Clone, DebugLisp)]
pub struct PrimaryExpr<'a> {
	pub expr: Box<Expr<'a>>,
	pub postfix: Option<PostfixExpr<'a>>,
}

#[derive(Clone, DebugLisp)]
pub struct PostfixExpr<'a> {
	pub accessor: Accessor<'a>,
	pub expr: Box<Expr<'a>>,
	pub postfix: Option<Box<PostfixExpr<'a>>>,
}

#[derive(Clone)]
pub enum Accessor<'a> {
	Dot(Token<'a>),
	Index([Token<'a>; 2]),
}

impl<'a> gramatika::DebugLisp for Accessor<'a> {
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

trait RecursiveDescent<'a> {
	type Token: gramatika::Token;

	fn assignment(&mut self) -> Result<'a, Expr<'a>>;
	fn short_circuit_or(&mut self) -> Result<'a, Expr<'a>>;
	fn short_circuit_and(&mut self) -> Result<'a, Expr<'a>>;
	fn inclusive_or(&mut self) -> Result<'a, Expr<'a>>;
	fn exclusive_or(&mut self) -> Result<'a, Expr<'a>>;
	fn and(&mut self) -> Result<'a, Expr<'a>>;
	fn equality(&mut self) -> Result<'a, Expr<'a>>;
	fn relational(&mut self) -> Result<'a, Expr<'a>>;
	fn shift(&mut self) -> Result<'a, Expr<'a>>;
	fn additive(&mut self) -> Result<'a, Expr<'a>>;
	fn multiplicative(&mut self) -> Result<'a, Expr<'a>>;
	fn unary(&mut self) -> Result<'a, Expr<'a>>;
	fn binary(
		&mut self,
		operators: &[Self::Token],
		operand_method: fn(&mut Self) -> Result<'a, Expr<'a>>,
	) -> Result<'a, Expr<'a>>;
}

// --- gramatika impls -------------------------------------------------------------------

impl<'a> Spanned for Expr<'a> {
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
		}
	}
}

impl<'a> Spanned for PrimaryExpr<'a> {
	fn span(&self) -> Span {
		match self.postfix {
			Some(ref postfix) => self.expr.span().through(postfix.span()),
			None => self.expr.span(),
		}
	}
}

impl<'a> Spanned for PostfixExpr<'a> {
	fn span(&self) -> Span {
		match self.postfix {
			Some(ref postfix) => self.expr.span().through(postfix.span()),
			None => self.expr.span(),
		}
	}
}

impl<'a> Spanned for IdentExpr<'a> {
	fn span(&self) -> Span {
		match self.namespace {
			Some(ref token) => token.span().through(self.name.span()),
			None => self.name.span(),
		}
	}
}

impl<'a> Spanned for FnCallExpr<'a> {
	fn span(&self) -> Span {
		self.ident.span().through(self.arguments.brace_close.span())
	}
}

impl<'a> Parse<'a> for Expr<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> Result<'a, Self> {
		input.assignment()
	}
}

impl<'a> Parse<'a> for PrimaryExpr<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> Result<'a, Self> {
		use Token::*;

		let expr = match input.peek() {
			Some(Type(_, _)) => Ok(Expr::TypeCtor(input.parse::<TypeCtorExpr>()?)),
			Some(Brace("(", _)) => Ok(Expr::Group(input.parse::<GroupExpr>()?)),
			Some(Keyword("bitcast", _)) => Ok(Expr::Bitcast(input.parse::<BitcastExpr>()?)),
			Some(
				IntLiteral(_, _)
				| UintLiteral(_, _)
				| FloatLiteral(_, _)
				| Keyword("true" | "false", _),
			) => {
				let token = input.next().unwrap();
				Ok(Expr::Literal(token))
			}
			Some(Ident(_, _)) => {
				let mut ident = input.parse::<IdentExpr>()?;

				if input.check(brace!["("]) {
					ident.name = input.upgrade_last(TokenKind::Ident, Token::function)?;
					let arguments = input.parse::<ArgumentList>()?;

					Ok(Expr::FnCall(FnCallExpr { ident, arguments }))
				} else {
					Ok(Expr::Ident(ident))
				}
			}
			Some(Comment(_, _)) => {
				input.discard();
				input.parse::<Expr>()
			}
			Some(other) => Err(SpannedError {
				message: "Expected expression".into(),
				span: Some(other.span()),
				source: input.source(),
			}),
			None => Err(SpannedError {
				message: "Unexpected end of input".into(),
				source: input.source(),
				span: None,
			}),
		}?;

		let postfix = match input.peek() {
			Some(Punct(".", _) | Punct("[", _)) => Some(input.parse::<PostfixExpr>()?),
			_ => None,
		};

		Ok(PrimaryExpr {
			expr: Box::new(expr),
			postfix,
		})
	}
}

impl<'a> Parse<'a> for FnCallExpr<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> Result<'a, Self> {
		let mut ident = input.parse::<IdentExpr>()?;
		ident.name = input.upgrade_last(TokenKind::Ident, Token::function)?;
		let arguments = input.parse::<ArgumentList>()?;

		Ok(Self { ident, arguments })
	}
}

impl<'a> Parse<'a> for TypeCtorExpr<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> Result<'a, Self> {
		let ty = input.parse::<TypeDecl>()?;
		let arguments = input.parse::<ArgumentList>()?;

		Ok(Self { ty, arguments })
	}
}

impl<'a> Parse<'a> for GroupExpr<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> Result<'a, Self> {
		let brace_open = input.consume(brace!["("])?;
		let expr = input.parse::<Expr>()?;
		let brace_close = input.consume(brace![")"])?;

		Ok(Self {
			brace_open,
			expr: Box::new(expr),
			brace_close,
		})
	}
}

impl<'a> Parse<'a> for BitcastExpr<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> Result<'a, Self> {
		let keyword = input.consume(keyword![bitcast])?;
		input.consume(operator![<])?;
		let ty = input.parse::<TypeDecl>()?;
		input.consume(operator![>])?;
		let expr = input.parse::<GroupExpr>()?;

		Ok(Self { keyword, ty, expr })
	}
}

impl<'a> Parse<'a> for IdentExpr<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> Result<'a, Self> {
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

impl<'a> Parse<'a> for PostfixExpr<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> Result<'a, Self> {
		use Token::*;

		if input.check(punct![.]) {
			let dot = input.next().unwrap();
			let accessor = Accessor::Dot(dot);

			let mut expr = input.parse::<PrimaryExpr>()?;
			if let Expr::Ident(IdentExpr { name, .. }) = expr.expr.as_mut() {
				*name = input.upgrade(*name, Token::field)?;
			}

			let postfix = match input.peek() {
				Some(Punct(".", _) | Brace("[", _)) => {
					Some(Box::new(input.parse::<PostfixExpr>()?))
				}
				_ => None,
			};

			Ok(Self {
				accessor,
				expr: Box::new(Expr::Primary(expr)),
				postfix,
			})
		} else {
			let brace_open = input.consume(brace!["["])?;
			let expr = input.parse::<Expr>()?;
			let brace_close = input.consume(brace!["]"])?;

			let postfix = match input.peek() {
				Some(Punct(".", _) | Brace("[", _)) => {
					Some(Box::new(input.parse::<PostfixExpr>()?))
				}
				_ => None,
			};

			Ok(Self {
				accessor: Accessor::Index([brace_open, brace_close]),
				expr: Box::new(expr),
				postfix,
			})
		}
	}
}

// --- RecursiveDescent impl -------------------------------------------------------------

impl<'a> RecursiveDescent<'a> for ParseStream<'a> {
	type Token = Token<'a>;

	fn assignment(&mut self) -> Result<'a, Expr<'a>> {
		let lhs = self.short_circuit_or()?;

		if self.check(operator![=]) {
			let eq = self.consume(operator![=])?;
			let value = self.assignment()?;

			match &lhs {
				Expr::Primary(PrimaryExpr { expr, .. })
					if matches!(expr.as_ref(), Expr::Ident(_)) =>
				{
					Ok(Expr::Assignment(BinaryExpr {
						lhs: Box::new(lhs),
						op: eq,
						rhs: Box::new(value),
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

	fn short_circuit_or(&mut self) -> Result<'a, Expr<'a>> {
		self.binary(&[operator![||]], Self::short_circuit_and)
	}

	fn short_circuit_and(&mut self) -> Result<'a, Expr<'a>> {
		self.binary(&[operator![&&]], Self::inclusive_or)
	}

	fn inclusive_or(&mut self) -> Result<'a, Expr<'a>> {
		self.binary(&[operator![|]], Self::exclusive_or)
	}

	fn exclusive_or(&mut self) -> Result<'a, Expr<'a>> {
		self.binary(&[operator![^]], Self::and)
	}

	fn and(&mut self) -> Result<'a, Expr<'a>> {
		self.binary(&[operator![&]], Self::equality)
	}

	fn equality(&mut self) -> Result<'a, Expr<'a>> {
		self.binary(&[operator![==], operator![!=]], Self::relational)
	}

	fn relational(&mut self) -> Result<'a, Expr<'a>> {
		self.binary(
			&[operator![<=], operator![>=], operator![<], operator![>]],
			Self::shift,
		)
	}

	fn shift(&mut self) -> Result<'a, Expr<'a>> {
		self.binary(&[operator![<<], operator![>>]], Self::additive)
	}

	fn additive(&mut self) -> Result<'a, Expr<'a>> {
		self.binary(&[operator!["+"], operator!["-"]], Self::multiplicative)
	}

	fn multiplicative(&mut self) -> Result<'a, Expr<'a>> {
		self.binary(&[operator![*], operator![/], operator![%]], Self::unary)
	}

	fn unary(&mut self) -> Result<'a, Expr<'a>> {
		if matches!(
			self.peek(),
			Some(Token::Operator("-" | "!" | "~" | "*" | "&", _))
		) {
			let op = self.next().unwrap();
			let expr = self.unary()?;

			Ok(Expr::Unary(UnaryExpr {
				op,
				expr: Box::new(expr),
			}))
		} else {
			Ok(Expr::Primary(self.parse()?))
		}
	}

	fn binary(
		&mut self,
		operators: &[Self::Token],
		operand_method: fn(&mut Self) -> Result<'a, Expr<'a>>,
	) -> Result<'a, Expr<'a>> {
		let mut expr = operand_method(self)?;

		while operators.iter().any(|op| self.check(*op)) {
			let op = self.next().unwrap();
			let rhs = operand_method(self)?;

			expr = Expr::Binary(BinaryExpr {
				lhs: Box::new(expr),
				op,
				rhs: Box::new(rhs),
			});
		}

		Ok(expr)
	}
}
