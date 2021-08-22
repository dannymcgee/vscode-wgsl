use gramatika::{Parse, ParseStreamer, Span, Spanned, SpannedError};

use crate::{
	expr::{Expr, IdentExpr},
	ParseStream, Token, TokenKind, *,
};

#[derive(Clone, DebugLisp)]
pub struct AttributeList<'a> {
	pub open_brace: Token<'a>,
	pub attributes: Vec<Attribute<'a>>,
	pub close_brace: Token<'a>,
}

#[derive(Clone, DebugLisp)]
pub struct Attribute<'a> {
	pub name: Token<'a>,
	pub value: Option<Token<'a>>,
}

#[derive(Clone, DebugLisp)]
pub struct TypeDecl<'a> {
	pub annotator: Option<Token<'a>>,
	pub attributes: Option<AttributeList<'a>>,
	pub name: IdentExpr<'a>,
	pub child_ty: Option<Token<'a>>,
	pub storage_class: Option<Token<'a>>,
	pub access_mode: Option<Token<'a>>,
}

#[derive(Clone, DebugLisp)]
pub struct ArgumentList<'a> {
	pub brace_open: Token<'a>,
	pub arguments: Vec<Expr<'a>>,
	pub brace_close: Token<'a>,
}

impl<'a> Spanned for AttributeList<'a> {
	fn span(&self) -> Span {
		self.open_brace.span().through(self.close_brace.span())
	}
}

impl<'a> Parse<'a> for AttributeList<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		use Token::*;

		let open_brace = input.consume(brace!["[["])?;

		let mut attributes = vec![];
		while !input.check(brace!["]]"]) {
			match input.peek() {
				Some(Ident(_, _)) => attributes.push(input.parse()?),
				Some(Punct(",", _)) => input.discard(),
				Some(other) => {
					return Err(SpannedError {
						message: "Expected attribute, `,`, or `]]`".into(),
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

		let close_brace = input.consume(brace!["]]"])?;

		Ok(Self {
			open_brace,
			attributes,
			close_brace,
		})
	}
}

impl<'a> Spanned for Attribute<'a> {
	fn span(&self) -> Span {
		if let Some(value) = self.value {
			self.name.span().through(value.span())
		} else {
			self.name.span()
		}
	}
}

impl<'a> Parse<'a> for Attribute<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		// TODO - see note in `tokens`
		let name = input.consume_as(TokenKind::Ident, Token::attribute)?;
		let value = if input.check(brace!["("]) {
			input.consume(brace!["("])?;
			let value = input.next().ok_or_else(|| SpannedError {
				message: "Unexpected end of input".into(),
				source: input.source(),
				span: None,
			})?;
			input.consume(brace![")"])?;

			Some(value)
		} else {
			None
		};

		Ok(Self { name, value })
	}
}

#[derive(Default)]
struct TypeDeclBuilder<'a> {
	attributes: Option<AttributeList<'a>>,
	annotator: Option<Token<'a>>,
	name: Option<IdentExpr<'a>>,
	child_ty: Option<Token<'a>>,
	storage_class: Option<Token<'a>>,
	access_mode: Option<Token<'a>>,
}

impl<'a> TypeDeclBuilder<'a> {
	fn new() -> Self {
		Self::default()
	}
	fn attributes(&mut self, attributes: AttributeList<'a>) -> &mut Self {
		self.attributes = Some(attributes);
		self
	}
	fn annotator(&mut self, colon: Token<'a>) -> &mut Self {
		self.annotator = Some(colon);
		self
	}
	fn name(&mut self, name: IdentExpr<'a>) -> &mut Self {
		self.name = Some(name);
		self
	}
	fn child_ty(&mut self, child_ty: Token<'a>) -> &mut Self {
		self.child_ty = Some(child_ty);
		self
	}
	fn storage_class(&mut self, storage_class: Token<'a>) -> &mut Self {
		self.storage_class = Some(storage_class);
		self
	}
	fn access_mode(&mut self, access_mode: Token<'a>) -> &mut Self {
		self.access_mode = Some(access_mode);
		self
	}
	fn build(self) -> TypeDecl<'a> {
		TypeDecl {
			annotator: self.annotator,
			attributes: self.attributes,
			name: self.name.expect("`name` field is required!"),
			child_ty: self.child_ty,
			storage_class: self.storage_class,
			access_mode: self.access_mode,
		}
	}
}

impl<'a> Spanned for TypeDecl<'a> {
	fn span(&self) -> Span {
		let first = self
			.annotator
			.map(|token| token.span())
			.or_else(|| self.attributes.as_ref().map(|attr| attr.span()))
			.unwrap_or_else(|| self.name.span());

		let last = self
			.access_mode
			.map(|token| token.span())
			.or_else(|| self.storage_class.map(|token| token.span()))
			.or_else(|| self.child_ty.map(|token| token.span()))
			.unwrap_or_else(|| self.name.span());

		first.through(last)
	}
}

impl<'a> Parse<'a> for TypeDecl<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		use Token::*;
		let mut builder = TypeDeclBuilder::new();

		if input.check(punct![:]) || input.check(operator![->]) {
			builder.annotator(input.next().unwrap());
		}
		if input.check(brace!["[["]) {
			builder.attributes(input.parse()?);
		}
		if input.check_kind(TokenKind::Type) {
			builder.name(IdentExpr {
				namespace: None,
				name: input.next().unwrap(),
			});

			if input.check(operator![<]) {
				input.consume(operator![<])?;

				while !input.check(operator![>]) {
					#[rustfmt::skip]
					match input.next() {
						Some(token @ Type(_, _) | token @ Ident(_, _)) => {
							builder.child_ty(token);
						}
						Some(token @ Keyword("function" | "private" | "workgroup" | "uniform" | "storage", _)) => {
							builder.storage_class(token);
						}
						Some(token @ Keyword("read" | "write" | "read_write", _)) => {
							builder.access_mode(token);
						}
						Some(Punct(",", _)) => {},
						Some(other) => {
							return Err(SpannedError {
								message: "Expected type, storage class, access mode, or texel format"
									.into(),
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

				input.consume(operator![>])?;
			}
		} else {
			builder.name(input.parse()?);
		}

		Ok(builder.build())
	}
}

impl<'a> Parse<'a> for ArgumentList<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		use Token::*;

		let brace_open = input.consume(brace!["("])?;
		let mut arguments = vec![];
		while !input.check(brace![")"]) {
			match input.peek() {
				Some(Punct(",", _)) => input.discard(),
				Some(_) => arguments.push(input.parse::<Expr>()?),
				None => {
					return Err(SpannedError {
						message: "Unexpected end of input".into(),
						source: input.source(),
						span: None,
					})
				}
			};
		}
		let brace_close = input.consume(brace![")"])?;

		Ok(Self {
			brace_open,
			arguments,
			brace_close,
		})
	}
}
