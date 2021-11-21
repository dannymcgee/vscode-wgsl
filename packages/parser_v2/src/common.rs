use std::sync::Arc;

use gramatika::{Parse, ParseStreamer, Span, Spanned, SpannedError, Token as _};

use crate::{
	expr::{Expr, IdentExpr},
	ParseStream, Token, TokenKind, *,
};

#[derive(Clone, DebugLisp)]
pub struct AttributeList {
	pub open_brace: Token,
	pub attributes: Arc<[Attribute]>,
	pub close_brace: Token,
}

#[derive(Clone, DebugLisp)]
pub struct Attribute {
	pub name: Token,
	pub value: Option<Token>,
}

#[derive(Clone, DebugLisp)]
pub struct TypeDecl {
	pub annotator: Option<Token>,
	pub attributes: Option<AttributeList>,
	pub name: IdentExpr,
	pub child_ty: Option<Token>,
	pub storage_class: Option<Token>,
	pub access_mode: Option<Token>,
}

#[derive(Clone, DebugLisp)]
pub struct ArgumentList {
	pub brace_open: Token,
	pub arguments: Arc<[Expr]>,
	pub brace_close: Token,
}

impl Spanned for AttributeList {
	fn span(&self) -> Span {
		self.open_brace.span().through(self.close_brace.span())
	}
}

impl Parse for AttributeList {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
		use TokenKind::*;

		let open_brace = input.consume(brace!["[["])?;

		let mut attributes = vec![];
		while !input.check(brace!["]]"]) {
			match input.peek() {
				Some(token) => match token.as_matchable() {
					(Ident, _, _) => attributes.push(input.parse()?),
					(Punct, ",", _) => input.discard(),
					(_, _, span) => {
						return Err(SpannedError {
							message: "Expected attribute, `,`, or `]]`".into(),
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

		let close_brace = input.consume(brace!["]]"])?;

		Ok(Self {
			open_brace,
			attributes: attributes.into(),
			close_brace,
		})
	}
}

impl Spanned for Attribute {
	fn span(&self) -> Span {
		if let Some(ref value) = self.value {
			self.name.span().through(value.span())
		} else {
			self.name.span()
		}
	}
}

impl Parse for Attribute {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
		use TokenKind::*;

		let name = input.consume_as(TokenKind::Ident, Token::attribute)?;
		let value = if input.check(brace!["("]) {
			input.consume(brace!["("])?;

			let value = match input.next() {
				Some(token) => match token.kind() {
					Ident | Keyword | IntLiteral | UintLiteral | FloatLiteral => Ok(token),
					_ => Err(SpannedError {
						message: "Expected literal or identifier".into(),
						source: input.source(),
						span: Some(token.span()),
					}),
				},
				None => Err(SpannedError {
					message: "Unexpected end of input".into(),
					source: input.source(),
					span: input.prev().map(|token| token.span()),
				}),
			}?;

			input.consume(brace![")"])?;

			Some(value)
		} else {
			None
		};

		Ok(Self { name, value })
	}
}

#[derive(Default)]
struct TypeDeclBuilder {
	attributes: Option<AttributeList>,
	annotator: Option<Token>,
	name: Option<IdentExpr>,
	child_ty: Option<Token>,
	storage_class: Option<Token>,
	access_mode: Option<Token>,
}

impl TypeDeclBuilder {
	fn new() -> Self {
		Self::default()
	}
	fn attributes(&mut self, attributes: AttributeList) -> &mut Self {
		self.attributes = Some(attributes);
		self
	}
	fn annotator(&mut self, colon: Token) -> &mut Self {
		self.annotator = Some(colon);
		self
	}
	fn name(&mut self, name: IdentExpr) -> &mut Self {
		self.name = Some(name);
		self
	}
	fn child_ty(&mut self, child_ty: Token) -> &mut Self {
		self.child_ty = Some(child_ty);
		self
	}
	fn storage_class(&mut self, storage_class: Token) -> &mut Self {
		self.storage_class = Some(storage_class);
		self
	}
	fn access_mode(&mut self, access_mode: Token) -> &mut Self {
		self.access_mode = Some(access_mode);
		self
	}
	fn build(self) -> TypeDecl {
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

impl Spanned for TypeDecl {
	fn span(&self) -> Span {
		let first = self
			.annotator
			.as_ref()
			.map(|token| token.span())
			.or_else(|| self.attributes.as_ref().map(|attr| attr.span()))
			.unwrap_or_else(|| self.name.span());

		let last = self
			.access_mode
			.as_ref()
			.map(|token| token.span())
			.or_else(|| self.storage_class.as_ref().map(|token| token.span()))
			.or_else(|| self.child_ty.as_ref().map(|token| token.span()))
			.unwrap_or_else(|| self.name.span());

		first.through(last)
	}
}

impl Parse for TypeDecl {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
		use TokenKind::*;
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
						Some(token) => match token.as_matchable() {
							(Type | Ident, _, _) => {
								builder.child_ty(token);
							}
							(Keyword, "function" | "private" | "workgroup" | "uniform" | "storage", _) => {
								builder.storage_class(token);
							}
							(Keyword, "read" | "write" | "read_write", _) => {
								builder.access_mode(token);
							}
							(Punct, ",", _) => {},
							(_, _, span) => {
								return Err(SpannedError {
									message: "Expected type, storage class, access mode, or texel format"
										.into(),
									source: input.source(),
									span: Some(span),
								})
							}
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

impl Parse for ArgumentList {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
		use Token::*;

		let brace_open = input.consume(brace!["("])?;
		let mut arguments = vec![];
		while !input.check(brace![")"]) {
			match input.peek() {
				Some(Punct(lex, _)) if lex == "," => input.discard(),
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
			arguments: arguments.into(),
			brace_close,
		})
	}
}
