use gramatika::{Parse, ParseStreamer, Span, SpannedError, Token as _};

use crate::{ParseStream, Token, TokenKind, *};

#[derive(DebugLisp)]
pub struct AttributeList<'a> {
	pub open_brace: Token<'a>,
	pub attributes: Vec<Attribute<'a>>,
	pub close_brace: Token<'a>,
	pub span: Span,
}

#[derive(DebugLisp)]
pub struct Attribute<'a> {
	pub name: Token<'a>,
	pub value: Option<Token<'a>>,
	pub span: Span,
}

#[derive(DebugLisp)]
pub struct TypeDecl<'a> {
	pub colon: Option<Token<'a>>,
	pub attributes: Option<AttributeList<'a>>,
	pub namespace: Option<Token<'a>>,
	pub name: Token<'a>,
	pub child_ty: Option<Token<'a>>,
	pub storage_class: Option<Token<'a>>,
	pub access_mode: Option<Token<'a>>,
	pub span: Span,
}

impl<'a> Parse<'a> for AttributeList<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		use Token::*;

		let open_brace = input.consume(brace!["[["])?;

		let mut attributes = vec![];
		loop {
			match input.peek() {
				Some(Ident(_, _)) => {
					attributes.push(input.parse()?);
				}
				Some(Punct(",", _)) => {
					input.next().unwrap();
					continue;
				}
				Some(Brace("]]", _)) => break,
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
			span: open_brace.span().through(close_brace.span()),
		})
	}
}

impl<'a> Parse<'a> for Attribute<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		let name = input.consume_kind(TokenKind::Ident)?;
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

		let span = if let Some(value) = value {
			name.span().through(value.span())
		} else {
			name.span()
		};

		Ok(Self { name, value, span })
	}
}

#[derive(Default)]
struct TypeDeclBuilder<'a> {
	attributes: Option<AttributeList<'a>>,
	colon: Option<Token<'a>>,
	namespace: Option<Token<'a>>,
	name: Option<Token<'a>>,
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
	fn colon(&mut self, colon: Token<'a>) -> &mut Self {
		self.colon = Some(colon);
		self
	}
	fn namespace(&mut self, namespace: Option<Token<'a>>) -> &mut Self {
		self.namespace = namespace;
		self
	}
	fn name(&mut self, name: Token<'a>) -> &mut Self {
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
		let first = self
			.colon
			.or_else(|| self.attributes.as_ref().map(|attr| attr.open_brace))
			.or(self.namespace)
			.or(self.name)
			.expect("`name` field is required!");

		let last = self
			.access_mode
			.or(self.storage_class)
			.or(self.child_ty)
			.or(self.name)
			.unwrap();

		let span = first.span().through(last.span());

		TypeDecl {
			colon: self.colon,
			attributes: self.attributes,
			namespace: self.namespace,
			name: self.name.unwrap(),
			child_ty: self.child_ty,
			storage_class: self.storage_class,
			access_mode: self.access_mode,
			span,
		}
	}
}

impl<'a> Parse<'a> for TypeDecl<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		use Token::*;
		let mut builder = TypeDeclBuilder::new();

		if input.check(punct![:]) {
			builder.colon(input.consume(punct![:])?);
		}
		if input.check(brace!["[["]) {
			builder.attributes(input.parse()?);
		}
		if input.check_kind(TokenKind::Type) {
			builder.name(input.consume_kind(TokenKind::Type)?);
			if input.check(operator![<]) {
				input.consume(operator![<])?;

				loop {
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
						Some(Punct(",", _)) => continue,
						Some(Operator(">", _)) => break,
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
			}
		} else {
			let mut name = input.consume_kind(TokenKind::Ident)?;
			let namespace = if input.check(punct![::]) {
				input.consume(punct![::])?;
				let namespace = name;
				name = input.consume_kind(TokenKind::Ident)?;

				Some(namespace)
			} else {
				None
			};

			builder.namespace(namespace).name(name);
		}

		Ok(builder.build())
	}
}
