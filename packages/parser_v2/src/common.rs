use std::sync::Arc;

use gramatika::{Parse, ParseStreamer, Span, Spanned, SpannedError, Token as _};

use crate::{
	expr::{Expr, GroupExpr, IdentExpr},
	ParseStream, Token, TokenKind, *,
};

#[derive(Clone, DebugLisp)]
pub struct AttributeList {
	pub attributes: Arc<[Attribute]>,
}

#[derive(Clone, DebugLisp)]
pub struct Attribute {
	pub at_sign: Token,
	pub name: Token,
	/// Will be `None` or `Expr::Group(...)`
	pub params: Option<Expr>,
}

#[derive(Clone, DebugLisp)]
pub struct TypeDecl {
	pub annotator: Option<Token>,
	pub attributes: Option<AttributeList>,
	pub name: IdentExpr,
	pub child_ty: Option<Arc<TypeDecl>>,
	pub storage_class: Option<Token>,
	pub access_mode: Option<Token>,
	pub element_count: Option<Token>,
}

#[derive(Clone, DebugLisp)]
pub struct ArgumentList {
	pub brace_open: Token,
	pub arguments: Arc<[Expr]>,
	pub brace_close: Token,
}

impl Spanned for AttributeList {
	fn span(&self) -> Span {
		match self.attributes.len() {
			0 => Span::default(),
			1 => self.attributes.first().unwrap().span(),
			_ => self
				.attributes
				.first()
				.unwrap()
				.span()
				.through(self.attributes.last().unwrap().span()),
		}
	}
}

impl Parse for AttributeList {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
		let mut attributes = vec![];
		while input.check(punct!["@"]) {
			attributes.push(input.parse()?);
		}

		Ok(Self {
			attributes: attributes.into(),
		})
	}
}

impl Spanned for Attribute {
	fn span(&self) -> Span {
		if let Some(ref params) = self.params {
			self.at_sign.span().through(params.span())
		} else {
			self.at_sign.span().through(self.name.span())
		}
	}
}

impl Parse for Attribute {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
		let at_sign = input.consume(punct!["@"])?;
		let name = input.consume_as(TokenKind::Ident, Token::attribute)?;
		let params = if input.check(brace!["("]) {
			Some(Expr::Group(input.parse::<GroupExpr>()?))
		} else {
			None
		};

		Ok(Self {
			at_sign,
			name,
			params,
		})
	}
}

#[derive(Default)]
struct TypeDeclBuilder {
	attributes: Option<AttributeList>,
	annotator: Option<Token>,
	name: Option<IdentExpr>,
	child_ty: Option<Arc<TypeDecl>>,
	storage_class: Option<Token>,
	access_mode: Option<Token>,
	element_count: Option<Token>,
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
	fn child_ty(&mut self, child_ty: TypeDecl) -> &mut Self {
		self.child_ty = Some(Arc::new(child_ty));
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
	fn element_count(&mut self, element_count: Token) -> &mut Self {
		self.element_count = Some(element_count);
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
			element_count: self.element_count,
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
		if input.check(punct!["@"]) {
			builder.attributes(input.parse()?);
		}
		if input.check_kind(TokenKind::Type) {
			let name = input.next().unwrap();

			builder.name(IdentExpr {
				namespace: None,
				name: name.clone(),
			});

			if input.check(operator![<]) {
				input.consume(operator![<])?;

				while !input.check(operator![>]) {
					#[rustfmt::skip]
					match input.peek() {
						Some(token) => match token.as_matchable() {
							(Type | Ident, _, _) => {
								builder.child_ty(input.parse()?);
							}
							(Keyword, "function" | "private" | "workgroup" | "uniform" | "storage", _) => {
								builder.storage_class(input.next().unwrap());
							}
							(Keyword, "read" | "write" | "read_write", _) => {
								builder.access_mode(input.next().unwrap());
							}
							(Punct, ",", _) => {
								input.discard();
							},
							(UintLiteral, _, _) if name.lexeme().as_str() == "array" => {
								builder.element_count(input.next().unwrap());
							}
							(_, _, span) => {
								return Err(SpannedError {
									message: "Expected type, storage class, access mode, texel format, or element count"
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
