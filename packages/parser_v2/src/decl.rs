use gramatika::{Parse, ParseStreamer, Span, Spanned, SpannedError};

use crate::{
	common::{AttributeList, TypeDecl},
	expr::Expr,
	stmt::BlockStmt,
	ParseStream, Token, TokenKind, *,
};

#[derive(Clone, DebugLisp)]
pub enum Decl<'a> {
	Var(VarDecl<'a>),
	Const(VarDecl<'a>),
	TypeAlias(TypeAliasDecl<'a>),
	Struct(StructDecl<'a>),
	Field(FieldDecl<'a>),
	Function(FunctionDecl<'a>),
	Param(ParamDecl<'a>),
	Extension(ExtensionDecl<'a>),
	Module(ModuleDecl<'a>),
}

#[derive(Clone, DebugLisp)]
pub struct VarDecl<'a> {
	pub attributes: Option<AttributeList<'a>>,
	pub storage: Token<'a>,
	pub storage_qualifiers: Option<Vec<Token<'a>>>,
	pub name: Token<'a>,
	pub ty: Option<TypeDecl<'a>>,
	pub assignment: Option<Expr<'a>>,
	pub semicolon: Token<'a>,
}

#[derive(Clone, DebugLisp)]
pub struct TypeAliasDecl<'a> {
	pub storage: Token<'a>,
	pub name: Token<'a>,
	pub eq: Token<'a>,
	pub value: TypeDecl<'a>,
	pub semicolon: Token<'a>,
}

#[derive(Clone, DebugLisp)]
pub struct StructDecl<'a> {
	pub attributes: Option<AttributeList<'a>>,
	pub storage: Token<'a>,
	pub storage_modifier: Option<Token<'a>>,
	pub name: Token<'a>,
	pub body: StructBody<'a>,
	pub semicolon: Token<'a>,
}

#[derive(Clone, DebugLisp)]
pub struct StructBody<'a> {
	pub open_brace: Token<'a>,
	pub fields: Vec<Decl<'a>>,
	pub close_brace: Token<'a>,
}

#[derive(Clone, DebugLisp)]
pub struct FieldDecl<'a> {
	pub attributes: Option<AttributeList<'a>>,
	pub name: Token<'a>,
	pub ty: TypeDecl<'a>,
	pub semicolon: Token<'a>,
}

#[derive(Clone, DebugLisp)]
pub struct FunctionDecl<'a> {
	pub attributes: Option<AttributeList<'a>>,
	pub storage: Token<'a>,
	pub storage_modifier: Option<Token<'a>>,
	pub name: Token<'a>,
	pub params: Vec<Decl<'a>>,
	pub return_ty: Option<TypeDecl<'a>>,
	pub body: BlockStmt<'a>,
}

#[derive(Clone, DebugLisp)]
pub struct ParamDecl<'a> {
	pub attributes: Option<AttributeList<'a>>,
	pub name: Token<'a>,
	pub ty: TypeDecl<'a>,
}

#[derive(Clone, DebugLisp)]
pub struct ExtensionDecl<'a> {
	pub keyword: Token<'a>,
	pub name: Token<'a>,
}

#[derive(Clone, DebugLisp)]
pub struct ModuleDecl<'a> {
	pub import_kw: Token<'a>,
	pub name: Token<'a>,
	pub from_kw: Token<'a>,
	pub path: Token<'a>,
	pub semicolon: Token<'a>,
}

impl<'a> Decl<'a> {
	pub fn name(&self) -> Token<'a> {
		match self {
			Decl::Var(decl) | Decl::Const(decl) => decl.name,
			Decl::TypeAlias(decl) => decl.name,
			Decl::Struct(decl) => decl.name,
			Decl::Field(decl) => decl.name,
			Decl::Function(decl) => decl.name,
			Decl::Param(decl) => decl.name,
			Decl::Extension(decl) => decl.name,
			Decl::Module(decl) => decl.name,
		}
	}
}

impl<'a> Parse<'a> for Decl<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		use Token::*;

		match input.peek() {
			Some(Brace("[[", _)) => {
				let attributes = Some(input.parse::<AttributeList>()?);

				match input.parse::<Decl>()? {
					Decl::Var(mut inner) => {
						inner.attributes = attributes;
						Ok(Decl::Var(inner))
					}
					Decl::Const(mut inner) => {
						inner.attributes = attributes;
						Ok(Decl::Const(inner))
					}
					Decl::Struct(mut inner) => {
						inner.attributes = attributes;
						Ok(Decl::Struct(inner))
					}
					Decl::Function(mut inner) => {
						inner.attributes = attributes;
						Ok(Decl::Function(inner))
					}
					_ => Err(SpannedError {
						message: "Attributes are not valid in this position".into(),
						span: Some(attributes.unwrap().span()),
						source: input.source(),
					}),
				}
			}
			Some(Keyword("var", _)) => Ok(Decl::Var(input.parse()?)),
			Some(Keyword("let", _)) => Ok(Decl::Const(input.parse()?)),
			Some(Keyword("type", _)) => Ok(Decl::TypeAlias(input.parse()?)),
			Some(Keyword("struct", _)) => Ok(Decl::Struct(input.parse()?)),
			Some(Keyword("fn", _)) => Ok(Decl::Function(input.parse()?)),
			Some(Keyword("enable", _)) => Ok(Decl::Extension(input.parse()?)),
			Some(Keyword("import", _)) => Ok(Decl::Module(input.parse()?)),
			Some(other) => Err(SpannedError {
				message: "Expected `var`, `let`, `type`, `struct`, `fn`, `enable`, or `import`"
					.into(),
				span: Some(other.span()),
				source: input.source(),
			}),
			None => Err(SpannedError {
				message: "Unexpected end of input".into(),
				source: input.source(),
				span: None,
			}),
		}
	}
}

impl<'a> Spanned for Decl<'a> {
	fn span(&self) -> Span {
		use Decl::*;

		match self {
			Var(inner) => inner.span(),
			Const(inner) => inner.span(),
			TypeAlias(inner) => inner.span(),
			Struct(inner) => inner.span(),
			Field(inner) => inner.span(),
			Function(inner) => inner.span(),
			Param(inner) => inner.span(),
			Extension(inner) => inner.span(),
			Module(inner) => inner.span(),
		}
	}
}

impl<'a> Parse<'a> for VarDecl<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		let storage = input.consume_kind(TokenKind::Keyword)?;
		assert!(matches!(storage, Token::Keyword("let" | "var", _)));

		let storage_qualifiers = if input.check(operator![<]) {
			input.consume(operator![<])?;
			let mut qual = vec![input.consume_kind(TokenKind::Keyword)?];
			if input.check(punct![,]) {
				input.consume(punct![,])?;
				qual.push(input.consume_kind(TokenKind::Keyword)?);
			}
			input.consume(operator![>])?;
			Some(qual)
		} else {
			None
		};

		let name = input.consume_kind(TokenKind::Ident)?;
		let ty = if input.check(punct![:]) {
			Some(input.parse::<TypeDecl>()?)
		} else {
			None
		};

		let assignment = if input.check(operator![=]) {
			input.next().unwrap();

			Some(input.parse::<Expr>()?)
		} else {
			None
		};

		let semicolon = input.consume(punct![;])?;

		Ok(Self {
			attributes: None,
			storage,
			storage_qualifiers,
			name,
			ty,
			assignment,
			semicolon,
		})
	}
}

impl<'a> Spanned for VarDecl<'a> {
	fn span(&self) -> Span {
		self.storage.span().through(self.semicolon.span())
	}
}

impl<'a> Parse<'a> for TypeAliasDecl<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		let storage = input.consume(keyword![type])?;
		let name = input.consume_kind(TokenKind::Ident)?;
		let eq = input.consume(operator![=])?;
		let value = input.parse()?;
		let semicolon = input.consume(punct![;])?;

		Ok(Self {
			storage,
			name,
			eq,
			value,
			semicolon,
		})
	}
}

impl<'a> Spanned for TypeAliasDecl<'a> {
	fn span(&self) -> Span {
		self.storage.span().through(self.semicolon.span())
	}
}

impl<'a> Parse<'a> for StructDecl<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		let storage = input.consume(keyword![struct])?;

		let storage_modifier = if input.check(operator![<]) {
			input.consume(operator![<])?;
			let modifier = input.consume_kind(TokenKind::Keyword)?;
			input.consume(operator![>])?;

			Some(modifier)
		} else {
			None
		};

		let name = input.consume_kind(TokenKind::Ident)?;
		let body = input.parse()?;
		let semicolon = input.consume(punct![;])?;

		Ok(Self {
			attributes: None,
			storage,
			storage_modifier,
			name,
			body,
			semicolon,
		})
	}
}

impl<'a> Spanned for StructDecl<'a> {
	fn span(&self) -> Span {
		self.storage.span().through(self.semicolon.span())
	}
}

impl<'a> Parse<'a> for StructBody<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		use Token::*;

		let open_brace = input.consume(brace!["{"])?;

		let mut fields = vec![];
		while !input.check(brace!["}"]) {
			match input.peek() {
				Some(Brace("[[", _)) | Some(Ident(_, _)) => {
					let field = input.parse::<FieldDecl>()?;
					fields.push(Decl::Field(field));
				}
				Some(other) => {
					return Err(SpannedError {
						message: "Expected field declaration".into(),
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

		let close_brace = input.consume(brace!["}"])?;

		Ok(Self {
			open_brace,
			fields,
			close_brace,
		})
	}
}

impl<'a> Spanned for StructBody<'a> {
	fn span(&self) -> Span {
		self.open_brace.span().through(self.close_brace.span())
	}
}

impl<'a> Parse<'a> for FieldDecl<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		let attributes = if input.check(brace!["[["]) {
			Some(input.parse::<AttributeList>()?)
		} else {
			None
		};
		let name = input.consume_as(TokenKind::Ident, Token::field)?;
		let ty = input.parse()?;
		let semicolon = input.consume(punct![;])?;

		Ok(Self {
			attributes,
			name,
			ty,
			semicolon,
		})
	}
}

impl<'a> Spanned for FieldDecl<'a> {
	fn span(&self) -> Span {
		let span_start = self
			.attributes
			.as_ref()
			.map(|attr| attr.open_brace)
			.unwrap_or(self.name)
			.span();

		span_start.through(self.semicolon.span())
	}
}

impl<'a> Parse<'a> for FunctionDecl<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		use Token::*;

		let storage = input.consume(keyword![fn])?;
		let storage_modifier = if input.check(operator![<]) {
			input.consume(operator![<])?;
			let modifier = input.consume_kind(TokenKind::Keyword)?;
			input.consume(operator![>])?;

			Some(modifier)
		} else {
			None
		};
		let name = input.consume_as(TokenKind::Ident, Token::function)?;
		input.consume(brace!["("])?;

		let mut params = vec![];
		while !input.check(brace![")"]) {
			match input.peek() {
				Some(Brace("[[", _) | Ident(_, _)) => {
					let param = input.parse::<ParamDecl>()?;
					params.push(Decl::Param(param));
				}
				Some(Punct(",", _)) => input.discard(),
				Some(other) => {
					return Err(SpannedError {
						message: "Expected parameter, `,`, or `)`".into(),
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

		input.consume(brace![")"])?;
		let return_ty = if input.check(operator![->]) {
			Some(input.parse::<TypeDecl>()?)
		} else {
			None
		};

		let body = input.parse::<BlockStmt>()?;

		Ok(Self {
			attributes: None,
			storage,
			storage_modifier,
			name,
			params,
			return_ty,
			body,
		})
	}
}

impl<'a> Spanned for FunctionDecl<'a> {
	fn span(&self) -> Span {
		self.storage.span().through(self.body.brace_close.span())
	}
}

impl<'a> Parse<'a> for ParamDecl<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		let attributes = if input.check(brace!["[["]) {
			Some(input.parse::<AttributeList>()?)
		} else {
			None
		};
		let name = input.consume_kind(TokenKind::Ident)?;
		let ty = input.parse::<TypeDecl>()?;

		Ok(Self {
			attributes,
			name,
			ty,
		})
	}
}

impl<'a> Spanned for ParamDecl<'a> {
	fn span(&self) -> Span {
		let span_start = self
			.attributes
			.as_ref()
			.map(|attr| attr.open_brace)
			.unwrap_or(self.name)
			.span();

		span_start.through(self.ty.span())
	}
}

impl<'a> Parse<'a> for ExtensionDecl<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		let keyword = input.consume(keyword![enable])?;
		let name = input.consume_as(TokenKind::Ident, Token::Module)?;
		input.consume(punct![;])?;

		Ok(Self { keyword, name })
	}
}

impl<'a> Spanned for ExtensionDecl<'a> {
	fn span(&self) -> Span {
		self.keyword.span().through(self.name.span())
	}
}

impl<'a> Parse<'a> for ModuleDecl<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		let import_kw = input.consume(keyword![import])?;
		let name = input.consume_as(TokenKind::Ident, Token::Module)?;
		let from_kw = input.consume(keyword![from])?;
		let path = input.consume_kind(TokenKind::Path)?;
		let semicolon = input.consume(punct![;])?;

		Ok(Self {
			import_kw,
			name,
			from_kw,
			path,
			semicolon,
		})
	}
}

impl<'a> Spanned for ModuleDecl<'a> {
	fn span(&self) -> Span {
		self.import_kw.span().through(self.semicolon.span())
	}
}
