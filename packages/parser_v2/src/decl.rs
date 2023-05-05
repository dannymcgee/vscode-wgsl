use std::sync::Arc;

use gramatika::{Parse, ParseStreamer, Span, Spanned, SpannedError, Substr, Token as _};
use lazy_static::lazy_static;

use crate::{
	common::{AttributeList, TypeDecl},
	expr::Expr,
	stmt::BlockStmt,
	ParseStream, Token, TokenKind, *,
};

#[derive(Clone, DebugLisp)]
pub enum Decl {
	Var(VarDecl),
	Const(VarDecl),
	TypeAlias(TypeAliasDecl),
	Struct(StructDecl),
	Field(FieldDecl),
	Function(FunctionDecl),
	Param(ParamDecl),
	Extension(ExtensionDecl),
	Module(ModuleDecl),
	Error(SpannedError),
}

#[derive(Clone, DebugLisp)]
pub struct VarDecl {
	pub attributes: Option<AttributeList>,
	pub storage: Token,
	pub storage_qualifiers: Option<Arc<[Token]>>,
	pub name: Token,
	pub ty: Option<TypeDecl>,
	pub assignment: Option<Expr>,
	pub semicolon: Token,
}

#[derive(Clone, DebugLisp)]
pub struct TypeAliasDecl {
	pub storage: Token,
	pub name: Token,
	pub eq: Token,
	pub value: TypeDecl,
	pub semicolon: Token,
}

#[derive(Clone, DebugLisp)]
pub struct StructDecl {
	pub attributes: Option<AttributeList>,
	pub storage: Token,
	pub storage_modifier: Option<Token>,
	pub name: Token,
	pub body: StructBody,
	pub semicolon: Token,
}

#[derive(Clone, DebugLisp)]
pub struct StructBody {
	pub open_brace: Token,
	pub fields: Arc<[Decl]>,
	pub close_brace: Token,
}

#[derive(Clone, DebugLisp)]
pub struct FieldDecl {
	pub attributes: Option<AttributeList>,
	pub name: Token,
	pub ty: TypeDecl,
	pub semicolon: Token,
}

#[derive(Clone, DebugLisp)]
pub struct FunctionDecl {
	pub attributes: Option<AttributeList>,
	pub storage: Token,
	pub storage_modifier: Option<Token>,
	pub name: Token,
	pub params: Arc<[Decl]>,
	pub return_ty: Option<TypeDecl>,
	pub body: BlockStmt,
}

#[derive(Clone, DebugLisp)]
pub struct ParamDecl {
	pub attributes: Option<AttributeList>,
	pub name: Token,
	pub ty: TypeDecl,
}

#[derive(Clone, DebugLisp)]
pub struct ExtensionDecl {
	pub keyword: Token,
	pub name: Token,
}

#[derive(Clone, DebugLisp)]
pub struct ModuleDecl {
	pub import_kw: Token,
	pub name: Token,
	pub from_kw: Token,
	pub path: Token,
	pub semicolon: Token,
}

lazy_static! {
	// FIXME: This is gross -- is there a better way to handle this?
	static ref ERR_TOKEN: Token = Token::Ident(literal_substr!("ERROR"), span!(0:0...0:0));
}

impl Decl {
	pub fn name(&self) -> &Token {
		match self {
			Decl::Var(decl) | Decl::Const(decl) => &decl.name,
			Decl::TypeAlias(decl) => &decl.name,
			Decl::Struct(decl) => &decl.name,
			Decl::Field(decl) => &decl.name,
			Decl::Function(decl) => &decl.name,
			Decl::Param(decl) => &decl.name,
			Decl::Extension(decl) => &decl.name,
			Decl::Module(decl) => &decl.name,
			Decl::Error(_) => &ERR_TOKEN,
		}
	}

	pub fn attributes(&self) -> Option<&AttributeList> {
		match self {
			Decl::Var(decl) => decl.attributes.as_ref(),
			Decl::Const(decl) => decl.attributes.as_ref(),
			Decl::TypeAlias(_) => None,
			Decl::Struct(decl) => decl.attributes.as_ref(),
			Decl::Field(decl) => decl.attributes.as_ref(),
			Decl::Function(decl) => decl.attributes.as_ref(),
			Decl::Param(decl) => decl.attributes.as_ref(),
			Decl::Extension(_) => None,
			Decl::Module(_) => None,
			Decl::Error(_) => None,
		}
	}
}

impl Parse for Decl {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
		use TokenKind::*;

		match input.peek() {
			Some(token) => match token.as_matchable() {
				(Punct, "@", _) => {
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
				(Keyword, "var", _) => Ok(Decl::Var(input.parse()?)),
				(Keyword, "let", _) => Ok(Decl::Const(input.parse()?)),
				(Keyword, "type", _) => Ok(Decl::TypeAlias(input.parse()?)),
				(Keyword, "struct", _) => Ok(Decl::Struct(input.parse()?)),
				(Keyword, "fn", _) => Ok(Decl::Function(input.parse()?)),
				(Keyword, "enable", _) => Ok(Decl::Extension(input.parse()?)),
				(Keyword, "import", _) => Ok(Decl::Module(input.parse()?)),
				(_, _, span) => Err(SpannedError {
					message: "Expected `var`, `let`, `type`, `struct`, `fn`, `enable`, or `import`"
						.into(),
					span: Some(span),
					source: input.source(),
				}),
			},
			None => Err(SpannedError {
				message: "Unexpected end of input".into(),
				source: input.source(),
				span: None,
			}),
		}
	}
}

impl Spanned for Decl {
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
			Error(inner) => inner.span.unwrap_or_default(),
		}
	}
}

impl Parse for VarDecl {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
		let storage = input.consume_kind(TokenKind::Keyword)?;
		assert!(matches!(
			storage.as_matchable(),
			(TokenKind::Keyword, "let" | "var", _)
		));

		let storage_qualifiers = if input.check(operator![<]) {
			input.consume(operator![<])?;
			let mut qual = vec![input.consume_kind(TokenKind::Keyword)?];
			if input.check(punct![,]) {
				input.consume(punct![,])?;
				qual.push(input.consume_kind(TokenKind::Keyword)?);
			}
			input.consume(operator![>])?;
			Some(qual.into())
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

impl Spanned for VarDecl {
	fn span(&self) -> Span {
		self.storage.span().through(self.semicolon.span())
	}
}

impl Parse for TypeAliasDecl {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
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

impl Spanned for TypeAliasDecl {
	fn span(&self) -> Span {
		self.storage.span().through(self.semicolon.span())
	}
}

impl Parse for StructDecl {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
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

impl Spanned for StructDecl {
	fn span(&self) -> Span {
		self.storage.span().through(self.semicolon.span())
	}
}

impl Parse for StructBody {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
		use TokenKind::*;

		let open_brace = input.consume(brace!["{"])?;

		let mut fields = vec![];
		while !input.check(brace!["}"]) {
			match input.peek() {
				Some(token) => match token.as_matchable() {
					(Punct, "@", _) | (Ident, _, _) => {
						let field = input.parse::<FieldDecl>()?;
						fields.push(Decl::Field(field));
					}
					(_, _, span) => {
						return Err(SpannedError {
							message: "Expected field declaration".into(),
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

		let close_brace = input.consume(brace!["}"])?;

		Ok(Self {
			open_brace,
			fields: fields.into(),
			close_brace,
		})
	}
}

impl Spanned for StructBody {
	fn span(&self) -> Span {
		self.open_brace.span().through(self.close_brace.span())
	}
}

impl Parse for FieldDecl {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
		let attributes = if input.check(punct!["@"]) {
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

impl Spanned for FieldDecl {
	fn span(&self) -> Span {
		let span_start = self
			.attributes
			.as_ref()
			.and_then(|attrs| attrs.attributes.first().map(|attr| &attr.at_sign))
			.unwrap_or(&self.name)
			.span();

		span_start.through(self.semicolon.span())
	}
}

impl Parse for FunctionDecl {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
		use TokenKind::*;

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
				Some(token) => match token.as_matchable() {
					(Punct, "@", _) | (Ident, _, _) => {
						let param = input.parse::<ParamDecl>()?;
						params.push(Decl::Param(param));
					}
					(Punct, ",", _) => input.discard(),
					(_, _, span) => {
						return Err(SpannedError {
							message: "Expected parameter, `,`, or `)`".into(),
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
			params: params.into(),
			return_ty,
			body,
		})
	}
}

impl Spanned for FunctionDecl {
	fn span(&self) -> Span {
		self.storage.span().through(self.body.brace_close.span())
	}
}

impl Parse for ParamDecl {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
		let attributes = if input.check(punct!["@"]) {
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

impl Spanned for ParamDecl {
	fn span(&self) -> Span {
		let span_start = self
			.attributes
			.as_ref()
			.and_then(|attrs| attrs.attributes.first().map(|attr| &attr.at_sign))
			.unwrap_or(&self.name)
			.span();

		span_start.through(self.ty.span())
	}
}

impl Parse for ExtensionDecl {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
		let keyword = input.consume(keyword![enable])?;
		let name = input.consume_as(TokenKind::Ident, Token::Module)?;
		input.consume(punct![;])?;

		Ok(Self { keyword, name })
	}
}

impl Spanned for ExtensionDecl {
	fn span(&self) -> Span {
		self.keyword.span().through(self.name.span())
	}
}

impl ModuleDecl {
	pub fn path(&self) -> Substr {
		let path = self.path.lexeme();

		path.substr(1..path.len() - 1)
	}
}

impl Parse for ModuleDecl {
	type Stream = ParseStream;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<Self> {
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

impl Spanned for ModuleDecl {
	fn span(&self) -> Span {
		self.import_kw.span().through(self.semicolon.span())
	}
}
