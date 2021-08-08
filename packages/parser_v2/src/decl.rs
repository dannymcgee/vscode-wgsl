use gramatika::{Parse, ParseStreamer, Span, SpannedError, Token as _};

use crate::{
	common::{AttributeList, TypeDecl},
	expr::Expr,
	ParseStream, Token, TokenKind, *,
};

#[derive(DebugLisp)]
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

#[derive(DebugLisp)]
pub struct VarDecl<'a> {
	pub attributes: Option<AttributeList<'a>>,
	pub storage: Token<'a>,
	pub storage_qualifiers: Option<Vec<Token<'a>>>,
	pub name: Token<'a>,
	pub ty: Option<TypeDecl<'a>>,
	pub assignment: Option<Expr<'a>>,
	pub semicolon: Token<'a>,
	pub span: Span,
}

#[derive(DebugLisp)]
pub struct TypeAliasDecl<'a> {
	pub storage: Token<'a>,
	pub name: Token<'a>,
	pub eq: Token<'a>,
	pub value: TypeDecl<'a>,
	pub span: Span,
}

#[derive(DebugLisp)]
pub struct StructDecl<'a> {
	pub attributes: Option<AttributeList<'a>>,
	pub storage: Token<'a>,
	pub storage_modifier: Option<Token<'a>>,
	pub name: Token<'a>,
	pub body: Vec<FieldDecl<'a>>,
	pub semicolon: Token<'a>,
	pub span: Span,
}

#[derive(DebugLisp)]
pub struct FieldDecl<'a> {
	pub attributes: Option<AttributeList<'a>>,
	pub name: Token<'a>,
	pub ty: TypeDecl<'a>,
	pub span: Span,
}

#[derive(DebugLisp)]
pub struct FunctionDecl<'a> {
	pub attributes: Option<AttributeList<'a>>,
	pub storage: Token<'a>,
	pub storage_modifier: Option<Token<'a>>,
	pub name: Token<'a>,
	pub params: Vec<ParamDecl<'a>>,
	pub return_ty: Option<TypeDecl<'a>>,
	pub span: Span,
}

#[derive(DebugLisp)]
pub struct ParamDecl<'a> {
	pub attributes: Option<AttributeList<'a>>,
	pub name: Token<'a>,
	pub ty: TypeDecl<'a>,
	pub span: Span,
}

#[derive(DebugLisp)]
pub struct ExtensionDecl<'a> {
	pub keyword: Token<'a>,
	pub name: Token<'a>,
	pub span: Span,
}

#[derive(DebugLisp)]
pub struct ModuleDecl<'a> {
	pub import_kw: Token<'a>,
	pub name: Token<'a>,
	pub from_kw: Token<'a>,
	pub path: Token<'a>,
	pub span: Span,
}

impl<'a> Parse<'a> for Decl<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		use Token::*;

		match input.peek() {
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
			Some(input.parse::<Expr>()?)
		} else {
			None
		};

		let semicolon = input.consume(punct![;])?;

		Ok(Self {
			attributes: None, // TOOD
			storage,
			storage_qualifiers,
			name,
			ty,
			assignment,
			semicolon,
			span: storage.span().through(semicolon.span()),
		})
	}
}

impl<'a> Parse<'a> for TypeAliasDecl<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		todo!()
	}
}

impl<'a> Parse<'a> for StructDecl<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		todo!()
	}
}

impl<'a> Parse<'a> for FunctionDecl<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		todo!()
	}
}

impl<'a> Parse<'a> for ExtensionDecl<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		todo!()
	}
}

impl<'a> Parse<'a> for ModuleDecl<'a> {
	type Stream = ParseStream<'a>;

	fn parse(input: &mut Self::Stream) -> gramatika::Result<'a, Self> {
		todo!()
	}
}
