use std::fmt;

use itertools::Itertools;
use lsp_types::Range;

mod debug;
mod display;
pub use debug::*;
pub use display::*;

#[derive(Clone)]
pub enum Token {
	Ident(String, Range),
	Attr(String, Range),
	Field(String, Range),
	Type(String, Range),
	Function(String, Range),
	Keyword(String, Range),
	Punct(String, Range),
	Op(String, Range),
	Literal(String, Range),
	Module(String, Range),
}

impl Token {
	pub fn into_inner(self) -> (String, Range) {
		use Token::*;

		match self {
			Ident(text, range) => (text, range),
			Attr(text, range) => (text, range),
			Field(text, range) => (text, range),
			Type(text, range) => (text, range),
			Function(text, range) => (text, range),
			Keyword(text, range) => (text, range),
			Punct(text, range) => (text, range),
			Op(text, range) => (text, range),
			Literal(text, range) => (text, range),
			Module(text, range) => (text, range),
		}
	}

	pub fn borrow_inner(&self) -> (&str, Range) {
		use Token::*;

		match self {
			Ident(text, range) => (text, *range),
			Attr(text, range) => (text, *range),
			Field(text, range) => (text, *range),
			Type(text, range) => (text, *range),
			Function(text, range) => (text, *range),
			Keyword(text, range) => (text, *range),
			Punct(text, range) => (text, *range),
			Op(text, range) => (text, *range),
			Literal(text, range) => (text, *range),
			Module(text, range) => (text, *range),
		}
	}
}

#[derive(Clone, Debug)]
pub enum Decl {
	Var(VarDecl),
	Const(VarDecl),
	TypeAlias(TypeAliasDecl),
	Struct(StructDecl),
	Field(StructField),
	Function(FunctionDecl),
	Param(FunctionParam),
	Extension(ExtensionDecl),
	Module(ModuleDecl),
}

impl Decl {
	pub fn ident(&self) -> &Token {
		use Decl::*;

		match self {
			Var(ref inner) | Const(ref inner) => &inner.name,
			TypeAlias(ref inner) => &inner.name,
			Struct(ref inner) => &inner.name,
			Field(ref inner) => &inner.name,
			Function(ref inner) => &inner.name,
			Param(ref inner) => &inner.name,
			Extension(ref inner) => &inner.name,
			Module(ref inner) => &inner.name,
		}
	}
}

#[derive(Builder, Clone)]
pub struct TypeDecl {
	#[builder(setter(into, strip_option), default)]
	pub attributes: Option<Vec<Attribute>>,
	#[builder(setter(into, strip_option), default)]
	pub namespace: Option<Token>,
	pub name: Token,
	#[builder(setter(into, strip_option), default)]
	pub component_type: Option<Box<TypeDecl>>,
	#[builder(setter(into, strip_option), default)]
	pub storage_class: Option<Token>,
	#[builder(setter(into, strip_option), default)]
	pub access_mode: Option<Token>,
	pub range: Range,
}

#[derive(Builder, Clone)]
pub struct Attribute {
	pub name: Token,
	#[builder(setter(into, strip_option), default)]
	pub value: Option<Token>,
	pub range: Range,
}

#[derive(Builder, Clone)]
pub struct VarDecl {
	#[builder(setter(into, strip_option), default)]
	pub attributes: Option<Vec<Attribute>>,
	pub storage: Token,
	#[builder(setter(into, strip_option), default)]
	pub storage_qualifiers: Option<Vec<Token>>,
	pub name: Token,
	#[builder(setter(into, strip_option), default)]
	pub type_decl: Option<Box<TypeDecl>>,
	#[builder(setter(into, strip_option), default)]
	pub assignment: Option<Box<Expr>>,
	pub range: Range,
}

#[derive(Builder, Clone)]
pub struct TypeAliasDecl {
	pub storage: Token,
	pub name: Token,
	pub value: TypeDecl,
	pub range: Range,
}

#[derive(Builder, Clone)]
pub struct StructDecl {
	#[builder(setter(into, strip_option), default)]
	pub attributes: Option<Vec<Attribute>>,
	pub storage: Token,
	#[builder(setter(into, strip_option), default)]
	pub storage_modifier: Option<Token>,
	pub name: Token,
	pub body: Vec<StructField>,
	pub range: Range,
}

#[derive(Builder, Clone)]
pub struct StructField {
	#[builder(setter(into, strip_option), default)]
	pub attributes: Option<Vec<Attribute>>,
	pub name: Token,
	pub type_decl: Box<TypeDecl>,
	pub range: Range,
}

#[derive(Builder, Clone)]
pub struct FunctionDecl {
	#[builder(setter(into, strip_option), default)]
	pub attributes: Option<Vec<Attribute>>,
	pub storage: Token,
	#[builder(setter(into, strip_option), default)]
	pub storage_modifier: Option<Token>,
	pub name: Token,
	pub signature: FunctionSignature,
	pub body: (Range, Vec<Stmt>),
	pub range: Range,
}

#[derive(Builder, Clone)]
pub struct FunctionSignature {
	#[builder(default)]
	pub params: Vec<FunctionParam>,
	#[builder(setter(into, strip_option), default)]
	pub return_type: Option<Box<TypeDecl>>,
	pub range: Range,
}

#[derive(Builder, Clone)]
pub struct FunctionParam {
	#[builder(setter(into, strip_option), default)]
	pub attributes: Option<Vec<Attribute>>,
	pub name: Token,
	pub type_decl: Box<TypeDecl>,
	pub range: Range,
}

#[derive(Builder, Clone)]
pub struct ExtensionDecl {
	pub keyword: Token,
	pub name: Token,
	pub range: Range,
}

#[derive(Builder, Clone)]
pub struct ModuleDecl {
	pub import_keyword: Token,
	pub name: Token,
	pub from_keyword: Token,
	pub path: Token,
	pub range: Range,
}

#[derive(Clone)]
pub enum Stmt {
	Return(ReturnStmt),
	If(Box<IfStmt>),
	Switch(SwitchStmt),
	Loop(LoopStmt),
	Continuing(ContinuingStmt),
	For(ForStmt),
	FunctionCall(FunctionCallExpr),
	Variable(VarDecl),
	Break(Token),
	Continue(Token),
	Discard(Token),
	Fallthrough(Token),
	Assignment(AssignmentStmt),
}

#[derive(Builder, Clone)]
pub struct ReturnStmt {
	pub keyword: Token,
	#[builder(setter(into, strip_option), default)]
	pub expr: Option<Box<Expr>>,
	pub range: Range,
}

#[derive(Builder, Clone, Debug)]
pub struct IfStmt {
	pub keyword: Token,
	pub condition: Box<Expr>,
	pub body: Vec<Stmt>,
	#[builder(setter(into, strip_option), default)]
	pub elseif: Option<ElseifStmt>,
	#[builder(setter(into, strip_option), default)]
	pub else_stmt: Option<ElseStmt>,
	pub range: Range,
}

#[derive(Builder, Clone, Debug)]
pub struct ElseifStmt {
	pub keyword: Token,
	pub condition: Box<Expr>,
	pub body: Vec<Stmt>,
	#[builder(setter(into, strip_option), default)]
	pub elseif: Option<Box<ElseifStmt>>,
	pub range: Range,
}

#[derive(Builder, Clone, Debug)]
pub struct ElseStmt {
	pub keyword: Token,
	pub body: Vec<Stmt>,
	pub range: Range,
}

#[derive(Builder, Clone, Debug)]
pub struct SwitchStmt {
	pub keyword: Token,
	pub subject: Box<Expr>,
	pub body: Vec<CaseStmt>,
	pub range: Range,
}

#[derive(Builder, Clone, Debug)]
pub struct CaseStmt {
	pub keyword: Token,
	/// Option is None for `default` case
	#[builder(setter(into, strip_option), default)]
	pub selectors: Option<Vec<Token>>,
	pub body: Vec<Stmt>,
	pub range: Range,
}

#[derive(Builder, Clone, Debug)]
pub struct LoopStmt {
	pub keyword: Token,
	pub body: Vec<Stmt>,
	#[builder(setter(into, strip_option), default)]
	pub continuing: Option<Box<ContinuingStmt>>,
	pub range: Range,
}

#[derive(Builder, Clone, Debug)]
pub struct ContinuingStmt {
	pub keyword: Token,
	pub body: Vec<Stmt>,
	pub range: Range,
}

#[derive(Builder, Clone, Debug)]
pub struct ForStmt {
	pub keyword: Token,
	pub header: ForHeader,
	pub body: Vec<Stmt>,
	pub range: Range,
}

#[derive(Builder, Clone, Debug)]
pub struct ForHeader {
	#[builder(setter(into, strip_option), default)]
	pub init: Option<Box<Stmt>>,
	#[builder(setter(into, strip_option), default)]
	pub condition: Option<Box<Expr>>,
	#[builder(setter(into, strip_option), default)]
	pub iterator: Option<Box<Expr>>,
	pub range: Range,
}

#[derive(Builder, Clone, Debug)]
pub struct AssignmentStmt {
	pub lhs: Box<SingularExpr>,
	pub op: Token,
	pub rhs: Box<Expr>,
	pub range: Range,
}

#[derive(Clone)]
pub enum Expr {
	Singular(SingularExpr),
	Binary(BinaryExpr),
}

#[derive(Clone)]
pub enum PrimaryExpr {
	TypeCtor(TypeCtorExpr),
	Literal(Token),
	Paren(Box<Expr>),
	Bitcast(BitcastExpr),
	Identifier(Token),
	FunctionCall(FunctionCallExpr),
}

#[derive(Clone, Debug)]
pub enum PostfixExpr {
	Bracket {
		expr: Box<Expr>,
		postfix: Option<Box<PostfixExpr>>,
		range: Range,
	},
	Dot {
		ident: Token,
		postfix: Option<Box<PostfixExpr>>,
		range: Range,
	},
}

#[derive(Builder, Clone)]
pub struct SingularExpr {
	#[builder(setter(into, strip_option), default)]
	pub prefix: Option<Vec<Token>>,
	pub expr: PrimaryExpr,
	#[builder(setter(into, strip_option), default)]
	pub postfix: Option<PostfixExpr>,
	pub range: Range,
}

impl SingularExpr {
	pub fn new(expr: PrimaryExpr, range: Range) -> Self {
		Self {
			prefix: None,
			expr,
			postfix: None,
			range,
		}
	}
}

#[derive(Builder, Clone, Debug)]
pub struct BinaryExpr {
	pub lhs: Box<SingularExpr>,
	pub op: Token,
	pub rhs: Box<Expr>,
	pub range: Range,
}

#[derive(Builder, Clone, Debug)]
pub struct TypeCtorExpr {
	pub ty: Box<TypeDecl>,
	pub args: Vec<Expr>,
	pub range: Range,
}

#[derive(Builder, Clone, Debug)]
pub struct BitcastExpr {
	pub keyword: Token,
	pub ty: Box<TypeDecl>,
	pub expr: Box<Expr>,
	pub range: Range,
}

#[derive(Builder, Clone, Debug)]
pub struct FunctionCallExpr {
	pub ident: Token,
	pub args: Vec<Expr>,
	pub range: Range,
}
