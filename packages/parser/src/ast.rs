use std::fmt;

use itertools::Itertools;
use lsp_types::Range;

pub trait Pretty {
	fn pretty(&self) -> String;
}

impl Pretty for Range {
	fn pretty(&self) -> String {
		format!(
			"{}:{} .. {}:{}",
			self.start.line, self.start.character, self.end.line, self.end.character
		)
	}
}

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

impl fmt::Debug for Token {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let (name, inner, range) = match self {
			Self::Ident(inner, range) => ("Ident", inner, range),
			Self::Attr(inner, range) => ("Attr", inner, range),
			Self::Field(inner, range) => ("Field", inner, range),
			Self::Type(inner, range) => ("Type", inner, range),
			Self::Function(inner, range) => ("Function", inner, range),
			Self::Keyword(inner, range) => ("Keyword", inner, range),
			Self::Punct(inner, range) => ("Punct", inner, range),
			Self::Op(inner, range) => ("Op", inner, range),
			Self::Literal(inner, range) => ("Literal", inner, range),
			Self::Module(inner, range) => ("Module", inner, range),
		};

		write!(f, "`{}` | Token::{} [{}]", inner, name, range.pretty())
	}
}

impl fmt::Display for Token {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use Token::*;

		let text = match self {
			Ident(text, _) => text,
			Attr(text, _) => text,
			Field(text, _) => text,
			Type(text, _) => text,
			Function(text, _) => text,
			Keyword(text, _) => text,
			Punct(text, _) => text,
			Op(text, _) => text,
			Literal(text, _) => text,
			Module(text, _) => text,
		};

		write!(f, "{}", text)
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
		}
	}
}

impl fmt::Display for Decl {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use Decl::*;

		match self {
			Var(decl) | Const(decl) => write!(f, "{}", decl),
			TypeAlias(decl) => write!(f, "{}", decl),
			Struct(decl) => write!(f, "{}", decl),
			Field(decl) => write!(f, "{}", decl),
			Function(decl) => {
				if let Some(ref attributes) = decl.attributes {
					writeln!(f, "{}", attributes.pretty())?;
				}
				write!(f, "{} {}{}", decl.storage, decl.name, decl.signature)
			}
			Param(decl) => write!(f, "{}", decl),
			Extension(decl) => write!(f, "{}", decl),
		}
	}
}

#[derive(Builder, Clone)]
pub struct TypeDecl {
	#[builder(setter(into, strip_option), default)]
	pub attributes: Option<Vec<Attribute>>,
	pub name: Token,
	#[builder(setter(into, strip_option), default)]
	pub component_type: Option<Box<TypeDecl>>,
	#[builder(setter(into, strip_option), default)]
	pub storage_class: Option<Token>,
	#[builder(setter(into, strip_option), default)]
	pub access_mode: Option<Token>,
	pub range: Range,
}

impl fmt::Debug for TypeDecl {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let mut debug = f.debug_struct("TypeDecl");
		debug.field("range", &self.range.pretty());

		if let Some(ref attr) = self.attributes {
			debug.field("attributes", attr);
		}
		debug.field("name", &self.name);
		if let Some(ref ty) = self.component_type {
			debug.field("component_type", ty);
		}
		if let Some(ref sc) = self.storage_class {
			debug.field("storage_class", sc);
		}
		if let Some(ref am) = self.access_mode {
			debug.field("access_mode", am);
		}

		debug.finish()
	}
}

impl fmt::Display for TypeDecl {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		if let Some(ref attributes) = self.attributes {
			write!(f, "{} ", attributes.pretty())?;
		}

		write!(f, "{}", self.name)?;

		if self.storage_class.is_some()
			|| self.component_type.is_some()
			|| self.access_mode.is_some()
		{
			write!(f, "<")?;

			if let Some(ref storage_class) = self.storage_class {
				write!(f, "{}, ", storage_class)?;
			}
			if let Some(ref component_type) = self.component_type {
				write!(f, "{}", component_type)?;
			}
			if let Some(ref access_mode) = self.access_mode {
				write!(f, ", {}", access_mode)?;
			}

			write!(f, ">")?;
		}

		Ok(())
	}
}

#[derive(Builder, Clone)]
pub struct Attribute {
	pub name: Token,
	#[builder(setter(into, strip_option), default)]
	pub value: Option<Token>,
	pub range: Range,
}

impl fmt::Debug for Attribute {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let mut debug = f.debug_struct("Attribute");
		debug.field("range", &self.range.pretty());
		debug.field("name", &self.name);

		if let Some(ref value) = self.value {
			debug.field("value", value);
		}

		debug.finish()
	}
}

impl fmt::Display for Attribute {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.name)?;

		if let Some(ref value) = self.value {
			write!(f, "({})", value)?;
		}

		Ok(())
	}
}

impl Pretty for Vec<Attribute> {
	fn pretty(&self) -> String {
		format!(
			"[[{}]]",
			self.iter().map(|attr| attr.to_string()).join(", ")
		)
	}
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

impl fmt::Debug for VarDecl {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let mut debug = f.debug_struct("VarDecl");
		debug.field("range", &self.range.pretty());

		if let Some(ref attr) = self.attributes {
			debug.field("attributes", attr);
		}
		debug.field("storage", &self.storage);
		if let Some(ref qual) = self.storage_qualifiers {
			debug.field("storage_qualifiers", qual);
		}
		debug.field("name", &self.name);
		if let Some(ref ty) = self.type_decl {
			debug.field("type_decl", ty);
		}
		if let Some(ref assignment) = self.assignment {
			debug.field("assignment", assignment);
		}

		debug.finish()
	}
}

impl fmt::Display for VarDecl {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.storage)?;

		if let Some(ref qualifiers) = self.storage_qualifiers {
			write!(
				f,
				"<{}>",
				qualifiers.iter().map(|q| q.to_string()).join(", ")
			)?;
		}

		write!(f, " {}", self.name)?;

		if let Some(ref ty) = self.type_decl {
			write!(f, ": {}", ty)?;
		}

		Ok(())
	}
}

#[derive(Builder, Clone)]
pub struct TypeAliasDecl {
	pub storage: Token,
	pub name: Token,
	pub value: TypeDecl,
	pub range: Range,
}

impl fmt::Debug for TypeAliasDecl {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.debug_struct("TypeAliasDecl")
			.field("range", &self.range.pretty())
			.field("storage", &self.storage)
			.field("name", &self.name)
			.field("value", &self.value)
			.finish()
	}
}

impl fmt::Display for TypeAliasDecl {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{} {} = {}", self.storage, self.name, self.value)
	}
}

#[derive(Builder, Clone)]
pub struct StructDecl {
	#[builder(setter(into, strip_option), default)]
	pub attributes: Option<Vec<Attribute>>,
	pub storage: Token,
	pub name: Token,
	pub body: Vec<StructField>,
	pub range: Range,
}

impl fmt::Debug for StructDecl {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let mut debug = f.debug_struct("StructDecl");
		debug.field("range", &self.range.pretty());

		if let Some(ref attr) = self.attributes {
			debug.field("attributes", attr);
		}

		debug
			.field("storage", &self.storage)
			.field("name", &self.name)
			.field("body", &self.body)
			.finish()
	}
}

impl fmt::Display for StructDecl {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		if let Some(ref attributes) = self.attributes {
			writeln!(f, "{}", attributes.pretty())?;
		}

		writeln!(f, "{} {} {{", self.storage, self.name)?;

		for field in &self.body {
			writeln!(f, "    {};", field)?;
		}

		write!(f, "}};")
	}
}

#[derive(Builder, Clone)]
pub struct StructField {
	#[builder(setter(into, strip_option), default)]
	pub attributes: Option<Vec<Attribute>>,
	pub name: Token,
	pub type_decl: Box<TypeDecl>,
	pub range: Range,
}

impl fmt::Debug for StructField {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let mut debug = f.debug_struct("StructField");
		debug.field("range", &self.range.pretty());

		if let Some(ref attr) = self.attributes {
			debug.field("attributes", attr);
		}

		debug
			.field("name", &self.name)
			.field("type_decl", &self.type_decl)
			.finish()
	}
}

impl fmt::Display for StructField {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		if let Some(ref attributes) = self.attributes {
			write!(f, "{} ", attributes.pretty())?;
		}

		write!(f, "{}: {}", self.name, self.type_decl)
	}
}

#[derive(Builder, Clone)]
pub struct FunctionDecl {
	#[builder(setter(into, strip_option), default)]
	pub attributes: Option<Vec<Attribute>>,
	pub storage: Token,
	pub name: Token,
	pub signature: FunctionSignature,
	pub body: (Range, Vec<Stmt>),
	pub range: Range,
}

impl fmt::Debug for FunctionDecl {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let mut debug = f.debug_struct("FunctionDecl");
		debug.field("range", &self.range.pretty());

		if let Some(ref attr) = self.attributes {
			debug.field("attributes", attr);
		}

		debug
			.field("storage", &self.storage)
			.field("name", &self.name)
			.field("signature", &self.signature)
			.field("body", &self.body)
			.finish()
	}
}

#[derive(Builder, Clone)]
pub struct FunctionSignature {
	#[builder(default)]
	pub params: Vec<FunctionParam>,
	#[builder(setter(into, strip_option), default)]
	pub return_type: Option<Box<TypeDecl>>,
	pub range: Range,
}

impl fmt::Debug for FunctionSignature {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let mut debug = f.debug_struct("FunctionSignature");
		debug.field("range", &self.range.pretty());
		debug.field("params", &self.params);

		if let Some(ref ty) = self.return_type {
			debug.field("return_type", &ty);
		}

		debug.finish()
	}
}

impl fmt::Display for FunctionSignature {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(
			f,
			"({})",
			self.params.iter().map(|p| p.to_string()).join(", ")
		)?;

		if let Some(ref ty) = self.return_type {
			write!(f, " -> {}", ty)?;
		}

		Ok(())
	}
}

#[derive(Builder, Clone)]
pub struct FunctionParam {
	#[builder(setter(into, strip_option), default)]
	pub attributes: Option<Vec<Attribute>>,
	pub name: Token,
	pub type_decl: Box<TypeDecl>,
	pub range: Range,
}

impl fmt::Debug for FunctionParam {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let mut debug = f.debug_struct("FunctionParam");
		debug.field("range", &self.range.pretty());

		if let Some(ref attr) = self.attributes {
			debug.field("attributes", attr);
		}

		debug
			.field("name", &self.name)
			.field("type_decl", &self.type_decl)
			.finish()
	}
}

impl fmt::Display for FunctionParam {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		if let Some(ref attributes) = self.attributes {
			write!(f, "{} ", attributes.pretty())?;
		}

		write!(f, "{}: {}", self.name, self.type_decl)
	}
}

#[derive(Builder, Clone)]
pub struct ExtensionDecl {
	pub keyword: Token,
	pub name: Token,
	pub range: Range,
}

impl fmt::Debug for ExtensionDecl {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.debug_struct("ExtensionDecl")
			.field("keyword", &self.keyword)
			.field("name", &self.name)
			.field("range", &self.range.pretty())
			.finish()
	}
}

impl fmt::Display for ExtensionDecl {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{} {};", self.keyword, self.name)
	}
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

impl fmt::Debug for Stmt {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Return(inner) => write!(f, "Stmt::Return::{:#?}", inner),
			Self::If(inner) => write!(f, "Stmt::If::{:#?}", inner),
			Self::Switch(inner) => write!(f, "Stmt::Switch::{:#?}", inner),
			Self::Loop(inner) => write!(f, "Stmt::Loop::{:#?}", inner),
			Self::Continuing(inner) => write!(f, "Stmt::Continuing::{:#?}", inner),
			Self::For(inner) => write!(f, "Stmt::For::{:#?}", inner),
			Self::FunctionCall(inner) => write!(f, "Stmt::FunctionCall::{:#?}", inner),
			Self::Variable(inner) => write!(f, "Stmt::Variable::{:#?}", inner),
			Self::Break(inner) => write!(f, "Stmt::Break( {:#?} )", inner),
			Self::Continue(inner) => write!(f, "Stmt::Continue( {:#?} )", inner),
			Self::Discard(inner) => write!(f, "Stmt::Discard( {:#?} )", inner),
			Self::Fallthrough(inner) => write!(f, "Stmt::Fallthrough( {:#?} )", inner),
			Self::Assignment(inner) => write!(f, "Stmt::Assignment::{:#?}", inner),
		}
	}
}

#[derive(Builder, Clone)]
pub struct ReturnStmt {
	pub keyword: Token,
	#[builder(setter(into, strip_option), default)]
	pub expr: Option<Box<Expr>>,
	pub range: Range,
}

impl fmt::Debug for ReturnStmt {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let mut debug = f.debug_struct("ReturnStmt");
		debug.field("range", &self.range.pretty());
		debug.field("keyword", &self.keyword);
		if let Some(ref expr) = self.expr {
			debug.field("expr", expr);
		}

		debug.finish()
	}
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
	pub iterator: Option<Box<Stmt>>,
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

impl fmt::Debug for Expr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Singular(inner) => write!(f, "Expr::Singular::{:#?}", inner),
			Self::Binary(inner) => write!(f, "Expr::Binary::{:#?}", inner),
		}
	}
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

impl fmt::Debug for PrimaryExpr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::TypeCtor(inner) => write!(f, "PrimaryExpr::TypeCtor::{:#?}", inner),
			Self::Literal(inner) => write!(f, "PrimaryExpr::Literal( {:#?} )", inner),
			Self::Paren(inner) => write!(f, "PrimaryExpr::Paren::{:#?}", inner),
			Self::Bitcast(inner) => write!(f, "PrimaryExpr::Bitcast::{:#?}", inner),
			Self::Identifier(inner) => write!(f, "PrimaryExpr::Identifier( {:#?} )", inner),
			Self::FunctionCall(inner) => write!(f, "PrimaryExpr::FunctionCall::{:#?}", inner),
		}
	}
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

impl fmt::Debug for SingularExpr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let mut debug = f.debug_struct("SingularExpr");
		debug.field("range", &self.range.pretty());

		if let Some(ref pre) = self.prefix {
			debug.field("prefix", pre);
		}
		debug.field("expr", &self.expr);
		if let Some(ref post) = self.postfix {
			debug.field("postfix", post);
		}

		debug.finish()
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
