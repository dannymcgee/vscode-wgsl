use lsp_types::Range;

#[derive(Clone, Debug)]
pub enum AstNode {
	Decl(Decl),
	Stmt(Stmt),
	Expr(Expr),
}

#[derive(Clone, Debug)]
pub enum Token {
	Ident(String, Range),
	Keyword(String, Range),
	Punct(String, Range),
	Op(String, Range),
	Literal(String, Range),
}

#[derive(Clone, Debug)]
pub enum Decl {
	Var(VarDecl),
	Const(VarDecl),
	TypeAlias(TypeAliasDecl),
	Struct(StructDecl),
	Function(FunctionDecl),
}

#[derive(Builder, Clone, Debug)]
pub struct TypeDecl {
	#[builder(setter(into, strip_option), default)]
	attributes: Option<Vec<Attribute>>,
	name: Token,
	#[builder(setter(into, strip_option), default)]
	component_type: Option<Box<TypeDecl>>,
	#[builder(setter(into, strip_option), default)]
	storage_class: Option<Token>,
	#[builder(setter(into, strip_option), default)]
	access_mode: Option<Token>,
	range: Range,
}

#[derive(Builder, Clone, Debug)]
pub struct Attribute {
	name: Token,
	#[builder(setter(into, strip_option), default)]
	value: Option<Token>,
	range: Range,
}

#[derive(Builder, Clone, Debug)]
pub struct VarDecl {
	#[builder(setter(into, strip_option), default)]
	attributes: Option<Vec<Attribute>>,
	storage: Token,
	#[builder(setter(into, strip_option), default)]
	storage_qualifiers: Option<Vec<Token>>,
	name: Token,
	#[builder(setter(into, strip_option), default)]
	type_decl: Option<Box<TypeDecl>>,
	#[builder(setter(into, strip_option), default)]
	assignment: Option<Box<Expr>>,
	range: Range,
}

#[derive(Builder, Clone, Debug)]
pub struct TypeAliasDecl {
	storage: Token,
	name: Token,
	value: TypeDecl,
	range: Range,
}

#[derive(Builder, Clone, Debug)]
pub struct StructDecl {
	#[builder(setter(into, strip_option), default)]
	attributes: Option<Vec<Attribute>>,
	storage: Token,
	name: Token,
	body: Vec<StructField>,
	range: Range,
}

#[derive(Builder, Clone, Debug)]
pub struct StructField {
	#[builder(setter(into, strip_option), default)]
	attributes: Option<Vec<Attribute>>,
	name: Token,
	type_decl: Box<TypeDecl>,
	range: Range,
}

#[derive(Builder, Clone, Debug)]
pub struct FunctionDecl {
	#[builder(setter(into, strip_option), default)]
	attributes: Option<Vec<Attribute>>,
	storage: Token,
	name: Token,
	signature: FunctionSignature,
	body: Vec<Stmt>,
	range: Range,
}

#[derive(Builder, Clone, Debug)]
pub struct FunctionSignature {
	params: Vec<FunctionParam>,
	#[builder(setter(into, strip_option), default)]
	return_type: Option<Box<TypeDecl>>,
	range: Range,
}

#[derive(Builder, Clone, Debug)]
pub struct FunctionParam {
	#[builder(setter(into, strip_option), default)]
	attributes: Option<Vec<Attribute>>,
	name: Token,
	type_decl: Box<TypeDecl>,
	range: Range,
}

#[derive(Clone, Debug)]
pub enum Stmt {
	Return(ReturnStmt),
	If(Box<IfStmt>),
	Switch(SwitchStmt),
	Loop(LoopStmt),
	For(ForStmt),
	FunctionCall(FunctionCallStmt),
	Variable(VarDecl),
	Break(Token),
	Continue(Token),
	Discard(Token),
	Assignment(AssignmentStmt),
}

#[derive(Builder, Clone, Debug)]
pub struct ReturnStmt {
	keyword: Token,
	#[builder(setter(into, strip_option), default)]
	expr: Option<Box<Expr>>,
	range: Range,
}

#[derive(Builder, Clone, Debug)]
pub struct IfStmt {
	keyword: Token,
	condition: Box<Expr>,
	body: Vec<Stmt>,
	#[builder(setter(into, strip_option), default)]
	elseif: Option<ElseifStmt>,
	#[builder(setter(into, strip_option), default)]
	else_stmt: Option<ElseStmt>,
	range: Range,
}

#[derive(Builder, Clone, Debug)]
pub struct ElseifStmt {
	keyword: Token,
	condition: Box<Expr>,
	body: Vec<Stmt>,
	range: Range,
}

#[derive(Builder, Clone, Debug)]
pub struct ElseStmt {
	keyword: Token,
	body: Vec<Stmt>,
	range: Range,
}

#[derive(Builder, Clone, Debug)]
pub struct SwitchStmt {
	keyword: Token,
	subject: Box<Expr>,
	body: Vec<CaseStmt>,
	range: Range,
}

#[derive(Builder, Clone, Debug)]
pub struct CaseStmt {
	keyword: Token,
	/// Option is None for `default` case
	selectors: Option<Vec<Expr>>,
	body: Vec<Stmt>,
	range: Range,
}

#[derive(Builder, Clone, Debug)]
pub struct LoopStmt {
	keyword: Token,
	body: Vec<Stmt>,
	range: Range,
}

#[derive(Builder, Clone, Debug)]
pub struct ForStmt {
	keyword: Token,
	header: Vec<Stmt>,
	body: Vec<Stmt>,
	range: Range,
}

#[derive(Builder, Clone, Debug)]
pub struct FunctionCallStmt {
	ident: Token,
	args: Vec<Expr>,
	range: Range,
}

#[derive(Builder, Clone, Debug)]
pub struct AssignmentStmt {
	lhs: Box<Expr>,
	op: Token,
	rhs: Box<Expr>,
	range: Range,
}

#[derive(Clone, Debug)]
pub enum Expr {
	Singular(SingularExpr, Option<PostfixExpr>),
	Binary(SingularExpr, Token, SingularExpr),
}

#[derive(Clone, Debug)]
pub enum SingularExpr {
	Unary(UnaryExpr),
	Primary(PrimaryExpr),
}

#[derive(Clone, Debug)]
pub enum PrimaryExpr {
	TypeCtor(TypeCtorExpr),
	Literal(Token),
	Paren(Box<Expr>),
	Bitcast(BitcastExpr),
	FunctionCall(FunctionCallExpr),
}

#[derive(Clone, Debug)]
pub enum PostfixExpr {
	Bracket(Box<Expr>),
	Dot(Token, Option<Box<PostfixExpr>>),
}

#[derive(Builder, Clone, Debug)]
pub struct UnaryExpr {
	op: Token,
	expr: Box<Expr>,
	range: Range,
}

#[derive(Builder, Clone, Debug)]
pub struct BinaryExpr {
	lhs: Box<Expr>,
	op: Token,
	rhs: Box<Expr>,
	range: Range,
}

#[derive(Builder, Clone, Debug)]
pub struct TypeCtorExpr {
	ty: Box<TypeDecl>,
	args: Vec<Expr>,
	range: Range,
}

#[derive(Builder, Clone, Debug)]
pub struct BitcastExpr {
	keyword: Token,
	ty: Box<TypeDecl>,
	expr: Box<Expr>,
	range: Range,
}

#[derive(Builder, Clone, Debug)]
pub struct FunctionCallExpr {
	ident: Token,
	args: Vec<Expr>,
	range: Range,
}
