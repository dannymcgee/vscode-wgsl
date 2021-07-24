use lsp_types::Range;

use crate::ast::*;

#[derive(Debug)]
pub enum AstNode {
	Decl(Decl),
	Stmt(Stmt),
	Expr(Expr),
}

pub trait GetRange {
	fn range(&self) -> Range;
}

pub trait IsWithin {
	fn is_within(&self, outer: &Self) -> bool;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ParentGranularity {
	Decl,
	Stmt,
	Expr,
}

pub trait ParentOfRange {
	fn parent_of(&self, range: &Range, granularity: ParentGranularity) -> Option<AstNode>;
}

trait ParentsOfRange {
	fn parents_of(&self, range: &Range, granularity: ParentGranularity, nodes: &mut Vec<AstNode>);
}

impl IsWithin for Range {
	fn is_within(&self, outer: &Range) -> bool {
		outer.start <= self.start && outer.end >= self.end
	}
}

impl GetRange for Decl {
	fn range(&self) -> Range {
		use Decl::*;

		match self {
			Var(inner) | Const(inner) => inner.range,
			TypeAlias(inner) => inner.range,
			Struct(inner) => inner.range,
			Field(inner) => inner.range,
			Function(inner) => inner.range,
			Param(inner) => inner.range,
			Extension(inner) => inner.range,
			Module(inner) => inner.range,
		}
	}
}

impl GetRange for Stmt {
	fn range(&self) -> Range {
		use Stmt::*;

		match self {
			Return(inner) => inner.range,
			If(inner) => inner.range,
			Switch(inner) => inner.range,
			Loop(inner) => inner.range,
			Continuing(inner) => inner.range,
			For(inner) => inner.range,
			FunctionCall(inner) => inner.range,
			Variable(inner) => inner.range,
			Break(Token::Keyword(_, r)) => *r,
			Continue(Token::Keyword(_, r)) => *r,
			Discard(Token::Keyword(_, r)) => *r,
			Fallthrough(Token::Keyword(_, r)) => *r,
			Assignment(inner) => inner.range,
			_ => unreachable!(),
		}
	}
}

impl GetRange for Token {
	fn range(&self) -> Range {
		use Token::*;

		*match self {
			Ident(_, range) => range,
			Attr(_, range) => range,
			Field(_, range) => range,
			Type(_, range) => range,
			Function(_, range) => range,
			Keyword(_, range) => range,
			Punct(_, range) => range,
			Op(_, range) => range,
			Literal(_, range) => range,
			Module(_, range) => range,
		}
	}
}

impl GetRange for Expr {
	fn range(&self) -> Range {
		use Expr::*;

		match self {
			Singular(expr) => expr.range,
			Binary(expr) => expr.range,
		}
	}
}

impl GetRange for PrimaryExpr {
	fn range(&self) -> Range {
		use PrimaryExpr::*;

		match self {
			TypeCtor(inner) => inner.range,
			Literal(inner) => inner.range(),
			Paren(inner) => inner.range(),
			Bitcast(inner) => inner.range,
			Identifier(inner) => inner.range(),
			FunctionCall(inner) => inner.range,
		}
	}
}

impl ParentOfRange for Vec<Decl> {
	fn parent_of(&self, range: &Range, granularity: ParentGranularity) -> Option<AstNode> {
		let mut nodes = vec![];

		for decl in self {
			decl.parents_of(range, granularity, &mut nodes);
		}

		nodes.pop()
	}
}

impl ParentsOfRange for Decl {
	fn parents_of(&self, range: &Range, granularity: ParentGranularity, nodes: &mut Vec<AstNode>) {
		use Decl::*;

		if range.is_within(&self.range()) {
			nodes.push(AstNode::Decl(self.clone()));

			match self {
				Var(ref decl) | Const(ref decl) if granularity > ParentGranularity::Stmt => {
					if let Some(ref expr) = decl.assignment {
						expr.parents_of(range, granularity, nodes);
					}
				}
				Function(ref decl) => decl.parents_of(range, granularity, nodes),
				_ => {}
			}
		}
	}
}

impl ParentsOfRange for FunctionDecl {
	fn parents_of(&self, range: &Range, granularity: ParentGranularity, nodes: &mut Vec<AstNode>) {
		let (inner_range, body) = &self.body;

		if !range.is_within(inner_range) {
			return;
		}

		for stmt in body.iter() {
			stmt.parents_of(range, granularity, nodes);
		}
	}
}

impl ParentsOfRange for Stmt {
	fn parents_of(&self, range: &Range, granularity: ParentGranularity, nodes: &mut Vec<AstNode>) {
		use Stmt::*;

		if range.is_within(&self.range())
			&& (granularity >= ParentGranularity::Stmt || matches!(self, Stmt::Variable(_)))
		{
			nodes.push(AstNode::Stmt(self.clone()));

			let go_deeper = granularity > ParentGranularity::Stmt;

			match self {
				Return(ref stmt) if go_deeper && stmt.expr.is_some() => {
					stmt.expr
						.as_ref()
						.unwrap()
						.parents_of(range, granularity, nodes);
				}
				If(ref stmt) => stmt.parents_of(range, granularity, nodes),
				Switch(ref stmt) => stmt.parents_of(range, granularity, nodes),
				Loop(ref stmt) => stmt.parents_of(range, granularity, nodes),
				For(ref stmt) => stmt.parents_of(range, granularity, nodes),
				FunctionCall(ref expr) if go_deeper => expr.parents_of(range, granularity, nodes),
				Assignment(ref stmt) if go_deeper => stmt.parents_of(range, granularity, nodes),
				Variable(ref decl) if go_deeper => {
					if let Some(ref expr) = decl.assignment {
						expr.parents_of(range, granularity, nodes);
					}
				}
				_ => {}
			}
		}
	}
}

impl ParentsOfRange for IfStmt {
	fn parents_of(&self, range: &Range, granularity: ParentGranularity, nodes: &mut Vec<AstNode>) {
		if granularity > ParentGranularity::Stmt {
			self.condition.parents_of(range, granularity, nodes);
		}

		for stmt in self.body.iter() {
			stmt.parents_of(range, granularity, nodes);
		}
		if let Some(ref stmt) = self.elseif {
			stmt.parents_of(range, granularity, nodes);
		}
		if let Some(ref stmt) = self.else_stmt {
			stmt.parents_of(range, granularity, nodes);
		}
	}
}

impl ParentsOfRange for ElseifStmt {
	fn parents_of(&self, range: &Range, granularity: ParentGranularity, nodes: &mut Vec<AstNode>) {
		for stmt in self.body.iter() {
			stmt.parents_of(range, granularity, nodes);
		}
	}
}

impl ParentsOfRange for ElseStmt {
	fn parents_of(&self, range: &Range, granularity: ParentGranularity, nodes: &mut Vec<AstNode>) {
		for stmt in self.body.iter() {
			stmt.parents_of(range, granularity, nodes);
		}
	}
}

impl ParentsOfRange for SwitchStmt {
	fn parents_of(&self, range: &Range, granularity: ParentGranularity, nodes: &mut Vec<AstNode>) {
		for case_stmt in self.body.iter() {
			for stmt in case_stmt.body.iter() {
				stmt.parents_of(range, granularity, nodes);
			}
		}
	}
}

impl ParentsOfRange for LoopStmt {
	fn parents_of(&self, range: &Range, granularity: ParentGranularity, nodes: &mut Vec<AstNode>) {
		for stmt in self.body.iter() {
			stmt.parents_of(range, granularity, nodes);
		}

		if let Some(ref continuing) = self.continuing {
			for stmt in continuing.body.iter() {
				stmt.parents_of(range, granularity, nodes);
			}
		}
	}
}

impl ParentsOfRange for ForStmt {
	fn parents_of(&self, range: &Range, granularity: ParentGranularity, nodes: &mut Vec<AstNode>) {
		if let Some(ref init) = self.header.init {
			init.parents_of(range, granularity, nodes);
		}

		if granularity > ParentGranularity::Stmt {
			if let Some(ref condition) = self.header.condition {
				condition.parents_of(range, granularity, nodes);
			}
		}

		if let Some(ref iterator) = self.header.iterator {
			iterator.parents_of(range, granularity, nodes);
		}

		for stmt in self.body.iter() {
			stmt.parents_of(range, granularity, nodes);
		}
	}
}

impl ParentsOfRange for AssignmentStmt {
	fn parents_of(&self, range: &Range, granularity: ParentGranularity, nodes: &mut Vec<AstNode>) {
		if granularity < ParentGranularity::Expr {
			return;
		}

		self.lhs.parents_of(range, granularity, nodes);
		self.rhs.parents_of(range, granularity, nodes);
	}
}

impl ParentsOfRange for Expr {
	fn parents_of(&self, range: &Range, granularity: ParentGranularity, nodes: &mut Vec<AstNode>) {
		if granularity < ParentGranularity::Expr {
			return;
		}

		if range.is_within(&self.range()) {
			nodes.push(AstNode::Expr(self.clone()));

			match self {
				Expr::Singular(ref expr) => expr.parents_of(range, granularity, nodes),
				Expr::Binary(ref expr) => {
					expr.lhs.parents_of(range, granularity, nodes);
					expr.rhs.parents_of(range, granularity, nodes);
				}
			}
		}
	}
}

impl ParentsOfRange for SingularExpr {
	fn parents_of(&self, range: &Range, granularity: ParentGranularity, nodes: &mut Vec<AstNode>) {
		if granularity < ParentGranularity::Expr {
			return;
		}

		if range.is_within(&self.range) {
			nodes.push(AstNode::Expr(Expr::Singular(self.clone())));

			self.expr.parents_of(range, granularity, nodes);
		}
	}
}

impl ParentsOfRange for PrimaryExpr {
	fn parents_of(&self, range: &Range, granularity: ParentGranularity, nodes: &mut Vec<AstNode>) {
		use PrimaryExpr::*;

		if granularity < ParentGranularity::Expr {
			return;
		}

		if range.is_within(&self.range()) {
			nodes.push(AstNode::Expr(Expr::Singular(SingularExpr::new(
				self.clone(),
				self.range(),
			))));

			match self {
				TypeCtor(ref expr) => {
					for arg in &expr.args {
						arg.parents_of(range, granularity, nodes);
					}
				}
				Paren(ref expr) => expr.parents_of(range, granularity, nodes),
				Bitcast(ref expr) => expr.parents_of(range, granularity, nodes),
				FunctionCall(ref expr) => expr.parents_of(range, granularity, nodes),
				_ => {}
			}
		}
	}
}

impl ParentsOfRange for BitcastExpr {
	fn parents_of(&self, range: &Range, granularity: ParentGranularity, nodes: &mut Vec<AstNode>) {
		if granularity < ParentGranularity::Expr {
			return;
		}

		if range.is_within(&self.range) {
			self.expr.parents_of(range, granularity, nodes);
		}
	}
}

impl ParentsOfRange for FunctionCallExpr {
	fn parents_of(&self, range: &Range, granularity: ParentGranularity, nodes: &mut Vec<AstNode>) {
		if granularity < ParentGranularity::Expr {
			return;
		}

		if range.is_within(&self.range) {
			for arg in &self.args {
				arg.parents_of(range, granularity, nodes);
			}
		}
	}
}
