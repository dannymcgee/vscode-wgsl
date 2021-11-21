use gramatika::{Span, Spanned};

use crate::{
	decl::Decl,
	expr::Expr,
	stmt::Stmt,
	traversal::{FlowControl, Visitor, Walk},
	SyntaxTree,
};

use super::SyntaxNode;

pub fn find_parent(tree: &SyntaxTree, target: Span) -> Option<SyntaxNode> {
	let mut finder = ParentFinder::new(target);
	tree.walk(&mut finder);

	finder.parents.pop()
}

pub fn find_parents(tree: &SyntaxTree, target: Span) -> Vec<SyntaxNode> {
	let mut finder = ParentFinder::new(target);
	tree.walk(&mut finder);

	finder.parents.reverse();
	finder.parents
}

struct ParentFinder {
	target: Span,
	parents: Vec<SyntaxNode>,
}

impl ParentFinder {
	fn new(target: Span) -> Self {
		Self {
			target,
			parents: vec![],
		}
	}
}

impl Visitor for ParentFinder {
	fn visit_decl(&mut self, decl: &Decl) -> FlowControl {
		if decl.span().contains(self.target) {
			self.parents.push(SyntaxNode::Decl(decl.clone()));

			FlowControl::Continue
		} else {
			FlowControl::Break
		}
	}

	fn visit_stmt(&mut self, stmt: &Stmt) -> FlowControl {
		if stmt.span().contains(self.target) {
			self.parents.push(SyntaxNode::Stmt(stmt.clone()));

			FlowControl::Continue
		} else {
			FlowControl::Break
		}
	}

	fn visit_expr(&mut self, expr: &Expr) -> FlowControl {
		if expr.span().contains(self.target) {
			self.parents.push(SyntaxNode::Expr(expr.clone()));

			FlowControl::Continue
		} else {
			FlowControl::Break
		}
	}
}
