use gramatika::{Span, Spanned};

use crate::{
	decl::Decl,
	expr::Expr,
	stmt::Stmt,
	traversal::{FlowControl, Visitor, Walk},
	SyntaxTree,
};

use super::SyntaxNode;

pub fn find_parent<'a>(tree: &'a SyntaxTree<'a>, target: Span) -> Option<SyntaxNode<'a>> {
	let mut finder = ParentFinder::new(target);
	tree.walk(&mut finder);

	finder.parents.pop()
}

pub fn find_parents<'a>(tree: &'a SyntaxTree<'a>, target: Span) -> Vec<SyntaxNode<'a>> {
	let mut finder = ParentFinder::new(target);
	tree.walk(&mut finder);

	finder.parents.reverse();
	finder.parents
}

struct ParentFinder<'a> {
	target: Span,
	parents: Vec<SyntaxNode<'a>>,
}

impl<'a> ParentFinder<'a> {
	fn new(target: Span) -> Self {
		Self {
			target,
			parents: vec![],
		}
	}
}

impl<'a> Visitor<'a> for ParentFinder<'a> {
	fn visit_decl(&mut self, decl: &'a Decl<'a>) -> FlowControl {
		if decl.span().contains(self.target) {
			self.parents.push(SyntaxNode::Decl(decl.clone()));

			FlowControl::Continue
		} else {
			FlowControl::Break
		}
	}

	fn visit_stmt(&mut self, stmt: &'a Stmt<'a>) -> FlowControl {
		if stmt.span().contains(self.target) {
			self.parents.push(SyntaxNode::Stmt(stmt.clone()));

			FlowControl::Continue
		} else {
			FlowControl::Break
		}
	}

	fn visit_expr(&mut self, expr: &'a Expr<'a>) -> FlowControl {
		if expr.span().contains(self.target) {
			self.parents.push(SyntaxNode::Expr(expr.clone()));

			FlowControl::Continue
		} else {
			FlowControl::Break
		}
	}
}
