use crate::{decl::Decl, expr::Expr, stmt::Stmt};

mod parents;

use gramatika::Spanned;
pub use parents::{find_parent, find_parents};

#[derive(DebugLisp)]
pub enum SyntaxNode<'a> {
	Decl(Decl<'a>),
	Stmt(Stmt<'a>),
	Expr(Expr<'a>),
}

impl<'a> Spanned for SyntaxNode<'a> {
	fn span(&self) -> gramatika::Span {
		match self {
			SyntaxNode::Decl(decl) => decl.span(),
			SyntaxNode::Stmt(stmt) => stmt.span(),
			SyntaxNode::Expr(expr) => expr.span(),
		}
	}
}
