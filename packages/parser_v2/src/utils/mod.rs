use crate::{decl::Decl, expr::Expr, stmt::Stmt};

mod parents;

use gramatika::{Span, Spanned};
use lsp_types::{Position, Range};
pub use parents::{find_parent, find_parents};

#[derive(DebugLisp)]
pub enum SyntaxNode {
	Decl(Decl),
	Stmt(Stmt),
	Expr(Expr),
}

impl Spanned for SyntaxNode {
	fn span(&self) -> gramatika::Span {
		match self {
			SyntaxNode::Decl(decl) => decl.span(),
			SyntaxNode::Stmt(stmt) => stmt.span(),
			SyntaxNode::Expr(expr) => expr.span(),
		}
	}
}

pub trait ToRange {
	fn to_range(self) -> Range;
}

impl ToRange for Span {
	fn to_range(self) -> Range {
		Range {
			start: Position {
				line: self.start.line as _,
				character: self.start.character as _,
			},
			end: Position {
				line: self.end.line as _,
				character: self.end.character as _,
			},
		}
	}
}
