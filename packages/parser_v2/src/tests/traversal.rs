use std::collections::HashMap;

use gramatika::{ParseStreamer, Token as _};

use crate::{
	decl::VarDecl,
	expr::IdentExpr,
	traversal::{FlowControl, Visitor},
	ParseStream, SyntaxTree,
};

#[derive(Default)]
struct UnusedVarsVisitor {
	counts: HashMap<String, usize>,
}

impl UnusedVarsVisitor {
	fn new() -> Self {
		Self::default()
	}
}

impl<'a> Visitor<'a> for UnusedVarsVisitor {
	fn visit_var_decl(&mut self, decl: &VarDecl<'a>) -> FlowControl {
		self.counts.insert(decl.name.lexeme().into(), 0);

		FlowControl::Continue
	}

	fn visit_ident_expr(&mut self, expr: &IdentExpr<'a>) {
		if let Some(count) = self.counts.get_mut(expr.name.lexeme()) {
			*count += 1;
		}
	}
}

const TEST_PROGRAM: &str = r#"
fn main() {
	var a: i32 = 4;
	let b = a;
	let c = 2;

	do_something(a, c);
}
"#;

#[test]
fn unused_vars_visitor() {
	match ParseStream::from(TEST_PROGRAM).parse::<SyntaxTree>() {
		Ok(tree) => {
			let mut visitor = UnusedVarsVisitor::new();
			tree.walk(&mut visitor);

			assert!(visitor.counts.contains_key("a"));
			assert!(visitor.counts.contains_key("b"));
			assert!(visitor.counts.contains_key("c"));

			for (token, count) in visitor.counts.iter() {
				match &token[..] {
					"a" => {
						assert_eq!(*count, 2);
					}
					"b" => {
						assert_eq!(*count, 0);
					}
					"c" => {
						assert_eq!(*count, 1);
					}
					_ => {}
				}
			}
		}
		Err(err) => {
			eprintln!("{}", err);
			panic!();
		}
	}
}
