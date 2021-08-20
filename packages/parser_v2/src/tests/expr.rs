use gramatika::ParseStreamer;

use crate::{expr::Expr, stmt::Stmt, tests, ParseStream};

#[test]
fn unary_expr() {
	super::parse::<Expr>("-foo");
	super::parse::<Expr>("!foo");
	super::parse::<Expr>("~foo");
	super::parse::<Expr>("*foo");
	super::parse::<Expr>("&foo");
}

#[test]
fn binary_expr() {
	super::parse::<Expr>("2 + 2");
	super::parse::<Expr>("2 + 2 * 4");
	super::parse::<Expr>("2 * 2 + 4");
	super::parse::<Expr>("idx % 15 == 0");
}

#[test]
fn type_ctor_expr() {
	super::parse::<Expr>("vec3<f32>(1.0, 1.0, 1.0)");
}

#[test]
fn group_expr() {
	super::parse::<Expr>("(2 + 2) * 4");
}

#[test]
fn bitcast_expr() {
	super::parse::<Expr>("bitcast<f32>(foo)");
}

#[test]
fn literal_expr() {
	super::parse::<Expr>("42.42");
	super::parse::<Expr>("0x42.42");
	super::parse::<Expr>("0");
	super::parse::<Expr>("42u");
	super::parse::<Expr>("true");
}

#[test]
fn ident_expr() {
	super::parse::<Expr>("foo");
	super::parse::<Expr>("foo::bar");
}

#[test]
fn assignment_expr() {
	super::parse::<Expr>("foo = 2");
	super::parse::<Expr>("foo.bar = 2");
	// super::parse::<Expr>("2 = 2"); ERROR: Invalid assignment target
}

#[test]
fn fn_call_expr() {
	super::parse::<Expr>("max(4, 2)");
	super::parse::<Expr>("some::imported_fn(4, 2)");
}

#[test]
fn primary_expr_postfixes() {
	super::parse::<Expr>("foo.bar.baz");
	super::parse::<Expr>("foo.bar[baz]");
	super::parse::<Expr>("foo.bar(4).baz(2)[0]");
}

// --- Regressions -----------------------------------------------------------------------

/// The postfix expression following `light.` should only encompass `color`,
/// instead of consuming `color * ambient_strength` as a binary expression
#[test]
fn limited_postfix_expr() {
	// FIXME: Use some sort of snapshot-testing library -- this is not sustainable
	match ParseStream::from("light.color * ambient_strength").parse::<Expr>() {
		Ok(expr) => {
			let parsed = format!("{:#?}", expr);
			let expected = r#"
(Expr::Binary (BinaryExpr
	lhs: (Expr::Primary (PrimaryExpr
		expr: (Expr::Ident (IdentExpr
			name: `light` (Ident (0:0...0:5)),
		)),
		postfix: (PostfixExpr
			accessor: (Accessor::Dot `.` (Punct (0:5...0:6))),
			expr: (Expr::Primary (PrimaryExpr
				expr: (Expr::Ident (IdentExpr
					name: `color` (Ident (0:6...0:11)),
				)),
			)),
		),
	)),
	op: `*` (Operator (0:12...0:13)),
	rhs: (Expr::Primary (PrimaryExpr
		expr: (Expr::Ident (IdentExpr
			name: `ambient_strength` (Ident (0:14...0:30)),
		)),
	)),
))
			"#;
			let expected = expected.trim().replace('\t', "   ");

			assert_eq!(parsed, expected, "{}", tests::diff(&parsed, &expected));
		}
		Err(err) => {
			eprintln!("{}", err);
			panic!();
		}
	}
}

#[test]
fn chained_method_calls() {
	let stmt = r#"
let specular_strength = in
	.world_normal
	.dot(half_dir)
	.max(0.0)
	.pow(32.0);
	"#;
	match ParseStream::from(stmt).parse::<Stmt>() {
		Ok(tree) => {
			eprintln!("{:#?}", tree);
		}
		Err(err) => {
			eprintln!("{}", err);
		}
	}
}
