use crate::expr::Expr;

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
