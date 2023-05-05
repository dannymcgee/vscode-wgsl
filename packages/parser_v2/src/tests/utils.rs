use crate::{tests, utils, ParseStream, SyntaxTree};
use gramatika::{span, ParseStreamer};

const PROGRAM: &str = include_str!("../../test-files/shader.wgsl");

#[test]
fn field_decl() {
	let tree = parse();
	// `position` on line 16
	// Expecting to match the field declaration `@location(0) position: vec3<f32>;`
	let target = span![15:17...15:25];
	if let Some(node) = utils::find_parent(&tree, target) {
		let found = format!("{:#?}", node);
		let expected = r#"
(SyntaxNode::Decl (Decl::Field (FieldDecl
	attributes: (AttributeList
		attributes: [
			(Attribute
				at_sign: `@` (Punct (16:2...16:3)),
				name: `location` (Attribute (16:3...16:11)),
				params: (Expr::Group (GroupExpr
					brace_open: `(` (Brace (16:11...16:12)),
					expr: (Expr::Primary (PrimaryExpr
						expr: (Expr::Literal `0` (IntLiteral (16:12...16:13))),
					)),
					brace_close: `)` (Brace (16:13...16:14)),
				)),
			),
		],
	),
	name: `position` (Field (16:15...16:23)),
	ty: (TypeDecl
		annotator: `:` (Punct (16:23...16:24)),
		name: (IdentExpr
			name: `vec3` (Type (16:25...16:29)),
		),
		child_ty: (TypeDecl
			name: (IdentExpr
				name: `f32` (Type (16:30...16:33)),
			),
		),
	),
	semicolon: `;` (Punct (16:34...16:35)),
)))
		"#;
		let expected = expected.trim().replace('\t', "  ");
		assert_eq!(found, expected, "\n{}", tests::diff(&found, &expected));
	} else {
		panic!("Failed to find parent node");
	}
}

#[test]
fn ident_expr() {
	let tree = parse();
	// `matrix` on line 43
	// Expecting to match the IdentExpr `model_matrix_1`
	let target = span![42:17...42:23];
	if let Some(node) = utils::find_parent(&tree, target) {
		let found = format!("{:#?}", node);
		let expected = r#"
(SyntaxNode::Expr (Expr::Ident (IdentExpr
	name: `model_matrix_1` (Field (43:12...43:26)),
)))
		"#;
		let expected = expected.trim().replace('\t', "  ");
		assert_eq!(found, expected, "\n{}", tests::diff(&found, &expected));
	} else {
		panic!("Failed to find parent node");
	}
}

#[test]
fn binary_expr() {
	let tree = parse();
	// `*` on line 78
	// Expecting to match the BinaryExpr `light.color * ambient_strength`
	let target = span![77:33...77:34];
	if let Some(node) = utils::find_parent(&tree, target) {
		// FIXME: Use some sort of snapshot-testing library -- this is not sustainable
		let found = format!("{:#?}", node);
		let expected = r#"
(SyntaxNode::Expr (Expr::Binary (BinaryExpr
	lhs: (Expr::Primary (PrimaryExpr
		expr: (Expr::Ident (IdentExpr
			name: `light` (Ident (78:22...78:27)),
		)),
		postfix: (PostfixExpr
			accessor: (Accessor::Dot `.` (Punct (78:27...78:28))),
			expr: (Expr::Primary (PrimaryExpr
				expr: (Expr::Ident (IdentExpr
					name: `color` (Field (78:28...78:33)),
				)),
			)),
		),
	)),
	op: `*` (Operator (78:34...78:35)),
	rhs: (Expr::Primary (PrimaryExpr
		expr: (Expr::Ident (IdentExpr
			name: `ambient_strength` (Ident (78:36...78:52)),
		)),
	)),
)))
		"#;
		let expected = expected.trim().replace('\t', "  ");
		assert_eq!(found, expected, "\n{}", tests::diff(&found, &expected));
	} else {
		panic!("Failed to find parent node");
	}
}

fn parse() -> SyntaxTree {
	match ParseStream::from(PROGRAM).parse::<SyntaxTree>() {
		Ok(tree) => tree,
		Err(err) => {
			eprintln!("{}", err);
			panic!();
		}
	}
}
