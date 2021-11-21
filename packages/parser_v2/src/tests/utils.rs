use crate::{tests, utils, ParseStream, SyntaxTree};
use gramatika::{span, ParseStreamer};

const PROGRAM: &str = include_str!("../../test-files/shader.wgsl");

#[test]
fn field_decl() {
	let tree = parse();
	// `position` on line 16
	// Expecting to match the field declaration `[[location(0)]] position: vec3<f32>;`
	let target = span![15:17...15:25];
	if let Some(node) = utils::find_parent(&tree, target) {
		let found = format!("{:#?}", node);
		let expected = r#"
(SyntaxNode::Decl (Decl::Field (FieldDecl
	attributes: (AttributeList
		open_brace: `[[` (Brace (16:2...16:4)),
		attributes: [
			(Attribute
				name: `location` (Attribute (16:4...16:12)),
				value: `0` (IntLiteral (16:13...16:14)),
			),
		],
	  close_brace: `]]` (Brace (16:15...16:17)),
	),
	name: `position` (Field (16:18...16:26)),
	ty: (TypeDecl
		annotator: `:` (Punct (16:26...16:27)),
		name: (IdentExpr
			name: `vec3` (Type (16:28...16:32)),
		),
		child_ty: `f32` (Type (16:33...16:36)),
	),
	semicolon: `;` (Punct (16:37...16:38)),
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
