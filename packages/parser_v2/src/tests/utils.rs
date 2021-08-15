use crate::{
	decl::{Decl, FieldDecl},
	expr::{Expr, IdentExpr},
	tests,
	utils::{self, SyntaxNode},
	ParseStream, SyntaxTree, Token,
};
use gramatika::{span, ParseStreamer};

const PROGRAM: &str = include_str!("../../test-files/shader.wgsl");

#[test]
fn field_decl() {
	let tree = parse();
	// `position` on line 16
	// Expecting to match the field declaration `[[location(0)]] position: vec3<f32>;`
	let target = span![15:17...15:25];
	if let Some(node) = utils::find_parent(&tree, target) {
		assert!(matches!(
			node,
			SyntaxNode::Decl(Decl::Field(FieldDecl {
				name: Token::Ident("position", _),
				..
			}))
		));
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
		assert!(matches!(
			node,
			SyntaxNode::Expr(Expr::Ident(IdentExpr {
				name: Token::Ident("model_matrix_1", _),
				..
			}))
		))
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
			name: `light` (Ident (77:21...77:26)),
		)),
		postfix: (PostfixExpr
			accessor: (Accessor::Dot `.` (Punct (77:26...77:27))),
			expr: (Expr::Primary (PrimaryExpr
				expr: (Expr::Ident (IdentExpr
					name: `color` (Ident (77:27...77:32)),
				)),
			)),
		),
	)),
	op: `*` (Operator (77:33...77:34)),
	rhs: (Expr::Primary (PrimaryExpr
		expr: (Expr::Ident (IdentExpr
			name: `ambient_strength` (Ident (77:35...77:51)),
		)),
	)),
)))
		"#;
		let expected = expected.trim().replace('\t', "   ");
		assert_eq!(found, expected, "{}", tests::diff(&found, &expected));
	} else {
		panic!("Failed to find parent node");
	}
}

fn parse<'a>() -> SyntaxTree<'a> {
	match ParseStream::from(PROGRAM).parse::<SyntaxTree>() {
		Ok(tree) => tree,
		Err(err) => {
			eprintln!("{}", err);
			panic!();
		}
	}
}
