use criterion::{criterion_group, criterion_main, BatchSize, BenchmarkId, Criterion};
use gramatika::{ParseStream, ParseStreamer};
use parser_v2::{
	common::{AttributeList, TypeDecl},
	decl::{
		Decl, ExtensionDecl, FieldDecl, FunctionDecl, ModuleDecl, ParamDecl, StructDecl,
		TypeAliasDecl, VarDecl,
	},
	expr::{
		BinaryExpr, BitcastExpr, Expr, FnCallExpr, GroupExpr, IdentExpr, PostfixExpr, PrimaryExpr,
		TypeCtorExpr, UnaryExpr,
	},
	stmt::{
		CaseStmt, ContinuingStmt, ElseStmt, ExprStmt, ForStmt, IfStmt, LoopStmt, ReturnStmt, Stmt,
		SwitchStmt,
	},
	traversal::{FlowControl, Visitor, Walk},
	SyntaxTree, Token,
};

const PROGRAM: &str = include_str!("shader.wgsl");

criterion_group!(benches, traversal);
criterion_main!(benches);

#[derive(Default)]
struct NodeCountVisitor {
	count: usize,
}

impl NodeCountVisitor {
	fn new() -> Self {
		Self::default()
	}
}

use FlowControl::*;

impl<'a> Visitor<'a> for NodeCountVisitor {
	fn visit_decl(&mut self, decl: &'a Decl<'a>) -> FlowControl {
		criterion::black_box(decl);
		self.count += 1;

		Continue
	}

	fn visit_stmt(&mut self, stmt: &'a Stmt<'a>) -> FlowControl {
		criterion::black_box(stmt);
		self.count += 1;

		Continue
	}

	fn visit_expr(&mut self, expr: &'a Expr<'a>) -> FlowControl {
		criterion::black_box(expr);
		self.count += 1;

		Continue
	}

	fn visit_type(&mut self, decl: &'a TypeDecl<'a>) -> FlowControl {
		criterion::black_box(decl);
		self.count += 1;

		Continue
	}

	fn visit_attributes(&mut self, attr: &'a AttributeList<'a>) {
		criterion::black_box(attr);
		self.count += 1;
	}

	fn visit_var_decl(&mut self, decl: &'a VarDecl<'a>) -> FlowControl {
		criterion::black_box(decl);
		self.count += 1;

		Continue
	}

	fn visit_type_alias_decl(&mut self, decl: &'a TypeAliasDecl<'a>) -> FlowControl {
		criterion::black_box(decl);
		self.count += 1;

		Continue
	}

	fn visit_struct_decl(&mut self, decl: &'a StructDecl<'a>) -> FlowControl {
		criterion::black_box(decl);
		self.count += 1;

		Continue
	}

	fn visit_field_decl(&mut self, decl: &'a FieldDecl<'a>) -> FlowControl {
		criterion::black_box(decl);
		self.count += 1;

		Continue
	}

	fn visit_func_decl(&mut self, decl: &'a FunctionDecl<'a>) -> FlowControl {
		criterion::black_box(decl);
		self.count += 1;

		Continue
	}

	fn visit_param_decl(&mut self, decl: &'a ParamDecl<'a>) -> FlowControl {
		criterion::black_box(decl);
		self.count += 1;

		Continue
	}

	fn visit_extension_decl(&mut self, decl: &'a ExtensionDecl<'a>) {
		criterion::black_box(decl);
		self.count += 1;
	}

	fn visit_module_decl(&mut self, decl: &'a ModuleDecl<'a>) {
		criterion::black_box(decl);
		self.count += 1;
	}

	fn visit_return_stmt(&mut self, stmt: &'a ReturnStmt<'a>) -> FlowControl {
		criterion::black_box(stmt);
		self.count += 1;

		Continue
	}

	fn visit_if_stmt(&mut self, stmt: &'a IfStmt<'a>) -> FlowControl {
		criterion::black_box(stmt);
		self.count += 1;

		Continue
	}

	fn visit_elseif_stmt(&mut self, stmt: &'a IfStmt<'a>) -> FlowControl {
		criterion::black_box(stmt);
		self.count += 1;

		Continue
	}

	fn visit_else_stmt(&mut self, stmt: &'a ElseStmt<'a>) -> FlowControl {
		criterion::black_box(stmt);
		self.count += 1;

		Continue
	}

	fn visit_switch_stmt(&mut self, stmt: &'a SwitchStmt<'a>) -> FlowControl {
		criterion::black_box(stmt);
		self.count += 1;

		Continue
	}

	fn visit_case_stmt(&mut self, stmt: &'a CaseStmt<'a>) -> FlowControl {
		criterion::black_box(stmt);
		self.count += 1;

		Continue
	}

	fn visit_loop_stmt(&mut self, stmt: &'a LoopStmt<'a>) -> FlowControl {
		criterion::black_box(stmt);
		self.count += 1;

		Continue
	}

	fn visit_continuing_stmt(&mut self, stmt: &'a ContinuingStmt<'a>) -> FlowControl {
		criterion::black_box(stmt);
		self.count += 1;

		Continue
	}

	fn visit_for_stmt(&mut self, stmt: &'a ForStmt<'a>) -> FlowControl {
		criterion::black_box(stmt);
		self.count += 1;

		Continue
	}

	fn visit_expr_stmt(&mut self, stmt: &'a ExprStmt<'a>) -> FlowControl {
		criterion::black_box(stmt);
		self.count += 1;

		Continue
	}

	fn visit_unary_expr(&mut self, expr: &'a UnaryExpr<'a>) -> FlowControl {
		criterion::black_box(expr);
		self.count += 1;

		Continue
	}

	fn visit_binary_expr(&mut self, expr: &'a BinaryExpr<'a>) -> FlowControl {
		criterion::black_box(expr);
		self.count += 1;

		Continue
	}

	fn visit_assignment_expr(&mut self, expr: &'a BinaryExpr<'a>) -> FlowControl {
		criterion::black_box(expr);
		self.count += 1;

		Continue
	}

	fn visit_fn_call_expr(&mut self, expr: &'a FnCallExpr<'a>) -> FlowControl {
		criterion::black_box(expr);
		self.count += 1;

		Continue
	}

	fn visit_type_ctor_expr(&mut self, expr: &'a TypeCtorExpr<'a>) -> FlowControl {
		criterion::black_box(expr);
		self.count += 1;

		Continue
	}

	fn visit_group_expr(&mut self, expr: &'a GroupExpr<'a>) -> FlowControl {
		criterion::black_box(expr);
		self.count += 1;

		Continue
	}

	fn visit_bitcast_expr(&mut self, expr: &'a BitcastExpr<'a>) -> FlowControl {
		criterion::black_box(expr);
		self.count += 1;

		Continue
	}

	fn visit_primary_expr(&mut self, expr: &'a PrimaryExpr<'a>) -> FlowControl {
		criterion::black_box(expr);
		self.count += 1;

		Continue
	}

	fn visit_postfix_expr(&mut self, expr: &'a PostfixExpr<'a>) -> FlowControl {
		criterion::black_box(expr);
		self.count += 1;

		Continue
	}

	fn visit_literal_expr(&mut self, expr: &'a Token<'a>) {
		criterion::black_box(expr);
		self.count += 1;
	}

	fn visit_ident_expr(&mut self, expr: &'a IdentExpr<'a>) {
		criterion::black_box(expr);
		self.count += 1;
	}
}

pub fn traversal(c: &mut Criterion) {
	let mut group = c.benchmark_group("Visitor");
	group.confidence_level(0.99);

	let name = BenchmarkId::new("Traversal", "shader.wgsl");

	let setup = || {
		let tree = match ParseStream::from(PROGRAM).parse::<SyntaxTree>() {
			Ok(tree) => tree,
			Err(_) => panic!(),
		};
		let visitor = NodeCountVisitor::new();

		(tree, visitor)
	};

	let routine = |(tree, visitor): &mut (SyntaxTree, NodeCountVisitor)| {
		tree.walk(visitor);
		visitor.count
	};

	group.bench_function(name, move |b| {
		b.iter_batched_ref(setup, routine, BatchSize::SmallInput)
	});
}
