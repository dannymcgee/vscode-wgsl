use criterion::{criterion_group, criterion_main, BatchSize, BenchmarkId, Criterion};
use gramatika::{ParseStream, ParseStreamer};
use parser_v2::{
	common::{AttributeList, TypeDecl},
	decl::*,
	expr::*,
	stmt::*,
	traversal::{FlowControl, Visitor, Walk},
	SyntaxTree, Token,
};
use utils::hashmap;

use FlowControl::*;

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

pub fn traversal(c: &mut Criterion) {
	let mut group = c.benchmark_group("Visitor");
	group.confidence_level(0.99);

	let programs = hashmap![
		"shader.wgsl" => include_str!("../test-files/shader.wgsl"),
		"boids.wgsl" => include_str!("../test-files/boids.wgsl"),
		"shadow.wgsl" => include_str!("../test-files/shadow.wgsl"),
		"water.wgsl" => include_str!("../test-files/water.wgsl"),
	];

	for (key, program) in programs {
		let name = BenchmarkId::new("Traversal", key);

		let setup = || {
			let tree = match ParseStream::from(program).parse::<SyntaxTree>() {
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
}

impl Visitor for NodeCountVisitor {
	fn visit_decl(&mut self, decl: &Decl) -> FlowControl {
		criterion::black_box(decl);
		self.count += 1;

		Continue
	}

	fn visit_stmt(&mut self, stmt: &Stmt) -> FlowControl {
		criterion::black_box(stmt);
		self.count += 1;

		Continue
	}

	fn visit_expr(&mut self, expr: &Expr) -> FlowControl {
		criterion::black_box(expr);
		self.count += 1;

		Continue
	}

	fn visit_type(&mut self, decl: &TypeDecl) -> FlowControl {
		criterion::black_box(decl);
		self.count += 1;

		Continue
	}

	fn visit_attributes(&mut self, attr: &AttributeList) {
		criterion::black_box(attr);
		self.count += 1;
	}

	fn visit_var_decl(&mut self, decl: &VarDecl) -> FlowControl {
		criterion::black_box(decl);
		self.count += 1;

		Continue
	}

	fn visit_type_alias_decl(&mut self, decl: &TypeAliasDecl) -> FlowControl {
		criterion::black_box(decl);
		self.count += 1;

		Continue
	}

	fn visit_struct_decl(&mut self, decl: &StructDecl) -> FlowControl {
		criterion::black_box(decl);
		self.count += 1;

		Continue
	}

	fn visit_field_decl(&mut self, decl: &FieldDecl) -> FlowControl {
		criterion::black_box(decl);
		self.count += 1;

		Continue
	}

	fn visit_func_decl(&mut self, decl: &FunctionDecl) -> FlowControl {
		criterion::black_box(decl);
		self.count += 1;

		Continue
	}

	fn visit_param_decl(&mut self, decl: &ParamDecl) -> FlowControl {
		criterion::black_box(decl);
		self.count += 1;

		Continue
	}

	fn visit_extension_decl(&mut self, decl: &ExtensionDecl) {
		criterion::black_box(decl);
		self.count += 1;
	}

	fn visit_module_decl(&mut self, decl: &ModuleDecl) {
		criterion::black_box(decl);
		self.count += 1;
	}

	fn visit_return_stmt(&mut self, stmt: &ReturnStmt) -> FlowControl {
		criterion::black_box(stmt);
		self.count += 1;

		Continue
	}

	fn visit_if_stmt(&mut self, stmt: &IfStmt) -> FlowControl {
		criterion::black_box(stmt);
		self.count += 1;

		Continue
	}

	fn visit_elseif_stmt(&mut self, stmt: &IfStmt) -> FlowControl {
		criterion::black_box(stmt);
		self.count += 1;

		Continue
	}

	fn visit_else_stmt(&mut self, stmt: &ElseStmt) -> FlowControl {
		criterion::black_box(stmt);
		self.count += 1;

		Continue
	}

	fn visit_switch_stmt(&mut self, stmt: &SwitchStmt) -> FlowControl {
		criterion::black_box(stmt);
		self.count += 1;

		Continue
	}

	fn visit_case_stmt(&mut self, stmt: &CaseStmt) -> FlowControl {
		criterion::black_box(stmt);
		self.count += 1;

		Continue
	}

	fn visit_loop_stmt(&mut self, stmt: &LoopStmt) -> FlowControl {
		criterion::black_box(stmt);
		self.count += 1;

		Continue
	}

	fn visit_continuing_stmt(&mut self, stmt: &ContinuingStmt) -> FlowControl {
		criterion::black_box(stmt);
		self.count += 1;

		Continue
	}

	fn visit_for_stmt(&mut self, stmt: &ForStmt) -> FlowControl {
		criterion::black_box(stmt);
		self.count += 1;

		Continue
	}

	fn visit_expr_stmt(&mut self, stmt: &ExprStmt) -> FlowControl {
		criterion::black_box(stmt);
		self.count += 1;

		Continue
	}

	fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> FlowControl {
		criterion::black_box(expr);
		self.count += 1;

		Continue
	}

	fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> FlowControl {
		criterion::black_box(expr);
		self.count += 1;

		Continue
	}

	fn visit_assignment_expr(&mut self, expr: &BinaryExpr) -> FlowControl {
		criterion::black_box(expr);
		self.count += 1;

		Continue
	}

	fn visit_fn_call_expr(&mut self, expr: &FnCallExpr) -> FlowControl {
		criterion::black_box(expr);
		self.count += 1;

		Continue
	}

	fn visit_type_ctor_expr(&mut self, expr: &TypeCtorExpr) -> FlowControl {
		criterion::black_box(expr);
		self.count += 1;

		Continue
	}

	fn visit_group_expr(&mut self, expr: &GroupExpr) -> FlowControl {
		criterion::black_box(expr);
		self.count += 1;

		Continue
	}

	fn visit_bitcast_expr(&mut self, expr: &BitcastExpr) -> FlowControl {
		criterion::black_box(expr);
		self.count += 1;

		Continue
	}

	fn visit_primary_expr(&mut self, expr: &PrimaryExpr) -> FlowControl {
		criterion::black_box(expr);
		self.count += 1;

		Continue
	}

	fn visit_postfix_expr(&mut self, expr: &PostfixExpr) -> FlowControl {
		criterion::black_box(expr);
		self.count += 1;

		Continue
	}

	fn visit_literal_expr(&mut self, expr: &Token) {
		criterion::black_box(expr);
		self.count += 1;
	}

	fn visit_ident_expr(&mut self, expr: &IdentExpr) {
		criterion::black_box(expr);
		self.count += 1;
	}
}
