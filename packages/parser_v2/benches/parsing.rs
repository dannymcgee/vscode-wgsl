use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use gramatika::{ParseStream, ParseStreamer};
use parser_v2::SyntaxTree;

const PROGRAM: &str = include_str!("shader.wgsl");

criterion_group!(benches, parsing);
criterion_main!(benches);

pub fn parsing(c: &mut Criterion) {
	let mut group = c.benchmark_group("Parsing");
	group.confidence_level(0.99);

	let name = BenchmarkId::new("Parser v2", "shader.wgsl");
	group.bench_with_input(name, PROGRAM, move |b, input| {
		b.iter_with_large_drop(|| match ParseStream::from(input).parse::<SyntaxTree>() {
			Ok(tree) => tree,
			Err(_) => panic!(),
		});
	});

	let name = BenchmarkId::new("Parser v1 (AST)", "shader.wgsl");
	group.bench_with_input(name, PROGRAM, move |b, input| {
		b.iter_with_large_drop(|| match parser::parse_ast(input) {
			Ok(ast) => ast,
			Err(_) => panic!(),
		});
	});

	let name = BenchmarkId::new("Parser v1 (raw)", "shader.wgsl");
	group.bench_with_input(name, PROGRAM, move |b, input| {
		b.iter_with_large_drop(|| match parser::parse(input) {
			Ok(rules) => rules,
			Err(_) => panic!(),
		});
	});
}
