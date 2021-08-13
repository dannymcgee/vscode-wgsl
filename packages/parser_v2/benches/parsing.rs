use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use gramatika::{ParseStream, ParseStreamer};
use parser_v2::SyntaxTree;
use utils::hashmap;

criterion_group!(benches, parsing);
criterion_main!(benches);

pub fn parsing(c: &mut Criterion) {
	let mut group = c.benchmark_group("Parsing");
	group.confidence_level(0.99);

	let programs = hashmap![
		"shader.wgsl" => include_str!("../test-files/shader.wgsl"),
		"boids.wgsl" => include_str!("../test-files/boids.wgsl"),
		"shadow.wgsl" => include_str!("../test-files/shadow.wgsl"),
		"water.wgsl" => include_str!("../test-files/water.wgsl"),
	];

	for (key, program) in programs {
		let name = BenchmarkId::new("Parser v2", key);
		group.bench_with_input(name, program, move |b, input| {
			b.iter_with_large_drop(|| match ParseStream::from(input).parse::<SyntaxTree>() {
				Ok(tree) => tree,
				Err(err) => panic!("{}", err),
			});
		});

		let name = BenchmarkId::new("Parser v1 (AST)", key);
		group.bench_with_input(name, program, move |b, input| {
			b.iter_with_large_drop(|| match parser::parse_ast(input) {
				Ok(ast) => ast,
				Err(err) => panic!("{}", err),
			});
		});

		let name = BenchmarkId::new("Parser v1 (raw)", key);
		group.bench_with_input(name, program, move |b, input| {
			b.iter_with_large_drop(|| match parser::parse(input) {
				Ok(rules) => rules,
				Err(err) => panic!("{}", err),
			});
		});
	}

	group.finish();
}
