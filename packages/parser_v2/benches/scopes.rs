use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use gramatika::{ParseStream, ParseStreamer};
use parser_v2::{scopes, SyntaxTree};
use utils::hashmap;

criterion_group!(benches, scopes_builder);
criterion_main!(benches);

pub fn scopes_builder(c: &mut Criterion) {
	let mut group = c.benchmark_group("Scopes");
	group.confidence_level(0.99);

	let programs = hashmap![
		"shader.wgsl" => include_str!("../test-files/shader.wgsl"),
		"boids.wgsl" => include_str!("../test-files/boids.wgsl"),
		"shadow.wgsl" => include_str!("../test-files/shadow.wgsl"),
		"water.wgsl" => include_str!("../test-files/water.wgsl"),
	];

	for (key, program) in programs {
		let name = BenchmarkId::new("Scopes", key);
		let tree = match ParseStream::from(program).parse::<SyntaxTree>() {
			Ok(tree) => tree,
			Err(_) => panic!(),
		};

		group.bench_with_input(name, &tree, move |b, input| {
			b.iter_with_large_drop(|| scopes::build(input))
		});
	}
}
