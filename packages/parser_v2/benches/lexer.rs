use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use gramatika::Lexer as _;
use parser_v2::token::Lexer;
use utils::hashmap;

criterion_group!(benches, lexer);
criterion_main!(benches);

pub fn lexer(c: &mut Criterion) {
	let mut group = c.benchmark_group("Lexer");
	group.confidence_level(0.99);

	let programs = hashmap![
		"shader.wgsl" => include_str!("../test-files/shader.wgsl"),
		"boids.wgsl" => include_str!("../test-files/boids.wgsl"),
		"shadow.wgsl" => include_str!("../test-files/shadow.wgsl"),
		"water.wgsl" => include_str!("../test-files/water.wgsl"),
	];

	for (key, program) in programs {
		let name = BenchmarkId::new("Lexer", key);

		group.bench_with_input(name, program, move |b, input| {
			b.iter_with_large_drop(|| Lexer::new(input).scan())
		});
	}

	group.finish();
}
