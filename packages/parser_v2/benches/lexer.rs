use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};

use gramatika::Lexer as _;
use parser_v2::token::Lexer;

const PROGRAM: &str = include_str!("shader.wgsl");

criterion_group!(benches, lexer);
criterion_main!(benches);

pub fn lexer(c: &mut Criterion) {
	let mut group = c.benchmark_group("Lexer");
	group.confidence_level(0.99);

	let name = BenchmarkId::new("Lexer", "shader.wgsl");

	group.bench_with_input(name, PROGRAM, move |b, input| {
		b.iter_with_large_drop(|| Lexer::new(input).scan())
	});
}
