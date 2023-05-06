struct LoremIpsum {
	foo: f32,
	bar: f32,
	baz: f32,
};

struct DolorSitAmet {
	foo: f32,
	bar: f32,
	baz: f32,
};

@fragment
fn main(lorem: LoremIpsum, dolor: DolorSitAmet) -> @location(0) vec4<f32> {
	let lorem_foo = lorem.foo;
	let dolor_foo = dolor.foo;

	let lorem_bar = lorem.bar;
	let dolor_bar = dolor.bar;

	let dolor_baz = dolor.baz;
	let lorem_baz = lorem.baz;

	return vec4<f32>(1.0, 1.0, 1.0, 1.0);
}
