use crate::SyntaxTree;

#[test]
fn e2e() {
	for program in [
		include_str!("../../test-files/shader.wgsl"),
		include_str!("../../test-files/boids.wgsl"),
		include_str!("../../test-files/shadow.wgsl"),
		include_str!("../../test-files/water.wgsl"),
	] {
		super::parse::<SyntaxTree>(program);
	}
}
