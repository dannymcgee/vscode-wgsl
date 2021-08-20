enable wgsl_plus;

fn<export> dot_floored(a: vec3<f32>, b: vec3<f32>) -> f32 {
	return max(dot(a, b), 0.0);
}
