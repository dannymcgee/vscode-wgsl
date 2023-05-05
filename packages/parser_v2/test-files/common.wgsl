enable wgsl_plus;

struct<export> Uniforms {
	view_pos: vec4<f32>;
	view_proj: mat4x4<f32>;
};

struct<export> Light {
	position: vec3<f32>;
	color: vec3<f32>;
};
