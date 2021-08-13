//
// Vertex Shader
//
enable wgsl_plus;

import common from "./common";
import lib from "./lib";

[[group(1), binding(0)]]
var<uniform> uniforms: common::Uniforms;

[[group(2), binding(0)]]
var<uniform> light: common::Light;

struct VertexInput {
	[[location(0)]] position: vec3<f32>;
	[[location(1)]] tex_coords: vec2<f32>;
	[[location(2)]] normal: vec3<f32>;
};

struct InstanceInput {
	[[location(5)]] model_matrix_0: vec4<f32>;
	[[location(6)]] model_matrix_1: vec4<f32>;
	[[location(7)]] model_matrix_2: vec4<f32>;
	[[location(8)]] model_matrix_3: vec4<f32>;

	[[location(9)]] normal_matrix_0: vec3<f32>;
	[[location(10)]] normal_matrix_1: vec3<f32>;
	[[location(11)]] normal_matrix_2: vec3<f32>;
};

struct VertexOutput {
	[[builtin(position)]] clip_position: vec4<f32>;
	[[location(0)]] tex_coords: vec2<f32>;
	[[location(1)]] world_normal: vec3<f32>;
	[[location(2)]] world_position: vec3<f32>;
};

[[stage(vertex)]]
fn vert(model: VertexInput, instance: InstanceInput) -> VertexOutput {
	let model_matrix = mat4x4<f32>(
		instance.model_matrix_0,
		instance.model_matrix_1,
		instance.model_matrix_2,
		instance.model_matrix_3,
	);
	let normal_matrix = mat3x3<f32>(
		instance.normal_matrix_0,
		instance.normal_matrix_1,
		instance.normal_matrix_2,
	);
	let world_position = model_matrix * vec4<f32>(model.position, 1.0);

	var out: VertexOutput;
	out.tex_coords = model.tex_coords;
	out.world_normal = normal_matrix * model.normal;
	out.world_position = world_position.xyz;
	out.clip_position = uniforms.view_proj * world_position;

	return out;
}

//
// Fragment Shader
//
[[group(0), binding(0)]] var t_diffuse: texture_2d<f32>;
[[group(0), binding(1)]] var s_diffuse: sampler;

[[stage(fragment)]]
fn frag(in: VertexOutput) -> [[location(0)]] vec4<f32> {
	let object_color: vec4<f32> = textureSample(
		t_diffuse,
		s_diffuse,
		in.tex_coords
	);

	let ambient_strength = 0.05;
	let ambient_color = light.color * ambient_strength;

	let light_dir = normalize(light.position - in.world_position);
	let diffuse_strength = max(dot(in.world_normal, light_dir), 0.0);
	let diffuse_strength = lib::dot_floored(in.world_normal, light_dir);
	let diffuse_color = light.color * diffuse_strength;

	let view_dir = normalize(uniforms.view_pos.xyz - in.world_position);
	let half_dir = normalize(view_dir + light_dir);
	let specular_strength = pow(lib::dot_floored(in.world_normal, half_dir), 32.0);
	// let specular_strength = in
	//		.world_normal
	// 	.dot(half_dir)
	// 	.max(0.0)
	// 	.pow(32.0);
	let specular_color = specular_strength * light.color;
	let result = (ambient_color + diffuse_color + specular_color) * object_color.xyz;

	return vec4<f32>(result, object_color.a);
}
