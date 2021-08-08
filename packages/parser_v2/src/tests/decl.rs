use crate::decl::{FunctionDecl, StructDecl, TypeAliasDecl, VarDecl};

#[test]
fn var_decl() {
	super::parse::<VarDecl>("let foo = 1.0;");
	super::parse::<VarDecl>("var foo: f32 = 1.0;");
	super::parse::<VarDecl>("var foo: f32;");
	super::parse::<VarDecl>("var<uniform> uniforms: Uniforms;");
	super::parse::<VarDecl>("var<uniform> uniforms: common::Uniforms;");
}

#[test]
fn type_alias_decl() {
	super::parse::<TypeAliasDecl>("type ViewProjectionMatrix = mat4x4<f32>;");
}

#[test]
fn struct_decl() {
	super::parse::<StructDecl>(
		r#"
struct Data {
	a: i32;
	b: vec2<f32>;
};
	"#,
	);
	super::parse::<StructDecl>(
		r#"
struct VertexOutput {
	[[builtin(position)]] clip_position: vec4<f32>;
	[[location(0)]] tex_coords: vec2<f32>;
	[[location(1)]] world_normal: vec3<f32>;
	[[location(2)]] world_position: vec3<f32>;
};
	"#,
	);
}

#[test]
fn function_decl() {
	super::parse::<FunctionDecl>(
		r#"
fn main() -> [[location(0)]] vec4<f32> {
	return vec4<f32>(0.4, 0.4, 0.8, 1.0);
}
	"#,
	);
	super::parse::<FunctionDecl>(
		r#"
fn main(
	[[builtin(position)]] coord_in: vec4<f32>,
) -> [[location(0)]] vec4<f32> {
	return vec4<f32>(coord_in.x, coord_in.y, 0.0, 1.0);
}
	"#,
	);
	super::parse::<FunctionDecl>(
		r#"
fn mul(a: f32, b: f32) -> f32 {
	return a * b;
}
	"#,
	);
}
