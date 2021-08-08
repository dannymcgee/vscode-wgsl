use crate::decl::{StructDecl, TypeAliasDecl, VarDecl};

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
	super::parse::<TypeAliasDecl>("type ViewProjectionMatrix = vec4<f32>;");
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
