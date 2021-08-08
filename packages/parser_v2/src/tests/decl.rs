use crate::decl::VarDecl;

#[test]
fn var_decl() {
	super::parse::<VarDecl>("let foo = 1.0;");
	super::parse::<VarDecl>("var foo: f32 = 1.0;");
	super::parse::<VarDecl>("var foo: f32;");
	super::parse::<VarDecl>("var<uniform> uniforms: Uniforms;");
	super::parse::<VarDecl>("var<uniform> uniforms: common::Uniforms;");
}
