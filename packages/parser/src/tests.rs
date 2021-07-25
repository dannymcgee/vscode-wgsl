use super::*;
use pest::Parser as PestParser;

#[test]
fn decimal_float_literal() {
	let results = [
		Parser::parse(Rule::DECIMAL_FLOAT_LITERAL, "1.0"),
		Parser::parse(Rule::DECIMAL_FLOAT_LITERAL, "0.05"),
		Parser::parse(Rule::DECIMAL_FLOAT_LITERAL, "-1.0"),
		Parser::parse(Rule::DECIMAL_FLOAT_LITERAL, ".5"),
		Parser::parse(Rule::DECIMAL_FLOAT_LITERAL, "-.5"),
		Parser::parse(Rule::DECIMAL_FLOAT_LITERAL, "1.0e10"),
		Parser::parse(Rule::DECIMAL_FLOAT_LITERAL, "-1.0e10"),
		Parser::parse(Rule::DECIMAL_FLOAT_LITERAL, "1.0e-10"),
	];

	for result in results {
		assert_ok(result);
	}
}

#[test]
fn hex_float_literal() {
	let result = Parser::parse(Rule::HEX_FLOAT_LITERAL, "0xFF.0Ap-1");
	assert_ok(result);
}

#[test]
fn int_literal() {
	let results = [
		Parser::parse(Rule::INT_LITERAL, "0xFFAA77"),
		Parser::parse(Rule::INT_LITERAL, "42"),
		Parser::parse(Rule::INT_LITERAL, "0"),
	];

	for result in results {
		assert_ok(result);
	}
}

#[test]
fn uint_literal() {
	let results = [
		Parser::parse(Rule::UINT_LITERAL, "0xFFAA77u"),
		Parser::parse(Rule::UINT_LITERAL, "42u"),
		Parser::parse(Rule::UINT_LITERAL, "0u"),
	];

	for result in results {
		assert_ok(result);
	}
}

#[test]
fn attribute() {
	let result = Parser::parse(Rule::attribute, "location(5)");
	assert_ok(result);
}

#[test]
fn attribute_list() {
	let result = Parser::parse(Rule::attribute_list, "[[group(2), binding(0)]]");
	assert_ok(result);
}

#[test]
fn type_decl() {
	let results = [
		Parser::parse(Rule::type_decl, "bool"),
		Parser::parse(Rule::type_decl, "f32"),
		Parser::parse(Rule::type_decl, "i32"),
		Parser::parse(Rule::type_decl, "u32"),
		Parser::parse(Rule::type_decl, "vec2<f32>"),
		Parser::parse(Rule::type_decl, "vec3<f32>"),
		Parser::parse(Rule::type_decl, "vec4<f32>"),
		Parser::parse(Rule::type_decl, "ptr<function, f32>"),
		Parser::parse(Rule::type_decl, "ptr<function, f32, read_write>"),
		Parser::parse(Rule::type_decl, "[[stride(16)]] array<vec4<f32>>"),
		Parser::parse(Rule::type_decl, "array<f32, 16>"),
		Parser::parse(Rule::type_decl, "mat4x4<f32>"),
		Parser::parse(Rule::type_decl, "sampler"),
		Parser::parse(Rule::type_decl, "texture_2d<f32>"),
		Parser::parse(Rule::type_decl, "texture_cube_array<f32>"),
		Parser::parse(Rule::type_decl, "texture_cube<f32>"),
		Parser::parse(Rule::type_decl, "texture_storage_2d<f32>"),
		Parser::parse(Rule::type_decl, "MyCustomType"),
	];

	for result in results {
		assert_ok(result);
	}
}

#[test]
fn struct_decl() {
	let results = [
		Parser::parse(
			Rule::struct_decl,
			"struct Data {
				a: i32;
				b: vec2<f32>;
			};",
		),
		Parser::parse(
			Rule::struct_decl,
			"[[block]]
			struct Light {
				position: vec3<f32>;
				color: vec3<f32>;
			};",
		),
		Parser::parse(
			Rule::struct_decl,
			"struct VertexOutput {
				[[builtin(position)]] clip_position: vec4<f32>;
				[[location(0)]] tex_coords: vec2<f32>;
				[[location(1)]] world_normal: vec3<f32>;
				[[location(2)]] world_position: vec3<f32>;
			};",
		),
	];

	for result in results {
		assert_ok(result);
	}
}

#[test]
fn type_alias() {
	let result = Parser::parse(
		Rule::type_alias,
		"type RTArr = [[stride(16)]] array<vec4<f32>>;",
	);

	assert_ok(result);
}

#[test]
fn global_variable_decl() {
	let results = [
		Parser::parse(
			Rule::global_variable_decl,
			"[[group(0), binding(0)]]
			var<storage, read_write> pbuf: PositionsBuffer;",
		),
		Parser::parse(
			Rule::global_variable_decl,
			"var<workgroup> worklist: array<i32, 10>;",
		),
		Parser::parse(
			Rule::global_variable_decl,
			"[[group(0), binding(0)]] var t_diffuse: texture_2d<f32>;",
		),
	];

	for result in results {
		assert_ok(result);
	}
}

#[test]
fn global_constant_decl() {
	let result = Parser::parse(
		Rule::global_constant_decl,
		"[[override(0)]] let has_point_light: bool = true;",
	);
	assert_ok(result);
}

#[test]
fn compound_expr() {
	let expr = "ambient_color + diffuse_color + specular_color";
	let result = Parser::parse(Rule::expression, expr);

	match result {
		Ok(pairs) => println!("{:#?}", pairs),
		Err(err) => {
			println!("{}", err);
			panic!();
		}
	}
}

#[test]
fn if_stmt() {
	let results = [
		Parser::parse(
			Rule::if_stmt,
			"if (true) {
				discard;
			} else {}",
		),
		Parser::parse(Rule::if_stmt, "if (0 != 1) {}"),
		Parser::parse(
			Rule::if_stmt,
			"if (false) {
				return;
			} elseif (true) {
				return;
			} elseif (2 + 2 == 5) {
				return;
			} else {}",
		),
	];

	for result in results {
		assert_ok(result);
	}
}

#[test]
fn switch_stmt() {
	let result = Parser::parse(
		Rule::switch_stmt,
		"switch (3) {
			case 0, 1: { pos = 0.0; }
			case 2:    { pos = 1.0; fallthrough; }
			case 3:    {}
			default:   { pos = 3.0; }
		}",
	);
	assert_ok(result);
}

#[test]
fn loop_stmt() {
	let results = [
		Parser::parse(
			Rule::loop_stmt,
			"loop {
				if (i == 1) { break; }
				continuing  { i = 1; }
			}",
		),
		Parser::parse(
			Rule::loop_stmt,
			"loop {
				if (i == 0) { continue; }
				break;
			}",
		),
	];

	for result in results {
		assert_ok(result);
	}
}

#[test]
fn for_stmt() {
	let results = [
		Parser::parse(
			Rule::for_stmt,
			"for (var i: i32 = 0; i < 4; i = i + 1) {
				a = a + 2;
			}",
		),
		Parser::parse(Rule::for_stmt, "for(;;) { break; }"),
	];

	for result in results {
		assert_ok(result);
	}
}

#[test]
fn func_call_stmt() {
	let result = Parser::parse(Rule::func_call_stmt, "my_function(42);");
	assert_ok(result);
}

#[test]
fn variable_stmt() {
	let results = [
		Parser::parse(Rule::variable_stmt, "var<uniform> uniforms: Uniforms;"),
		Parser::parse(Rule::variable_stmt, "var out: VertexOutput;"),
		Parser::parse(Rule::variable_stmt, "var x: f32 = 1.0;"),
		Parser::parse(Rule::variable_stmt, "let block_size: i32 = 1024;"),
		Parser::parse(Rule::variable_stmt, "let ambient_strength = 0.05;"),
	];

	for result in results {
		assert_ok(result);
	}
}

#[test]
fn assignment_stmt() {
	let results = [
		Parser::parse(Rule::assignment_stmt, "a = 30"),
		Parser::parse(Rule::assignment_stmt, "person.age = 31"),
		Parser::parse(Rule::assignment_stmt, "*uv_x_ptr = 2.5"),
	];

	for result in results {
		assert_ok(result);
	}
}

#[test]
fn func_decl() {
	let results = [
		Parser::parse(
			Rule::func_decl,
			"fn add_two(i: i32, b: f32) -> i32 {
				return i + 2;
			}",
		),
		Parser::parse(
			Rule::func_decl,
			"[[stage(compute)]]
			fn main() {
				let six: i32 = add_two(4, 5.0);
			}",
		),
	];

	for result in results {
		assert_ok(result);
	}
}

#[test]
fn program() {
	let results = [
		Parser::parse(
			Rule::program,
			"//
// Vert
//
[[block]]
struct Uniforms {
	view_pos: vec4<f32>;
	view_proj: mat4x4<f32>;
};
[[group(0), binding(0)]]
var<uniform> uniforms: Uniforms;

[[block]]
struct Light {
	position: vec3<f32>;
	color: vec3<f32>;
};
[[group(1), binding(0)]]
var<uniform> light: Light;

struct VertexInput {
	[[location(0)]] position: vec3<f32>;
};

struct VertexOutput {
	[[builtin(position)]] clip_position: vec4<f32>;
	[[location(0)]] color: vec3<f32>;
};

[[stage(vertex)]]
fn vert(model: VertexInput) -> VertexOutput {
	let scale = 0.25;
	var out: VertexOutput;

	out.clip_position = uniforms.view_proj
		* vec4<f32>(model.position * scale + light.position, 1.0);
	out.color = light.color;

	return out;
}

//
// Frag
//
[[stage(fragment)]]
fn frag(in: VertexOutput) -> [[location(0)]] vec4<f32> {
	return vec4<f32>(in.color, 1.0);
}
",
		),
		Parser::parse(
			Rule::program,
			"//
// Vertex Shader
//
[[block]]
struct Uniforms {
	view_pos: vec4<f32>;
	view_proj: mat4x4<f32>;
};
[[group(1), binding(0)]]
var<uniform> uniforms: Uniforms;

[[block]]
struct Light {
	position: vec3<f32>;
	color: vec3<f32>;
};
[[group(2), binding(0)]]
var<uniform> light: Light;

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
		instance.model_matrix_3
	);
	let normal_matrix = mat3x3<f32>(
		instance.normal_matrix_0,
		instance.normal_matrix_1,
		instance.normal_matrix_2
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
	let diffuse_color = light.color * diffuse_strength;

	let view_dir = normalize(uniforms.view_pos.xyz - in.world_position);
	let half_dir = normalize(view_dir + light_dir);
	let specular_strength = pow(max(dot(in.world_normal, half_dir), 0.0), 32.0);
	let specular_color = specular_strength * light.color;

	let result = (ambient_color + diffuse_color + specular_color) * object_color.xyz;

	return vec4<f32>(result, object_color.a);
}
",
		),
	];

	for result in results {
		assert_ok(result);
	}
}

#[test]
fn ast_global_var_decl() {
	let results = [
		parse_ast(
			"[[group(0), binding(0)]]
			var<storage, read_write> pbuf: PositionsBuffer;",
		),
		parse_ast("[[location(0)]] var position: vec3<f32> = vec3<f32>(1.0, 0.5, 0.333);"),
	];

	for result in results {
		assert!(result.is_ok());
	}
}

#[test]
fn ast_global_const_decl() {
	let result = parse_ast("[[override(0)]] let has_point_light: bool = true;");
	assert!(result.is_ok());
}

#[test]
fn ast_struct_decl() {
	let results = [
		parse_ast(
			"struct Data {
				a: i32;
				b: vec2<f32>;
			};",
		),
		parse_ast(
			"[[block]]
			struct Light {
				position: vec3<f32>;
				color: vec3<f32>;
			};",
		),
		parse_ast(
			"struct VertexOutput {
				[[builtin(position)]] clip_position: vec4<f32>;
				[[location(0)]] tex_coords: vec2<f32>;
				[[location(1)]] world_normal: vec3<f32>;
				[[location(2)]] world_position: vec3<f32>;
			};",
		),
	];

	for result in results {
		assert!(result.is_ok());
		println!("{:#?}", result.unwrap());
	}
}

#[test]
fn ast_func_decl() {
	let funcs = [
		"fn add_two(i: i32, b: f32) -> i32 {
			return i + 2;
		}",
		"[[stage(compute)]]
		fn main() {
			let six: i32 = add_two(4, 5.0);
		}",
	];

	for func in funcs {
		let pair = Parser::parse(Rule::func_decl, func)
			.unwrap()
			.next()
			.unwrap();

		let decl = pair.parse_func_decl();
		println!("{:#?}", decl);
	}
}

#[test]
fn ast_if_stmt() {
	let stmts = [
		"if (true) {
			discard;
		} else {}",
		"if (0 != 1) {}",
		"if (false) {
			return;
		} elseif (true) {
			return;
		} else {}",
		"if (false) {
			return;
		} elseif (true) {
			return;
		} elseif (2 + 2 == 5) {
			return;
		} else {}",
	];

	for stmt in stmts {
		let result = parse_stmt(stmt);
		assert!(result.is_ok());
		println!("{:#?}", result.unwrap());
	}
}

#[test]
fn ast_switch_stmt() {
	let stmt = "switch (3) {
		case 0, 1: { pos = 0.0; }
		case 2:    { pos = 1.0; fallthrough; }
		case 3:    {}
		default:   { pos = 3.0; }
	}";
	let result = parse_stmt(stmt);

	assert!(result.is_ok());
	println!("{:#?}", result.unwrap());
}

#[test]
fn ast_loop_stmt() {
	let stmts = [
		"loop {
			if (i == 1) { break; }
			continuing  { i = 1; }
		}",
		"loop {
			if (i == 0) { continue; }
			break;
		}",
	];

	for stmt in stmts {
		let result = parse_stmt(stmt);
		assert!(result.is_ok());
		println!("{:#?}", result.unwrap());
	}
}

#[test]
fn ast_for_stmt() {
	let stmts = [
		"for (var i: i32 = 0; i < 4; i = i + 1) {
			a = a + 2;
		}",
		"for(;;) { break; }",
	];

	for stmt in stmts {
		let result = parse_stmt(stmt);
		assert!(result.is_ok());
		println!("{:#?}", result.unwrap());
	}
}

#[test]
fn ast_func_call_stmt() {
	let stmt = "my_function(42);";
	let result = parse_stmt(stmt);

	assert!(result.is_ok());
	println!("{:#?}", result.unwrap());
}

#[test]
fn ast_variable_stmt() {
	let stmts = [
		"var<uniform> uniforms: Uniforms;",
		"var out: VertexOutput;",
		"var x: f32 = 1.0;",
		"let block_size: i32 = 1024;",
		"let ambient_strength = 0.05;",
	];

	for stmt in stmts {
		let result = parse_stmt(stmt);

		assert!(result.is_ok());
		println!("{:#?}", result.unwrap());
	}
}

#[test]
fn ast_assignment_stmt() {
	let stmts = ["a = 30;", "person.age = 31;", "*uv_x_ptr = 2.5;"];

	for stmt in stmts {
		match parse_stmt(stmt) {
			Ok(ast) => println!("{:#?}", ast),
			Err(err) => {
				println!("{}", err);
				panic!();
			}
		}
	}
}

#[test]
fn ast_program_1() {
	let program = "
//
// Vert
//
[[block]]
struct Uniforms {
	view_pos: vec4<f32>;
	view_proj: mat4x4<f32>;
};
[[group(0), binding(0)]]
var<uniform> uniforms: Uniforms;

[[block]]
struct Light {
	position: vec3<f32>;
	color: vec3<f32>;
};
[[group(1), binding(0)]]
var<uniform> light: Light;

struct VertexInput {
	[[location(0)]] position: vec3<f32>;
};

struct VertexOutput {
	[[builtin(position)]] clip_position: vec4<f32>;
	[[location(0)]] color: vec3<f32>;
};

[[stage(vertex)]]
fn vert(model: VertexInput) -> VertexOutput {
	let scale = 0.25;
	var out: VertexOutput;

	out.clip_position = uniforms.view_proj
		* vec4<f32>(model.position * scale + light.position, 1.0);
	out.color = light.color;

	return out;
}

//
// Frag
//
[[stage(fragment)]]
fn frag(in: VertexOutput) -> [[location(0)]] vec4<f32> {
	return vec4<f32>(in.color, 1.0);
}
";
	let result = parse_ast(program);
	assert!(result.is_ok());
}

#[test]
fn ast_program_2() {
	let program = "//
// Vertex Shader
//
[[block]]
struct Uniforms {
	view_pos: vec4<f32>;
	view_proj: mat4x4<f32>;
};
[[group(1), binding(0)]]
var<uniform> uniforms: Uniforms;

[[block]]
struct Light {
	position: vec3<f32>;
	color: vec3<f32>;
};
[[group(2), binding(0)]]
var<uniform> light: Light;

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
		instance.model_matrix_3
	);
	let normal_matrix = mat3x3<f32>(
		instance.normal_matrix_0,
		instance.normal_matrix_1,
		instance.normal_matrix_2
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
	let diffuse_color = light.color * diffuse_strength;

	let view_dir = normalize(uniforms.view_pos.xyz - in.world_position);
	let half_dir = normalize(view_dir + light_dir);
	let specular_strength = pow(max(dot(in.world_normal, half_dir), 0.0), 32.0);
	let specular_color = specular_strength * light.color;

	let result = (ambient_color + diffuse_color + specular_color) * object_color.xyz;

	return vec4<f32>(result, object_color.a);
}
";
	let result = parse_ast(program);

	assert!(result.is_ok());

	println!("{:#?}", result.unwrap());
}

#[test]
fn plus_import() {
	let program = "
enable wgsl_plus;
import common from \"./common\";

[[group(1), binding(0)]]
var<uniform> uniforms: common::Uniforms;
	";

	let result = parse_ast(program);

	assert!(result.is_ok());
	println!("{:#?}", result.unwrap());
}

fn assert_ok(result: Result<Pairs<Rule>>) {
	if let Err(err) = result {
		println!("{}", err);
		panic!();
	}
}
