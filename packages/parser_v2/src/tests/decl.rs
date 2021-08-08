use std::fmt;

use gramatika::{Parse, ParseStream, ParseStreamer};

use crate::decl::VarDecl;

#[test]
fn var_decl() {
	parse::<VarDecl>("let foo = 1.0;");
	parse::<VarDecl>("var foo: f32 = 1.0;");
	parse::<VarDecl>("var foo: f32;");
	parse::<VarDecl>("var<uniform> uniforms: Uniforms;");
	parse::<VarDecl>("var<uniform> uniforms: common::Uniforms;");
}

fn parse<'a, P>(input: &'a str)
where P: Parse<'a, Stream = crate::ParseStream<'a>> + fmt::Debug {
	match ParseStream::from(input).parse::<P>() {
		Ok(tree) => eprintln!("{:#?}", tree),
		Err(err) => {
			eprintln!("{}", err);
			panic!();
		}
	}
}
