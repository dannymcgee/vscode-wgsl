use crate::{
	ast::{FunctionDecl, StructDecl, Token},
	GetRange,
};

pub trait Rename {
	fn rename(&mut self, name: String);
}

impl Rename for StructDecl {
	fn rename(&mut self, name: String) {
		let range = self.name.range();
		self.name = Token::Ident(name, range);
	}
}

impl Rename for FunctionDecl {
	fn rename(&mut self, name: String) {
		let range = self.name.range();
		self.name = Token::Ident(name, range);
	}
}
