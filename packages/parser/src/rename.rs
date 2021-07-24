use crate::ast::{FunctionDecl, StructDecl, Token};

pub trait Rename {
	fn rename(self, name: String) -> Self;
}

impl Rename for StructDecl {
	fn rename(mut self, name: String) -> Self {
		let (_, range) = self.name.into_inner();
		self.name = Token::Ident(name, range);

		self
	}
}

impl Rename for FunctionDecl {
	fn rename(mut self, name: String) -> Self {
		let (_, range) = self.name.into_inner();
		self.name = Token::Ident(name, range);

		self
	}
}
