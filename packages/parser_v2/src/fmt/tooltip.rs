use std::fmt;

use itertools::Itertools;

use crate::decl::{
	Decl, ExtensionDecl, FieldDecl, FunctionDecl, ModuleDecl, ParamDecl, StructBody, StructDecl,
	TypeAliasDecl, VarDecl,
};

pub trait Tooltip {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result;
}

impl<'a> Tooltip for Decl<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		use Decl::*;

		match self {
			Var(inner) | Const(inner) => Tooltip::fmt(inner, f),
			TypeAlias(inner) => Tooltip::fmt(inner, f),
			Struct(inner) => Tooltip::fmt(inner, f),
			Field(inner) => Tooltip::fmt(inner, f),
			Function(inner) => Tooltip::fmt(inner, f),
			Param(inner) => Tooltip::fmt(inner, f),
			Extension(inner) => Tooltip::fmt(inner, f),
			Module(inner) => Tooltip::fmt(inner, f),
		}
	}
}

impl<'a> Tooltip for VarDecl<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		if let Some(ref attributes) = self.attributes {
			writeln!(f, "{}", attributes)?;
		}

		write!(f, "{}", self.storage)?;
		if let Some(ref qual) = self.storage_qualifiers {
			write!(f, "<{}>", qual.iter().join(", "))?;
		}

		write!(f, " {}", self.name)?;
		if let Some(ref ty) = self.ty {
			write!(f, ": {}", ty)?;
		}

		Ok(())
	}
}

impl<'a> Tooltip for TypeAliasDecl<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "TODO")
	}
}

impl<'a> Tooltip for StructDecl<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		if let Some(ref attributes) = self.attributes {
			writeln!(f, "{}", attributes)?;
		}

		write!(f, "{}", self.storage)?;
		if let Some(ref modifier) = self.storage_modifier {
			write!(f, "<{}>", modifier)?;
		}

		write!(f, " {} ", self.name)?;

		Tooltip::fmt(&self.body, f)
	}
}

impl<'a> Tooltip for StructBody<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		writeln!(f, "{{")?;

		for field in self.fields.iter() {
			if let Decl::Field(ref field) = field {
				writeln!(f, "\t{}", field)?;
			}
		}

		write!(f, "}};")
	}
}

impl<'a> Tooltip for FieldDecl<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", self)
	}
}

impl<'a> Tooltip for FunctionDecl<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		if let Some(ref attributes) = self.attributes {
			writeln!(f, "{} ", attributes)?;
		}

		write!(f, "{}", self.storage)?;
		if let Some(ref modifier) = self.storage_modifier {
			write!(f, "<{}>", modifier)?;
		}

		write!(f, " {}(", self.name)?;
		for (idx, param) in self.params.iter().enumerate() {
			Tooltip::fmt(param, f)?;
			if idx < self.params.len() - 1 {
				write!(f, ", ")?;
			}
		}
		write!(f, ")")?;

		if let Some(ref ty) = self.return_ty {
			write!(f, " -> {}", ty)?;
		}

		Ok(())
	}
}

impl<'a> Tooltip for ParamDecl<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		if let Some(ref attributes) = self.attributes {
			writeln!(f, "{} ", attributes)?;
		}

		write!(f, "{}: {}", self.name, self.ty)
	}
}

impl<'a> Tooltip for ExtensionDecl<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "TODO")
	}
}

impl<'a> Tooltip for ModuleDecl<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "import {} from {};", self.name, self.path)
	}
}
