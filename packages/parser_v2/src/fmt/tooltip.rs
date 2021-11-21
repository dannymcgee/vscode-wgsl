use std::fmt;

use itertools::Itertools;

use crate::decl::{
	Decl, ExtensionDecl, FieldDecl, FunctionDecl, ModuleDecl, ParamDecl, StructBody, StructDecl,
	TypeAliasDecl, VarDecl,
};

pub trait Tooltip {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result;
}

impl Tooltip for Decl {
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

impl Tooltip for VarDecl {
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

impl Tooltip for TypeAliasDecl {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "TODO")
	}
}

impl Tooltip for StructDecl {
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

impl Tooltip for StructBody {
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

impl Tooltip for FieldDecl {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", self)
	}
}

impl Tooltip for FunctionDecl {
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

impl Tooltip for ParamDecl {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		if let Some(ref attributes) = self.attributes {
			writeln!(f, "{} ", attributes)?;
		}

		write!(f, "{}: {}", self.name, self.ty)
	}
}

impl Tooltip for ExtensionDecl {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "TODO")
	}
}

impl Tooltip for ModuleDecl {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "import {} from {};", self.name, self.path)
	}
}
