use std::fmt;

use itertools::Itertools;

use crate::{
	common::{Attribute, AttributeList, TypeDecl},
	decl::{Decl, FieldDecl},
	expr::IdentExpr,
};

use super::Tooltip;

impl fmt::Display for TypeDecl {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		if let Some(ref namespace) = self.name.namespace {
			write!(f, "{}::", namespace)?;
		}
		write!(f, "{}", self.name.name)?;

		if let Some(ref child_ty) = self.child_ty {
			write!(f, "<{}>", child_ty)?;
		}

		Ok(())
	}
}

impl fmt::Display for Attribute {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "@{}", self.name)?;

		// TODO
		// if let Some(ref params) = self.params {
		// 	write!(f, "{}", params)?;
		// }

		Ok(())
	}
}

impl fmt::Display for AttributeList {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let inner = self.attributes.iter().join(" ");

		write!(f, "{}", inner)
	}
}

// FIXME
impl fmt::Display for Decl {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		Tooltip::fmt(self, f)
	}
}

impl fmt::Display for FieldDecl {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		if let Some(ref attributes) = self.attributes {
			write!(f, "{} ", attributes)?;
		}
		write!(f, "{}: {};", self.name, self.ty)
	}
}

impl fmt::Display for IdentExpr {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		if let Some(ref namespace) = self.namespace {
			write!(f, "{}::", namespace)?;
		}
		write!(f, "{}", self.name)
	}
}
