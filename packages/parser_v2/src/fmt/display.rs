use std::fmt;

use itertools::Itertools;

use crate::{
	common::{Attribute, AttributeList, TypeDecl},
	decl::{Decl, FieldDecl},
	expr::IdentExpr,
};

use super::Tooltip;

impl<'a> fmt::Display for TypeDecl<'a> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		if let Some(namespace) = self.name.namespace {
			write!(f, "{}::", namespace)?;
		}
		write!(f, "{}", self.name.name)?;

		if let Some(child_ty) = self.child_ty {
			write!(f, "<{}>", child_ty)?;
		}

		Ok(())
	}
}

impl<'a> fmt::Display for Attribute<'a> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.name)?;

		if let Some(value) = self.value {
			write!(f, "({})", value)?;
		}

		Ok(())
	}
}

impl<'a> fmt::Display for AttributeList<'a> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let inner = self.attributes.iter().join(", ");

		write!(f, "[[{}]]", inner)
	}
}

// FIXME
impl<'a> fmt::Display for Decl<'a> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		Tooltip::fmt(self, f)
	}
}

impl<'a> fmt::Display for FieldDecl<'a> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		if let Some(ref attributes) = self.attributes {
			write!(f, "{} ", attributes)?;
		}
		write!(f, "{}: {};", self.name, self.ty)
	}
}

impl<'a> fmt::Display for IdentExpr<'a> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		if let Some(ref namespace) = self.namespace {
			write!(f, "{}::", namespace)?;
		}
		write!(f, "{}", self.name)
	}
}
