use super::*;

impl fmt::Debug for Token {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use Token::*;

		let (name, inner, range) = match self {
			Ident(inner, range) => ("Ident", inner, range),
			Attr(inner, range) => ("Attr", inner, range),
			Field(inner, range) => ("Field", inner, range),
			Type(inner, range) => ("Type", inner, range),
			Function(inner, range) => ("Function", inner, range),
			Keyword(inner, range) => ("Keyword", inner, range),
			Punct(inner, range) => ("Punct", inner, range),
			Op(inner, range) => ("Op", inner, range),
			Literal(inner, range) => ("Literal", inner, range),
			Module(inner, range) => ("Module", inner, range),
		};

		write!(f, "`{}` | Token::{} [{}]", inner, name, range.pretty())
	}
}

impl fmt::Debug for TypeDecl {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let mut debug = f.debug_struct("TypeDecl");
		debug.field("range", &self.range.pretty());

		if let Some(ref attr) = self.attributes {
			debug.field("attributes", attr);
		}

		if let Some(ref namespace) = self.namespace {
			debug.field("namespace", namespace);
		}

		debug.field("name", &self.name);

		if let Some(ref ty) = self.component_type {
			debug.field("component_type", ty);
		}

		if let Some(ref sc) = self.storage_class {
			debug.field("storage_class", sc);
		}

		if let Some(ref am) = self.access_mode {
			debug.field("access_mode", am);
		}

		debug.finish()
	}
}

impl fmt::Debug for Attribute {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let mut debug = f.debug_struct("Attribute");
		debug.field("range", &self.range.pretty());
		debug.field("name", &self.name);

		if let Some(ref value) = self.value {
			debug.field("value", value);
		}

		debug.finish()
	}
}

impl fmt::Debug for VarDecl {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let mut debug = f.debug_struct("VarDecl");
		debug.field("range", &self.range.pretty());

		if let Some(ref attr) = self.attributes {
			debug.field("attributes", attr);
		}
		debug.field("storage", &self.storage);
		if let Some(ref qual) = self.storage_qualifiers {
			debug.field("storage_qualifiers", qual);
		}
		debug.field("name", &self.name);
		if let Some(ref ty) = self.type_decl {
			debug.field("type_decl", ty);
		}
		if let Some(ref assignment) = self.assignment {
			debug.field("assignment", assignment);
		}

		debug.finish()
	}
}

impl fmt::Debug for TypeAliasDecl {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.debug_struct("TypeAliasDecl")
			.field("range", &self.range.pretty())
			.field("storage", &self.storage)
			.field("name", &self.name)
			.field("value", &self.value)
			.finish()
	}
}

impl fmt::Debug for StructDecl {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let mut debug = f.debug_struct("StructDecl");
		debug.field("range", &self.range.pretty());

		if let Some(ref attr) = self.attributes {
			debug.field("attributes", attr);
		}

		debug.field("storage", &self.storage);

		if let Some(ref storage_modifier) = self.storage_modifier {
			debug.field("storage_modifier", storage_modifier);
		}

		debug
			.field("name", &self.name)
			.field("body", &self.body)
			.finish()
	}
}

impl fmt::Debug for StructField {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let mut debug = f.debug_struct("StructField");
		debug.field("range", &self.range.pretty());

		if let Some(ref attr) = self.attributes {
			debug.field("attributes", attr);
		}

		debug
			.field("name", &self.name)
			.field("type_decl", &self.type_decl)
			.finish()
	}
}

impl fmt::Debug for FunctionDecl {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let mut debug = f.debug_struct("FunctionDecl");
		debug.field("range", &self.range.pretty());

		if let Some(ref attr) = self.attributes {
			debug.field("attributes", attr);
		}

		debug
			.field("storage", &self.storage)
			.field("name", &self.name)
			.field("signature", &self.signature)
			.field("body", &self.body)
			.finish()
	}
}

impl fmt::Debug for FunctionSignature {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let mut debug = f.debug_struct("FunctionSignature");
		debug.field("range", &self.range.pretty());
		debug.field("params", &self.params);

		if let Some(ref ty) = self.return_type {
			debug.field("return_type", &ty);
		}

		debug.finish()
	}
}

impl fmt::Debug for FunctionParam {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let mut debug = f.debug_struct("FunctionParam");
		debug.field("range", &self.range.pretty());

		if let Some(ref attr) = self.attributes {
			debug.field("attributes", attr);
		}

		debug
			.field("name", &self.name)
			.field("type_decl", &self.type_decl)
			.finish()
	}
}

impl fmt::Debug for ExtensionDecl {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.debug_struct("ExtensionDecl")
			.field("keyword", &self.keyword)
			.field("name", &self.name)
			.field("range", &self.range.pretty())
			.finish()
	}
}

impl fmt::Debug for ModuleDecl {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.debug_struct("ModuleDecl")
			.field("import_keyword", &self.import_keyword)
			.field("name", &self.name)
			.field("from_keyword", &self.from_keyword)
			.field("path", &self.path)
			.field("range", &self.range.pretty())
			.finish()
	}
}

impl fmt::Debug for Stmt {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use Stmt::*;

		match self {
			Return(inner) => write!(f, "Stmt::Return::{:#?}", inner),
			If(inner) => write!(f, "Stmt::If::{:#?}", inner),
			Switch(inner) => write!(f, "Stmt::Switch::{:#?}", inner),
			Loop(inner) => write!(f, "Stmt::Loop::{:#?}", inner),
			Continuing(inner) => write!(f, "Stmt::Continuing::{:#?}", inner),
			For(inner) => write!(f, "Stmt::For::{:#?}", inner),
			FunctionCall(inner) => write!(f, "Stmt::FunctionCall::{:#?}", inner),
			Variable(inner) => write!(f, "Stmt::Variable::{:#?}", inner),
			Break(inner) => write!(f, "Stmt::Break( {:#?} )", inner),
			Continue(inner) => write!(f, "Stmt::Continue( {:#?} )", inner),
			Discard(inner) => write!(f, "Stmt::Discard( {:#?} )", inner),
			Fallthrough(inner) => write!(f, "Stmt::Fallthrough( {:#?} )", inner),
			Assignment(inner) => write!(f, "Stmt::Assignment::{:#?}", inner),
		}
	}
}

impl fmt::Debug for ReturnStmt {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let mut debug = f.debug_struct("ReturnStmt");
		debug.field("range", &self.range.pretty());
		debug.field("keyword", &self.keyword);
		if let Some(ref expr) = self.expr {
			debug.field("expr", expr);
		}

		debug.finish()
	}
}

impl fmt::Debug for Expr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use Expr::*;

		match self {
			Singular(inner) => write!(f, "Expr::Singular::{:#?}", inner),
			Binary(inner) => write!(f, "Expr::Binary::{:#?}", inner),
		}
	}
}

impl fmt::Debug for PrimaryExpr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use PrimaryExpr::*;

		match self {
			TypeCtor(inner) => write!(f, "PrimaryExpr::TypeCtor::{:#?}", inner),
			Literal(inner) => write!(f, "PrimaryExpr::Literal( {:#?} )", inner),
			Paren(inner) => write!(f, "PrimaryExpr::Paren::{:#?}", inner),
			Bitcast(inner) => write!(f, "PrimaryExpr::Bitcast::{:#?}", inner),
			Identifier(inner) => write!(f, "PrimaryExpr::Identifier( {:#?} )", inner),
			FunctionCall(inner) => write!(f, "PrimaryExpr::FunctionCall::{:#?}", inner),
		}
	}
}

impl fmt::Debug for SingularExpr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let mut debug = f.debug_struct("SingularExpr");
		debug.field("range", &self.range.pretty());

		if let Some(ref pre) = self.prefix {
			debug.field("prefix", pre);
		}
		debug.field("expr", &self.expr);
		if let Some(ref post) = self.postfix {
			debug.field("postfix", post);
		}

		debug.finish()
	}
}
