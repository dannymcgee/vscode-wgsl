use super::*;

pub trait Pretty {
	fn pretty(&self) -> String;
}

impl Pretty for Range {
	fn pretty(&self) -> String {
		format!(
			"{}:{} .. {}:{}",
			self.start.line, self.start.character, self.end.line, self.end.character
		)
	}
}

impl fmt::Display for Token {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use Token::*;

		let text = match self {
			Ident(text, _) => text,
			Attr(text, _) => text,
			Field(text, _) => text,
			Type(text, _) => text,
			Function(text, _) => text,
			Keyword(text, _) => text,
			Punct(text, _) => text,
			Op(text, _) => text,
			Literal(text, _) => text,
			Module(text, _) => text,
		};

		write!(f, "{}", text)
	}
}

impl fmt::Display for Decl {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use Decl::*;

		match self {
			Var(decl) | Const(decl) => write!(f, "{}", decl),
			TypeAlias(decl) => write!(f, "{}", decl),
			Struct(decl) => write!(f, "{}", decl),
			Field(decl) => write!(f, "{}", decl),
			Function(decl) => write!(f, "{}", decl),
			Param(decl) => write!(f, "{}", decl),
			Extension(decl) => write!(f, "{}", decl),
			Module(decl) => write!(f, "{}", decl),
		}
	}
}

impl fmt::Display for TypeDecl {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		if let Some(ref attributes) = self.attributes {
			write!(f, "{} ", attributes.pretty())?;
		}

		if let Some(ref namespace) = self.namespace {
			write!(f, "{}::", namespace)?;
		}

		write!(f, "{}", self.name)?;

		if self.storage_class.is_some()
			|| self.component_type.is_some()
			|| self.access_mode.is_some()
		{
			write!(f, "<")?;

			if let Some(ref storage_class) = self.storage_class {
				write!(f, "{}, ", storage_class)?;
			}
			if let Some(ref component_type) = self.component_type {
				write!(f, "{}", component_type)?;
			}
			if let Some(ref access_mode) = self.access_mode {
				write!(f, ", {}", access_mode)?;
			}

			write!(f, ">")?;
		}

		Ok(())
	}
}

impl fmt::Display for Attribute {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.name)?;

		if let Some(ref value) = self.value {
			write!(f, "({})", value)?;
		}

		Ok(())
	}
}

impl Pretty for Vec<Attribute> {
	fn pretty(&self) -> String {
		format!(
			"[[{}]]",
			self.iter().map(|attr| attr.to_string()).join(", ")
		)
	}
}

impl fmt::Display for VarDecl {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		if let Some(ref attributes) = self.attributes {
			writeln!(f, "{}", attributes.pretty())?;
		}
		write!(f, "{}", self.storage)?;

		if let Some(ref qualifiers) = self.storage_qualifiers {
			write!(
				f,
				"<{}>",
				qualifiers.iter().map(|q| q.to_string()).join(", ")
			)?;
		}

		write!(f, " {}", self.name)?;

		if let Some(ref ty) = self.type_decl {
			write!(f, ": {}", ty)?;
		}
		if let Some(ref assignment) = self.assignment {
			write!(f, " = {}", assignment)?;
		}

		write!(f, ";")
	}
}

impl fmt::Display for TypeAliasDecl {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{} {} = {}", self.storage, self.name, self.value)
	}
}

impl fmt::Display for StructDecl {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		if let Some(ref attributes) = self.attributes {
			writeln!(f, "{}", attributes.pretty())?;
		}

		write!(f, "{}", self.storage)?;
		if let Some(ref storage_modifier) = self.storage_modifier {
			write!(f, "<{}>", storage_modifier)?;
		}

		writeln!(f, " {} {{", self.name)?;

		for field in &self.body {
			writeln!(f, "\t{};", field)?;
		}

		write!(f, "}};")
	}
}

impl fmt::Display for StructField {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		if let Some(ref attributes) = self.attributes {
			write!(f, "{} ", attributes.pretty())?;
		}

		write!(f, "{}: {}", self.name, self.type_decl)
	}
}

impl fmt::Display for FunctionDecl {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		if let Some(ref attributes) = self.attributes {
			writeln!(f, "{}", attributes.pretty())?;
		}
		writeln!(f, "{} {}{} {{", self.storage, self.name, self.signature)?;

		let (_, stmts) = &self.body;
		for stmt in stmts {
			writeln!(f, "\t{}", stmt)?;
		}

		write!(f, "}}")
	}
}

impl fmt::Display for FunctionSignature {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(
			f,
			"({})",
			self.params.iter().map(|p| p.to_string()).join(", ")
		)?;

		if let Some(ref ty) = self.return_type {
			write!(f, " -> {}", ty)?;
		}

		Ok(())
	}
}

impl fmt::Display for FunctionParam {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		if let Some(ref attributes) = self.attributes {
			write!(f, "{} ", attributes.pretty())?;
		}

		write!(f, "{}: {}", self.name, self.type_decl)
	}
}

impl fmt::Display for ExtensionDecl {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{} {};", self.keyword, self.name)
	}
}

impl fmt::Display for ModuleDecl {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(
			f,
			"{} {} {} {};",
			self.import_keyword, self.name, self.from_keyword, self.path
		)
	}
}

impl fmt::Display for Stmt {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use Stmt::*;

		match self {
			Return(stmt) => write!(f, "{}", stmt),
			If(stmt) => write!(f, "{}", stmt),
			Switch(stmt) => write!(f, "{}", stmt),
			Loop(stmt) => write!(f, "{}", stmt),
			Continuing(stmt) => write!(f, "{}", stmt),
			For(stmt) => write!(f, "{}", stmt),
			FunctionCall(stmt) => write!(f, "{};", stmt),
			Variable(stmt) => write!(f, "{}", stmt),
			Break(token) | Continue(token) | Discard(token) | Fallthrough(token) => {
				write!(f, "{};", token)
			}
			Assignment(stmt) => write!(f, "{};", stmt),
		}
	}
}

impl fmt::Display for ReturnStmt {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.keyword)?;

		if let Some(ref expr) = self.expr {
			write!(f, " {}", expr)?;
		}

		write!(f, ";")
	}
}

impl fmt::Display for IfStmt {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		writeln!(f, "{} ({}) {{", self.keyword, self.condition)?;

		for stmt in self.body.iter() {
			writeln!(f, "\t\t{}", stmt)?;
		}

		write!(f, "\t}}")?;

		if let Some(ref elseif) = self.elseif {
			write!(f, "{}", elseif)?;
		}
		if let Some(ref else_stmt) = self.else_stmt {
			write!(f, "{}", else_stmt)?;
		}

		Ok(())
	}
}

impl fmt::Display for ElseifStmt {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		writeln!(f, " {} ({}) {{", self.keyword, self.condition)?;

		for stmt in self.body.iter() {
			writeln!(f, "\t\t{}", stmt)?;
		}

		write!(f, "\t}}")?;

		if let Some(ref elseif) = self.elseif {
			write!(f, "{}", elseif)?;
		}

		Ok(())
	}
}

impl fmt::Display for ElseStmt {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		writeln!(f, " {} {{", self.keyword)?;

		for stmt in self.body.iter() {
			writeln!(f, "\t\t{}", stmt)?;
		}

		write!(f, "\t}}")
	}
}

impl fmt::Display for SwitchStmt {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		writeln!(f, "{} ({}) {{", self.keyword, self.subject)?;

		for stmt in self.body.iter() {
			writeln!(f, "\t\t{}", stmt)?;
		}

		Ok(())
	}
}

impl fmt::Display for CaseStmt {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.keyword)?;

		if let Some(ref selectors) = self.selectors {
			write!(f, " {}", selectors.iter().map(|s| s.to_string()).join(", "))?;
		}

		writeln!(f, ": {{")?;

		for stmt in self.body.iter() {
			writeln!(f, "\t\t\t{}", stmt)?;
		}

		write!(f, "\t\t}}")
	}
}

impl fmt::Display for LoopStmt {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		writeln!(f, "{} {{", self.keyword)?;

		for stmt in self.body.iter() {
			writeln!(f, "\t\t{}", stmt)?;
		}

		if let Some(ref continuing) = self.continuing {
			write!(f, "{}", continuing)?;
		}

		write!(f, "\t}}")
	}
}

impl fmt::Display for ContinuingStmt {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		writeln!(f, "\t\t{} {{", self.keyword)?;

		for stmt in self.body.iter() {
			writeln!(f, "\t\t\t{}", stmt)?;
		}

		writeln!(f, "\t\t}}")
	}
}

impl fmt::Display for ForStmt {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		writeln!(f, "{} ({}) {{", self.keyword, self.header)?;

		for stmt in self.body.iter() {
			writeln!(f, "\t\t{}", stmt)?;
		}

		write!(f, "\t}}")
	}
}

impl fmt::Display for ForHeader {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		if let Some(ref init) = self.init {
			write!(f, "{}", init)?;
		} else {
			write!(f, ";")?;
		}

		if let Some(ref condition) = self.condition {
			write!(f, " {};", condition)?;
		} else {
			write!(f, ";")?;
		}

		if let Some(ref iterator) = self.iterator {
			write!(f, " {}", iterator)?;
		}

		Ok(())
	}
}

impl fmt::Display for AssignmentStmt {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.lhs.clone())?;
		write!(f, " {} ", self.op)?;
		write!(f, "{}", self.rhs.clone())
	}
}

impl fmt::Display for Expr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use Expr::*;

		match self {
			Singular(expr) => write!(f, "{}", expr),
			Binary(expr) => write!(f, "{}", expr),
		}
	}
}

impl fmt::Display for SingularExpr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		if let Some(ref prefix) = self.prefix {
			for token in prefix {
				write!(f, "{}", token)?;
			}
		}

		write!(f, "{}", self.expr)?;

		if let Some(ref postfix) = self.postfix {
			write!(f, "{}", postfix)?;
		}

		Ok(())
	}
}

impl fmt::Display for PrimaryExpr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use PrimaryExpr::*;

		match self {
			TypeCtor(expr) => write!(f, "{}", expr),
			Literal(token) | Identifier(token) => write!(f, "{}", token),
			Paren(expr) => write!(f, "({})", expr),
			Bitcast(expr) => write!(f, "{}", expr),
			FunctionCall(expr) => write!(f, "{}", expr),
		}
	}
}

impl fmt::Display for TypeCtorExpr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(
			f,
			"{}({})",
			self.ty,
			self.args.iter().map(|arg| arg.to_string()).join(", ")
		)
	}
}

impl fmt::Display for BitcastExpr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}<{}>({})", self.keyword, self.ty, self.expr)
	}
}

impl fmt::Display for FunctionCallExpr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(
			f,
			"{}({})",
			self.ident,
			self.args.iter().map(|arg| arg.to_string()).join(", ")
		)
	}
}

impl fmt::Display for PostfixExpr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use PostfixExpr::*;

		let inner = match &self {
			Bracket { expr, postfix, .. } => {
				write!(f, "[{}]", expr)?;
				postfix
			}
			Dot { ident, postfix, .. } => {
				write!(f, ".{}", ident)?;
				postfix
			}
		};

		if let Some(postfix) = inner {
			write!(f, "{}", postfix)?;
		}

		Ok(())
	}
}

impl fmt::Display for BinaryExpr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.lhs)?;
		write!(f, " {} ", self.op)?;
		write!(f, "{}", self.rhs)
	}
}
