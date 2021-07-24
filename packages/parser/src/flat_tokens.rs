use crate::ast::*;

pub trait FlatTokens {
	fn flat_tokens(&self, tokens: &mut Vec<Token>);
}

impl FlatTokens for Vec<Decl> {
	fn flat_tokens(&self, tokens: &mut Vec<Token>) {
		for decl in self {
			decl.flat_tokens(tokens);
		}
	}
}

impl FlatTokens for Decl {
	fn flat_tokens(&self, tokens: &mut Vec<Token>) {
		use Decl::*;

		match self {
			Var(inner) | Const(inner) => inner.flat_tokens(tokens),
			TypeAlias(inner) => inner.flat_tokens(tokens),
			Struct(inner) => inner.flat_tokens(tokens),
			Field(inner) => inner.flat_tokens(tokens),
			Function(inner) => inner.flat_tokens(tokens),
			Param(inner) => inner.flat_tokens(tokens),
			Extension(inner) => inner.flat_tokens(tokens),
			Module(inner) => inner.flat_tokens(tokens),
		}
	}
}

impl FlatTokens for Vec<Attribute> {
	fn flat_tokens(&self, tokens: &mut Vec<Token>) {
		for attr in self {
			tokens.push(attr.name.clone());

			if let Some(ref value) = attr.value {
				tokens.push(value.clone());
			}
		}
	}
}

impl FlatTokens for VarDecl {
	fn flat_tokens(&self, tokens: &mut Vec<Token>) {
		if let Some(ref attributes) = self.attributes {
			attributes.flat_tokens(tokens);
		}

		tokens.push(self.storage.clone());

		if let Some(ref qualifiers) = self.storage_qualifiers {
			for token in qualifiers {
				tokens.push(token.clone());
			}
		}

		tokens.push(self.name.clone());

		if let Some(ref ty) = self.type_decl {
			ty.flat_tokens(tokens);
		}

		if let Some(ref expr) = self.assignment {
			expr.flat_tokens(tokens);
		}
	}
}

impl FlatTokens for TypeDecl {
	fn flat_tokens(&self, tokens: &mut Vec<Token>) {
		if let Some(ref attr) = self.attributes {
			attr.flat_tokens(tokens);
		}

		if let Some(ref access_mode) = self.access_mode {
			tokens.push(access_mode.clone());
		}

		if let Some(ref namespace) = self.namespace {
			tokens.push(namespace.clone());
		}

		tokens.push(self.name.clone());

		if let Some(ref inner) = self.component_type {
			inner.flat_tokens(tokens);
		}
	}
}

impl FlatTokens for TypeAliasDecl {
	fn flat_tokens(&self, tokens: &mut Vec<Token>) {
		tokens.push(self.storage.clone());
		tokens.push(self.name.clone());
		self.value.flat_tokens(tokens);
	}
}

impl FlatTokens for StructDecl {
	fn flat_tokens(&self, tokens: &mut Vec<Token>) {
		if let Some(ref attributes) = self.attributes {
			attributes.flat_tokens(tokens);
		}

		tokens.push(self.storage.clone());
		tokens.push(self.name.clone());

		for field in &self.body {
			field.flat_tokens(tokens);
		}
	}
}

impl FlatTokens for StructField {
	fn flat_tokens(&self, tokens: &mut Vec<Token>) {
		if let Some(ref attr) = self.attributes {
			attr.flat_tokens(tokens);
		}
		tokens.push(self.name.clone());
		self.type_decl.flat_tokens(tokens);
	}
}

impl FlatTokens for FunctionDecl {
	fn flat_tokens(&self, tokens: &mut Vec<Token>) {
		if let Some(ref attr) = self.attributes {
			attr.flat_tokens(tokens);
		}

		tokens.push(self.storage.clone());
		tokens.push(self.name.clone());

		for param in &self.signature.params {
			param.flat_tokens(tokens);
		}

		if let Some(ref ty) = self.signature.return_type {
			ty.flat_tokens(tokens);
		}

		for stmt in &self.body.1 {
			stmt.flat_tokens(tokens);
		}
	}
}

impl FlatTokens for FunctionParam {
	fn flat_tokens(&self, tokens: &mut Vec<Token>) {
		if let Some(ref attr) = self.attributes {
			attr.flat_tokens(tokens);
		}

		tokens.push(self.name.clone());
		self.type_decl.flat_tokens(tokens);
	}
}

impl FlatTokens for ExtensionDecl {
	fn flat_tokens(&self, tokens: &mut Vec<Token>) {
		tokens.push(self.keyword.clone());
		tokens.push(self.name.clone());
	}
}

impl FlatTokens for ModuleDecl {
	fn flat_tokens(&self, tokens: &mut Vec<Token>) {
		tokens.push(self.import_keyword.clone());
		tokens.push(self.name.clone());
		tokens.push(self.from_keyword.clone());
		tokens.push(self.path.clone());
	}
}

impl FlatTokens for Stmt {
	fn flat_tokens(&self, tokens: &mut Vec<Token>) {
		use Stmt::*;

		match self {
			Return(stmt) => stmt.flat_tokens(tokens),
			If(stmt) => stmt.flat_tokens(tokens),
			Switch(stmt) => stmt.flat_tokens(tokens),
			Loop(stmt) => stmt.flat_tokens(tokens),
			Continuing(stmt) => stmt.flat_tokens(tokens),
			For(stmt) => stmt.flat_tokens(tokens),
			FunctionCall(stmt) => stmt.flat_tokens(tokens),
			Variable(stmt) => stmt.flat_tokens(tokens),
			Break(stmt) => tokens.push(stmt.clone()),
			Continue(stmt) => tokens.push(stmt.clone()),
			Discard(stmt) => tokens.push(stmt.clone()),
			Fallthrough(stmt) => tokens.push(stmt.clone()),
			Assignment(stmt) => stmt.flat_tokens(tokens),
		}
	}
}

impl FlatTokens for ReturnStmt {
	fn flat_tokens(&self, tokens: &mut Vec<Token>) {
		tokens.push(self.keyword.clone());

		if let Some(ref expr) = self.expr {
			expr.flat_tokens(tokens);
		}
	}
}

impl FlatTokens for IfStmt {
	fn flat_tokens(&self, tokens: &mut Vec<Token>) {
		tokens.push(self.keyword.clone());
		self.condition.flat_tokens(tokens);

		for stmt in &self.body {
			stmt.flat_tokens(tokens);
		}

		if let Some(ref elseif) = self.elseif {
			tokens.push(elseif.keyword.clone());
			elseif.condition.flat_tokens(tokens);

			for stmt in &elseif.body {
				stmt.flat_tokens(tokens);
			}
		}

		if let Some(ref else_stmt) = self.else_stmt {
			tokens.push(else_stmt.keyword.clone());

			for stmt in &else_stmt.body {
				stmt.flat_tokens(tokens);
			}
		}
	}
}

impl FlatTokens for SwitchStmt {
	fn flat_tokens(&self, tokens: &mut Vec<Token>) {
		tokens.push(self.keyword.clone());
		self.subject.flat_tokens(tokens);

		for stmt in &self.body {
			tokens.push(stmt.keyword.clone());

			if let Some(ref selectors) = stmt.selectors {
				for selector in selectors {
					tokens.push(selector.clone());
				}
			}

			for stmt in &stmt.body {
				stmt.flat_tokens(tokens);
			}
		}
	}
}

impl FlatTokens for LoopStmt {
	fn flat_tokens(&self, tokens: &mut Vec<Token>) {
		tokens.push(self.keyword.clone());

		for stmt in &self.body {
			stmt.flat_tokens(tokens);
		}

		if let Some(ref stmt) = self.continuing {
			stmt.flat_tokens(tokens);
		}
	}
}

impl FlatTokens for ContinuingStmt {
	fn flat_tokens(&self, tokens: &mut Vec<Token>) {
		tokens.push(self.keyword.clone());

		for stmt in &self.body {
			stmt.flat_tokens(tokens);
		}
	}
}

impl FlatTokens for ForStmt {
	fn flat_tokens(&self, tokens: &mut Vec<Token>) {
		tokens.push(self.keyword.clone());

		if let Some(ref init) = self.header.init {
			init.flat_tokens(tokens);
		}
		if let Some(ref condition) = self.header.condition {
			condition.flat_tokens(tokens);
		}
		if let Some(ref iterator) = self.header.iterator {
			iterator.flat_tokens(tokens);
		}

		for stmt in &self.body {
			stmt.flat_tokens(tokens);
		}
	}
}

impl FlatTokens for FunctionCallExpr {
	fn flat_tokens(&self, tokens: &mut Vec<Token>) {
		tokens.push(self.ident.clone());

		for arg in &self.args {
			arg.flat_tokens(tokens);
		}
	}
}

impl FlatTokens for AssignmentStmt {
	fn flat_tokens(&self, tokens: &mut Vec<Token>) {
		self.lhs.flat_tokens(tokens);
		tokens.push(self.op.clone());
		self.rhs.flat_tokens(tokens);
	}
}

impl FlatTokens for Expr {
	fn flat_tokens(&self, tokens: &mut Vec<Token>) {
		match self {
			Expr::Singular(expr) => expr.flat_tokens(tokens),
			Expr::Binary(expr) => expr.flat_tokens(tokens),
		}
	}
}

impl FlatTokens for SingularExpr {
	fn flat_tokens(&self, tokens: &mut Vec<Token>) {
		if let Some(ref prefix) = self.prefix {
			for token in prefix {
				tokens.push(token.clone());
			}
		}
		self.expr.flat_tokens(tokens);

		if let Some(ref postfix) = self.postfix {
			postfix.flat_tokens(tokens);
		}
	}
}

impl FlatTokens for BinaryExpr {
	fn flat_tokens(&self, tokens: &mut Vec<Token>) {
		self.lhs.flat_tokens(tokens);
		tokens.push(self.op.clone());
		self.rhs.flat_tokens(tokens);
	}
}

impl FlatTokens for PrimaryExpr {
	fn flat_tokens(&self, tokens: &mut Vec<Token>) {
		use PrimaryExpr::*;

		match self {
			TypeCtor(expr) => expr.flat_tokens(tokens),
			Literal(expr) => tokens.push(expr.clone()),
			Paren(expr) => expr.flat_tokens(tokens),
			Bitcast(expr) => expr.flat_tokens(tokens),
			Identifier(expr) => tokens.push(expr.clone()),
			FunctionCall(expr) => expr.flat_tokens(tokens),
		}
	}
}

impl FlatTokens for TypeCtorExpr {
	fn flat_tokens(&self, tokens: &mut Vec<Token>) {
		self.ty.flat_tokens(tokens);

		for arg in &self.args {
			arg.flat_tokens(tokens);
		}
	}
}

impl FlatTokens for BitcastExpr {
	fn flat_tokens(&self, tokens: &mut Vec<Token>) {
		tokens.push(self.keyword.clone());
		self.ty.flat_tokens(tokens);
		self.expr.flat_tokens(tokens);
	}
}

impl FlatTokens for PostfixExpr {
	fn flat_tokens(&self, tokens: &mut Vec<Token>) {
		use PostfixExpr::*;

		match self {
			Bracket { expr, postfix, .. } => {
				expr.flat_tokens(tokens);

				if let Some(ref postfix) = postfix {
					postfix.flat_tokens(tokens);
				}
			}
			Dot { ident, postfix, .. } => {
				tokens.push(ident.clone());

				if let Some(ref postfix) = postfix {
					postfix.flat_tokens(tokens);
				}
			}
		}
	}
}
