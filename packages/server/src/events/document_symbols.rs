use std::sync::Arc;

use dashmap::DashMap;
use gramatika::Spanned;
use itertools::Itertools;
use lsp_server::{Message, RequestId, Response};
use lsp_types::{
	DocumentSymbol, DocumentSymbolParams, DocumentSymbolResponse, Range, SymbolKind, SymbolTag, Url,
};
use parser_v2::{
	decl::{Decl, FieldDecl, FunctionDecl, StructBody, StructDecl, VarDecl},
	utils::ToRange,
	SyntaxTree,
};
use serde_json as json;

use crate::documents_v2::Documents;

lazy_static! {
	static ref CACHE: Arc<DashMap<Url, Arc<DocumentSymbolResponse>>> = Arc::new(DashMap::default());
}

pub fn handle(id: RequestId, params: DocumentSymbolParams, docs: &Documents) -> Message {
	let response = if let Some(document) = docs.get(&params.text_document.uri) {
		let symbols = Arc::clone(&document.ast).into_symbols();
		let response = Arc::new(DocumentSymbolResponse::Nested(symbols));
		CACHE.insert(params.text_document.uri, response.clone());

		response
	} else {
		// If there's some syntax error in the file, we won't be able to parse it.
		// In this case, just retrieve the cached result if any, or an empty vec otherwise.
		match CACHE.get(&params.text_document.uri) {
			Some(cached_entry) => Arc::clone(cached_entry.value()),
			None => Arc::new(DocumentSymbolResponse::Nested(vec![])),
		}
	};

	let result = Some(json::to_value(response.as_ref()).unwrap());

	Message::Response(Response {
		id,
		result,
		error: None,
	})
}

trait IntoSymbols {
	fn into_symbols(self) -> Vec<DocumentSymbol>;
}

trait AsSymbol {
	fn as_symbol(&self) -> Option<DocumentSymbol>;
}

trait Detail {
	fn detail(&self) -> Option<String>;
}

impl<'a> IntoSymbols for Arc<SyntaxTree<'a>> {
	fn into_symbols(self) -> Vec<DocumentSymbol> {
		self.inner
			.iter()
			.filter_map(|decl| decl.as_symbol())
			.collect()
	}
}

impl<'a> IntoSymbols for Arc<Vec<Decl<'a>>> {
	fn into_symbols(self) -> Vec<DocumentSymbol> {
		Arc::clone(&self)
			.iter()
			.filter_map(|decl| decl.as_symbol())
			.collect()
	}
}

impl<'a> AsSymbol for Decl<'a> {
	fn as_symbol(&self) -> Option<DocumentSymbol> {
		use Decl::*;

		let ident = self.name();
		let builder = DocSymBuilder::new()
			.name(ident)
			.range(self.span().to_range())
			.selection_range(ident.span().to_range());

		match self {
			Var(ref inner) => Some(
				builder
					.kind(SymbolKind::Variable)
					.detail(inner.detail())
					.build(),
			),
			Const(ref inner) => Some(
				builder
					.kind(SymbolKind::Constant)
					.detail(inner.detail())
					.build(),
			),
			TypeAlias(ref inner) => Some(
				builder
					.kind(SymbolKind::TypeParameter)
					.detail(Some(&inner.value))
					.build(),
			),
			Struct(ref inner) => Some(
				builder
					.kind(SymbolKind::Struct)
					.detail(inner.detail())
					.children(inner.body.into_symbols())
					.build(),
			),
			Function(ref inner) => Some(
				builder
					.kind(SymbolKind::Function)
					.detail(inner.detail())
					.build(),
			),
			_ => None,
		}
	}
}

impl<'a> IntoSymbols for &Vec<FieldDecl<'a>> {
	fn into_symbols(self) -> Vec<DocumentSymbol> {
		self.iter()
			.map(|field| {
				DocSymBuilder::new()
					.kind(SymbolKind::Field)
					.name(&field.name)
					.detail(Some(&field.ty))
					.range(field.span().to_range())
					.selection_range(field.name.span().to_range())
					.build()
			})
			.collect()
	}
}

impl<'a> IntoSymbols for &StructBody<'a> {
	fn into_symbols(self) -> Vec<DocumentSymbol> {
		self.fields
			.iter()
			.map(|field| {
				let field = if let Decl::Field(field) = field {
					field
				} else {
					unreachable!()
				};
				DocSymBuilder::new()
					.kind(SymbolKind::Field)
					.name(&field.name)
					.detail(Some(&field.ty))
					.range(field.span().to_range())
					.selection_range(field.name.span().to_range())
					.build()
			})
			.collect()
	}
}

impl<'a> Detail for &VarDecl<'a> {
	fn detail(&self) -> Option<String> {
		let attr_detail = self.attributes.as_ref();
		let type_detail = self.ty.as_ref().map(|ty| ty.to_string());
		let combine = |a, b| format!("{} {}", a, b);

		attr_detail
			.and_then(|attr| type_detail.as_ref().map(|ty| combine(attr, ty)))
			.or(type_detail)
	}
}

impl<'a> Detail for &StructDecl<'a> {
	fn detail(&self) -> Option<String> {
		self.attributes
			.as_ref()
			.map(|attributes| attributes.to_string())
	}
}

impl<'a> Detail for &FieldDecl<'a> {
	fn detail(&self) -> Option<String> {
		self.attributes
			.as_ref()
			.map(|attr| format!("{} {}", attr, self.ty))
			.or_else(|| Some(self.ty.to_string()))
	}
}

impl<'a> Detail for &FunctionDecl<'a> {
	fn detail(&self) -> Option<String> {
		let params = self
			.params
			.iter()
			.map(|param| {
				if let Decl::Param(param) = param {
					format!("{}: {}", param.name, param.ty)
				} else {
					unreachable!()
				}
			})
			.join(", ");

		let return_ty = if let Some(return_ty) = self.return_ty.as_ref() {
			format!(" -> {}", return_ty)
		} else {
			"".into()
		};

		let sig_detail = format!("fn ({}){}", params, return_ty);

		self.attributes
			.as_ref()
			.map(|attr| format!("{} {}", attr, sig_detail))
			.or(Some(sig_detail))
	}
}

#[derive(Debug, Default)]
struct DocSymBuilder {
	kind: Option<SymbolKind>,
	name: Option<String>,
	range: Option<Range>,
	selection_range: Option<Range>,
	detail: Option<String>,
	tags: Option<Vec<SymbolTag>>,
	children: Option<Vec<DocumentSymbol>>,
}

impl DocSymBuilder {
	fn new() -> Self {
		Self::default()
	}

	fn kind(mut self, kind: SymbolKind) -> Self {
		self.kind = Some(kind);
		self
	}

	fn name<S>(mut self, name: S) -> Self
	where
		S: ToString,
	{
		self.name = Some(name.to_string());
		self
	}

	fn range(mut self, range: Range) -> Self {
		self.range = Some(range);
		self
	}

	fn selection_range(mut self, range: Range) -> Self {
		self.selection_range = Some(range);
		self
	}

	fn detail<S>(mut self, detail: Option<S>) -> Self
	where
		S: ToString,
	{
		self.detail = detail.map(|inner| inner.to_string());
		self
	}

	#[allow(dead_code)]
	fn tags(mut self, tags: Vec<SymbolTag>) -> Self {
		self.tags = Some(tags);
		self
	}

	fn children(mut self, children: Vec<DocumentSymbol>) -> Self {
		self.children = Some(children);
		self
	}

	fn build(self) -> DocumentSymbol {
		if self.kind.is_none() {
			panic!("`kind` field not initialized!");
		}
		if self.name.is_none() {
			panic!("`name` field not initialized!");
		}
		if self.range.is_none() {
			panic!("`range` field not initialized!");
		}
		if self.selection_range.is_none() {
			panic!("`selection_range` field not initialized!");
		}

		self.into()
	}
}

#[allow(deprecated)]
impl From<DocSymBuilder> for DocumentSymbol {
	fn from(partial: DocSymBuilder) -> Self {
		Self {
			kind: partial.kind.unwrap(),
			name: partial.name.unwrap(),
			range: partial.range.unwrap(),
			selection_range: partial.selection_range.unwrap(),
			detail: partial.detail,
			tags: partial.tags,
			deprecated: None,
			children: partial.children,
		}
	}
}
