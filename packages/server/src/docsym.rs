use std::{sync::Arc, thread};

use crossbeam::channel::Sender;
use dashmap::DashMap;
use itertools::Itertools;
use lsp_server::{Message, Response};
use lsp_types::{
	request::{DocumentSymbolRequest, Request},
	DocumentSymbol, DocumentSymbolParams, DocumentSymbolResponse, Range, SymbolKind, SymbolTag,
	Url,
};
use parser::{
	ast::{Decl, FunctionDecl, FunctionSignature, Pretty, StructDecl, StructField, VarDecl},
	GetRange,
};
use serde_json as json;

use crate::documents;

lazy_static! {
	static ref CACHE: Arc<DashMap<Url, Arc<DocumentSymbolResponse>>> = Arc::new(DashMap::default());
}

pub fn handle(req: lsp_server::Request, tx: Sender<Message>) {
	thread::spawn(move || {
		let (id, params) = req
			.extract::<DocumentSymbolParams>(DocumentSymbolRequest::METHOD)
			.unwrap();

		let response = match documents::parse(&params.text_document.uri) {
			Some(ast) => {
				let symbols = ast.into_symbols();
				let response = Arc::new(DocumentSymbolResponse::Nested(symbols));
				CACHE.insert(params.text_document.uri, response.clone());

				response
			}
			// If there's some syntax error in the file, we won't be able to parse it.
			// In this case, just retrieve the cached result if any, or an empty vec otherwise.
			None => match CACHE.get(&params.text_document.uri) {
				Some(cached_entry) => Arc::clone(cached_entry.value()),
				None => Arc::new(DocumentSymbolResponse::Nested(vec![])),
			},
		};
		let result = Some(json::to_value(response.as_ref()).unwrap());

		tx.send(Message::Response(Response {
			id,
			result,
			error: None,
		}))
		.unwrap();
	});
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

impl IntoSymbols for Arc<Vec<Decl>> {
	fn into_symbols(self) -> Vec<DocumentSymbol> {
		Arc::clone(&self)
			.iter()
			.filter_map(|decl| decl.as_symbol())
			.collect()
	}
}

impl AsSymbol for Decl {
	fn as_symbol(&self) -> Option<DocumentSymbol> {
		use Decl::*;

		let ident = self.ident();
		let builder = DocSymBuilder::new()
			.name(ident)
			.range(self.range())
			.selection_range(ident.range());

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

impl IntoSymbols for &Vec<StructField> {
	fn into_symbols(self) -> Vec<DocumentSymbol> {
		self.iter()
			.map(|field| {
				DocSymBuilder::new()
					.kind(SymbolKind::Field)
					.name(&field.name)
					.detail(Some(&field.type_decl))
					.range(field.range)
					.selection_range(field.name.range())
					.build()
			})
			.collect()
	}
}

impl Detail for &VarDecl {
	fn detail(&self) -> Option<String> {
		let attr_detail = self.attributes.as_ref().map(|attr| attr.pretty());
		let type_detail = self.type_decl.as_ref().map(|ty| ty.to_string());
		let combine = |a, b| format!("{} {}", a, b);

		attr_detail
			.and_then(|attr| type_detail.as_ref().map(|ty| combine(attr, ty)))
			.or(type_detail)
	}
}

impl Detail for &StructDecl {
	fn detail(&self) -> Option<String> {
		self.attributes
			.as_ref()
			.map(|attributes| attributes.pretty())
	}
}

impl Detail for &StructField {
	fn detail(&self) -> Option<String> {
		let type_detail = self.type_decl.to_string();

		self.attributes
			.as_ref()
			.map(|attr| format!("{} {}", attr.pretty(), type_detail))
			.or(Some(type_detail))
	}
}

impl Detail for &FunctionDecl {
	fn detail(&self) -> Option<String> {
		let sig_detail = (&self.signature).detail().unwrap();

		self.attributes
			.as_ref()
			.map(|attr| format!("{} {}", attr.pretty(), sig_detail))
			.or(Some(sig_detail))
	}
}

impl Detail for &FunctionSignature {
	fn detail(&self) -> Option<String> {
		Some(format!(
			"fn({}){}",
			self.params
				.iter()
				.map(|param| param.type_decl.to_string())
				.join(", "),
			self.return_type
				.as_ref()
				.map_or("".to_string(), |ty| format!(" -> {}", ty))
		))
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
