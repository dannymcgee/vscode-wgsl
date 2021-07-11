use std::thread;

use crossbeam_channel::Sender;
use lsp_server::{Message, Response, ResponseError};
use lsp_types::{
	request::{DocumentSymbolRequest, Request},
	DocumentSymbol, DocumentSymbolParams, DocumentSymbolResponse, Position, Range, SymbolKind,
	SymbolTag,
};
use parser::{
	pest::{
		iterators::{Pair, Pairs},
		Span,
	},
	Rule,
};
use serde_json as json;

use crate::documents::Documents;

pub fn handle(req: lsp_server::Request, documents: Documents, tx: Sender<Message>) {
	thread::spawn(move || {
		let (id, params) = req
			.extract::<DocumentSymbolParams>(DocumentSymbolRequest::METHOD)
			.unwrap();

		#[allow(unused_must_use)]
		match documents.read(&params.text_document.uri) {
			Ok(content) => match parser::parse(&content) {
				Ok(result) => {
					let symbols = result.into_symbols();
					let response = DocumentSymbolResponse::Nested(symbols);

					tx.send(Message::Response(Response {
						id,
						result: Some(json::to_value(&response).unwrap()),
						error: None,
					}));
				}
				Err(err) => {
					tx.send(Message::Response(Response {
						id,
						result: None,
						error: Some(ResponseError {
							code: 1,
							message: format!("{}", err),
							data: None,
						}),
					}));
				}
			},
			Err(err) => {
				tx.send(Message::Response(Response {
					id,
					result: None,
					error: Some(ResponseError {
						code: 1,
						message: format!("{}", err),
						data: None,
					}),
				}));
			}
		}
	});
}

trait SymbolParser {
	fn parse(self) -> Option<DocumentSymbol>;
	fn parse_global_var_decl(self) -> DocumentSymbol;
	fn parse_global_const_decl(self) -> DocumentSymbol;
	fn parse_type_alias(self) -> DocumentSymbol;
	fn parse_struct_decl(self) -> DocumentSymbol;
	fn parse_struct_body_decl(self) -> Vec<DocumentSymbol>;
	fn parse_struct_member(self) -> DocumentSymbol;
	fn parse_func_decl(self) -> DocumentSymbol;
	fn parse_func_body(self) -> Option<Vec<DocumentSymbol>>;
	fn parse_variable_stmt(self) -> DocumentSymbol;
}

trait IntoSymbols {
	fn into_symbols(self) -> Vec<DocumentSymbol>;
}

trait IntoRange {
	fn into_range(self) -> Range;
}

trait Detail {
	fn detail(&self) -> Option<String>;
}

trait PrettyDebug {
	fn pretty(&self) -> String;
}

impl<'a> IntoSymbols for Pairs<'a, Rule> {
	fn into_symbols(mut self) -> Vec<DocumentSymbol> {
		let program = self
			.find_map(|pair| {
				if let Rule::program = pair.as_rule() {
					Some(pair)
				} else {
					None
				}
			})
			.unwrap();

		program
			.into_inner()
			.filter_map(|pair| pair.parse())
			.collect()
	}
}

impl<'a> IntoRange for Span<'a> {
	fn into_range(self) -> Range {
		let (start_line, start_col) = self.start_pos().line_col();
		let (end_line, end_col) = self.end_pos().line_col();

		Range {
			start: Position {
				line: (start_line - 1) as u32,
				character: (start_col - 1) as u32,
			},
			end: Position {
				line: (end_line - 1) as u32,
				character: (end_col - 1) as u32,
			},
		}
	}
}

impl PrettyDebug for Range {
	fn pretty(&self) -> String {
		format!(
			"{}:{}..{}:{}",
			self.start.line, self.start.character, self.end.line, self.end.character
		)
	}
}

impl<'a> Detail for Pair<'a, Rule> {
	fn detail(&self) -> Option<String> {
		match self.as_rule() {
			Rule::attribute_list => Some(self.as_str().into()),
			_ => None,
		}
	}
}

#[allow(deprecated)]
impl<'a> SymbolParser for Pair<'a, Rule> {
	fn parse(self) -> Option<DocumentSymbol> {
		use Rule::*;

		let rule = self.as_rule();

		match rule {
			global_variable_decl => Some(self.parse_global_var_decl()),
			global_constant_decl => Some(self.parse_global_const_decl()),
			type_alias => Some(self.parse_type_alias()),
			struct_decl => Some(self.parse_struct_decl()),
			func_decl => Some(self.parse_func_decl()),
			_ => None,
		}
	}

	fn parse_global_var_decl(self) -> DocumentSymbol {
		use Rule::*;

		let kind = SymbolKind::Variable;
		let range = self.as_span().into_range();

		self.into_inner()
			.fold(DocSymBuilder::new(kind, range), |mut accum, pair| {
				match pair.as_rule() {
					attribute_list => {
						accum.detail = pair.detail();
					}
					variable_decl => {
						let ident_decl = pair
							.into_inner()
							.flatten()
							.find(|pair| pair.as_rule() == variable_ident_decl)
							.unwrap();

						for pair in ident_decl.into_inner() {
							match pair.as_rule() {
								IDENT => {
									accum.name = Some(pair.as_str().into());
									accum.selection_range = Some(pair.as_span().into_range());
								}
								type_decl => {
									let type_detail = pair.as_str();
									if let Some(detail) = accum.detail {
										accum.detail = Some(format!("{} {}", detail, type_detail));
									} else {
										accum.detail = Some(type_detail.into());
									}
								}
								_ => {}
							}
						}
					}
					_ => {}
				}
				accum
			})
			.into()
	}

	fn parse_global_const_decl(self) -> DocumentSymbol {
		use Rule::*;

		let kind = SymbolKind::Constant;
		let range = self.as_span().into_range();

		self.into_inner()
			.fold(DocSymBuilder::new(kind, range), |mut accum, pair| {
				match pair.as_rule() {
					attribute_list => {
						accum.detail = pair.detail();
					}
					variable_ident_decl => {
						for pair in pair.into_inner() {
							match pair.as_rule() {
								IDENT => {
									accum.name = Some(pair.as_str().into());
									accum.selection_range = Some(pair.as_span().into_range());
								}
								type_decl => {
									let type_detail = pair.as_str();
									if let Some(detail) = accum.detail {
										accum.detail = Some(format!("{} {}", detail, type_detail));
									} else {
										accum.detail = Some(type_detail.into());
									}
								}
								_ => {}
							}
						}
					}
					_ => {}
				}
				accum
			})
			.into()
	}

	fn parse_type_alias(self) -> DocumentSymbol {
		use Rule::*;

		let kind = SymbolKind::TypeParameter;
		let range = self.as_span().into_range();

		self.into_inner()
			.fold(DocSymBuilder::new(kind, range), |mut accum, pair| {
				match pair.as_rule() {
					IDENT => {
						accum.name = Some(pair.as_str().into());
						accum.selection_range = Some(pair.as_span().into_range());
					}
					type_decl => {
						accum.detail = Some(pair.as_str().into());
					}
					_ => {}
				}
				accum
			})
			.into()
	}

	fn parse_struct_decl(self) -> DocumentSymbol {
		use Rule::*;

		let kind = SymbolKind::Struct;
		let range = self.as_span().into_range();

		self.into_inner()
			.fold(DocSymBuilder::new(kind, range), |mut accum, pair| {
				match pair.as_rule() {
					IDENT => {
						accum.name = Some(pair.as_str().into());
						accum.selection_range = Some(pair.as_span().into_range());
					}
					attribute_list => {
						accum.detail = pair.detail();
					}
					struct_body_decl => {
						accum.children = Some(pair.parse_struct_body_decl());
					}
					_ => {}
				}
				accum
			})
			.into()
	}

	fn parse_struct_body_decl(self) -> Vec<DocumentSymbol> {
		use Rule::*;

		self.into_inner()
			.filter_map(|pair| match pair.as_rule() {
				struct_member => Some(pair.parse_struct_member()),
				_ => None,
			})
			.collect()
	}

	fn parse_struct_member(self) -> DocumentSymbol {
		use Rule::*;

		let kind = SymbolKind::Field;
		let range = self.as_span().into_range();

		self.into_inner()
			.fold(DocSymBuilder::new(kind, range), |mut accum, pair| {
				match pair.as_rule() {
					attribute_list => {
						accum.detail = pair.detail();
					}
					variable_ident_decl => {
						for pair in pair.into_inner() {
							match pair.as_rule() {
								IDENT => {
									accum.name = Some(pair.as_str().into());
									accum.selection_range = Some(pair.as_span().into_range());
								}
								type_decl => {
									let type_detail = pair.as_str();
									if let Some(detail) = accum.detail {
										accum.detail = Some(format!("{} {}", detail, type_detail));
									} else {
										accum.detail = Some(type_detail.into());
									}
								}
								_ => {}
							}
						}
					}
					_ => {}
				}
				accum
			})
			.into()
	}

	fn parse_func_decl(self) -> DocumentSymbol {
		use Rule::*;

		let kind = SymbolKind::Function;
		let range = self.as_span().into_range();
		let mut signature = String::new();

		let mut symbol =
			self.into_inner()
				.fold(DocSymBuilder::new(kind, range), |mut accum, pair| {
					match pair.as_rule() {
						attribute_list => {
							accum.detail = pair.detail();
						}
						func_header => {
							for pair in pair.into_inner() {
								match pair.as_rule() {
									IDENT => {
										accum.name = Some(pair.as_str().into());
										accum.selection_range = Some(pair.as_span().into_range());
									}
									param_list => {
										signature.push_str("fn(");
										signature.push_str(
											&pair
												.into_inner()
												.flatten()
												.filter_map(|pair| match pair.as_rule() {
													type_decl => Some(pair.as_str()),
													_ => None,
												})
												.collect::<Vec<_>>()
												.join(", "),
										);
										signature.push(')');
									}
									func_return_type => {
										signature.push(' ');
										signature.push_str(pair.as_str());
									}
									_ => {}
								}
							}
						}
						compound_stmt => {
							accum.children = pair.parse_func_body();
						}
						_ => {}
					}
					accum
				});

		if let Some(detail) = symbol.detail {
			symbol.detail = Some(format!("{} {}", detail, signature));
		} else {
			symbol.detail = Some(signature);
		}

		symbol.into()
	}

	fn parse_func_body(self) -> Option<Vec<DocumentSymbol>> {
		use Rule::*;

		let symbols = self
			.into_inner()
			.flatten()
			.filter_map(|pair| match pair.as_rule() {
				variable_stmt => Some(pair.parse_variable_stmt()),
				_ => None,
			})
			.collect::<Vec<_>>();

		if !symbols.is_empty() {
			Some(symbols)
		} else {
			None
		}
	}

	fn parse_variable_stmt(self) -> DocumentSymbol {
		use Rule::*;

		let range = self.as_span().into_range();
		let kind = SymbolKind::Variable;

		self.into_inner()
			.fold(DocSymBuilder::new(kind, range), |mut accum, pair| {
				match pair.as_rule() {
					LET => {
						accum.kind = SymbolKind::Constant;
					}
					variable_decl => {
						let ident_decl = pair
							.into_inner()
							.flatten()
							.find(|pair| pair.as_rule() == variable_ident_decl)
							.unwrap();

						for pair in ident_decl.into_inner() {
							match pair.as_rule() {
								IDENT => {
									accum.name = Some(pair.as_str().into());
									accum.selection_range = Some(pair.as_span().into_range());
								}
								type_decl => {
									accum.detail = Some(pair.as_str().into());
								}
								_ => {}
							}
						}
					}
					variable_ident_decl => {
						for pair in pair.into_inner() {
							match pair.as_rule() {
								IDENT => {
									accum.name = Some(pair.as_str().into());
									accum.selection_range = Some(pair.as_span().into_range());
								}
								type_decl => {
									accum.detail = Some(pair.as_str().into());
								}
								_ => {}
							}
						}
					}
					IDENT => {
						accum.name = Some(pair.as_str().into());
						accum.selection_range = Some(pair.as_span().into_range());
					}
					_ => {}
				}
				accum
			})
			.into()
	}
}

#[derive(Debug)]
struct DocSymBuilder {
	kind: SymbolKind,
	name: Option<String>,
	range: Range,
	selection_range: Option<Range>,
	detail: Option<String>,
	tags: Option<Vec<SymbolTag>>,
	deprecated: Option<bool>,
	children: Option<Vec<DocumentSymbol>>,
}

impl DocSymBuilder {
	pub fn new(kind: SymbolKind, range: Range) -> Self {
		Self {
			kind,
			name: None,
			range,
			selection_range: None,
			detail: None,
			tags: None,
			deprecated: None,
			children: None,
		}
	}
}

#[allow(deprecated)]
impl From<DocSymBuilder> for DocumentSymbol {
	fn from(partial: DocSymBuilder) -> Self {
		Self {
			kind: partial.kind,
			name: partial.name.unwrap(),
			range: partial.range,
			selection_range: partial.selection_range.unwrap(),
			detail: partial.detail,
			tags: partial.tags,
			deprecated: partial.deprecated,
			children: partial.children,
		}
	}
}
