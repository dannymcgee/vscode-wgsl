use gramatika::SpannedError;
use lsp_types::{Diagnostic, DiagnosticSeverity, Range};
use naga::{
	front::wgsl::ParseError as NagaParseError,
	valid::{FunctionError, ValidationError as NagaValidationError},
	WithSpan,
};

use crate::documents::Document;

use super::builder::DiagnosticBuilder;

#[derive(Clone, Copy, Debug, Serialize, Deserialize, PartialEq)]
pub enum ErrorKind {
	Parse,
	Transpile,
	NagaParse,
	NagaValidation,
}

pub trait IntoDiagnostics {
	fn into_diagnostics(self, kind: ErrorKind, doc: &Document) -> Vec<Diagnostic>;
}

impl IntoDiagnostics for Vec<SpannedError> {
	fn into_diagnostics(self, kind: ErrorKind, _: &Document) -> Vec<Diagnostic> {
		self.iter()
			.map(|err| {
				DiagnosticBuilder::new(kind)
					.range(err.span.unwrap_or_default().as_range(None))
					.severity(DiagnosticSeverity::ERROR)
					.message(std::format!("ParseError: {}", &err.message))
					.build()
			})
			.collect()
	}
}

impl IntoDiagnostics for WithSpan<NagaValidationError> {
	fn into_diagnostics(self, kind: ErrorKind, doc: &Document) -> Vec<Diagnostic> {
		use FunctionError::*;
		use NagaValidationError::*;

		let range = self
			.location(&doc.source)
			.map(|source_loc| source_loc.as_range(Some(&doc.source)))
			.unwrap_or_default();

		let outer_msg = std::format!("{}", &self);
		let inner_msg = match self.into_inner() {
			InvalidHandle(inner) => inner.to_string(),
			Layouter(inner) => inner.to_string(),
			Type { source, .. } => source.to_string(),
			Constant { source, .. } => source.to_string(),
			GlobalVariable { source, .. } => source.to_string(),
			Function { source, .. } => {
				let fn_msg = source.to_string();
				match source {
					Expression { source, .. } => std::format!("{}: {}", fn_msg, source),
					LocalVariable { source, .. } => std::format!("{}: {}", fn_msg, source),
					InvalidCall { error, .. } => std::format!("{}: {}", fn_msg, error),
					_ => fn_msg,
				}
			}
			EntryPoint { source, .. } => source.to_string(),
			Corrupted => "".into(),
		};

		let message = if !inner_msg.is_empty() {
			std::format!("ValidationError: {}: {}", outer_msg, inner_msg)
		} else {
			std::format!("ValidationError: {}", outer_msg)
		};

		vec![DiagnosticBuilder::new(kind)
			.range(range)
			.severity(DiagnosticSeverity::ERROR)
			.source("naga::validation")
			.message(message)
			.build()]
	}
}

impl IntoDiagnostics for NagaParseError {
	fn into_diagnostics(self, kind: ErrorKind, doc: &Document) -> Vec<Diagnostic> {
		let range = self
			.location(&doc.source)
			.map(|source_loc| source_loc.as_range(Some(&doc.source)))
			.unwrap_or_default();

		vec![DiagnosticBuilder::new(kind)
			.range(range)
			.severity(DiagnosticSeverity::ERROR)
			.source("naga::parse")
			.message(std::format!("ParseError: {}", self))
			.build()]
	}
}

trait AsRange {
	fn as_range(&self, src: Option<&str>) -> Range;
}

impl AsRange for gramatika::Span {
	fn as_range(&self, _: Option<&str>) -> Range {
		Range {
			start: lsp_types::Position {
				line: self.start.line as u32,
				character: self.start.character as u32,
			},
			end: lsp_types::Position {
				line: self.end.line as u32,
				character: self.end.character as u32,
			},
		}
	}
}

impl AsRange for naga::Span {
	fn as_range(&self, src: Option<&str>) -> Range {
		use gramatika::Span;

		let std::ops::Range { start, end } = self.to_range().unwrap();
		let mut span = Span::default();

		for (idx, c) in src.unwrap().chars().enumerate() {
			match (idx, c) {
				(i, '\n') if i <= start => {
					span.start.line += 1;
					span.start.character = 0;
					span.end = span.start;
				}
				(i, _) if i <= start => {
					span.start.character += 1;
					span.end = span.start;
				}
				(i, '\n') if i < end => {
					span.end.line += 1;
					span.end.character = 0;
				}
				(i, _) if i < end => {
					span.end.character += 1;
				}
				(_, _) => break,
			}
		}

		span.as_range(None)
	}
}

impl AsRange for naga::SourceLocation {
	fn as_range(&self, src: Option<&str>) -> Range {
		let start = lsp_types::Position {
			line: self.line_number - 1,
			character: self.line_position - 1,
		};

		let mut end = start;
		for (idx, c) in src.unwrap()[(self.offset as usize)..].chars().enumerate() {
			match (idx, c) {
				(i, '\n') if i < (self.length as usize) => {
					end.line += 1;
					end.character = 0;
				}
				(i, _) if i < (self.length as usize) => {
					end.character += 1;
				}
				(_, _) => break,
			}
		}

		Range { start, end }
	}
}

trait FirstLine {
	fn first_line() -> Self;
}

impl FirstLine for Range {
	fn first_line() -> Self {
		use lsp_types::Position;

		Range {
			start: Position {
				line: 0,
				character: 0,
			},
			end: Position {
				line: 1,
				character: 0,
			},
		}
	}
}
