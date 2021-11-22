use gramatika::{Spanned, SpannedError};
use itertools::Itertools;
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

		let spans = self.spans().cloned().collect_vec();
		let outer_msg = std::format!("{}", &self);
		let inner_msg = match self.into_inner() {
			Layouter(inner) => inner.to_string(),
			Type { error, .. } => error.to_string(),
			Constant { error, .. } => error.to_string(),
			GlobalVariable { error, .. } => error.to_string(),
			Function { error, .. } => {
				let fn_msg = error.to_string();
				match error {
					Expression { error, .. } => std::format!("{}: {}", fn_msg, error),
					LocalVariable { error, .. } => std::format!("{}: {}", fn_msg, error),
					InvalidCall { error, .. } => std::format!("{}: {}", fn_msg, error),
					_ => fn_msg,
				}
			}
			EntryPoint { error, .. } => error.to_string(),
			Corrupted => "".into(),
		};

		let message = if !inner_msg.is_empty() {
			std::format!("ValidationError: {}: {}", outer_msg, inner_msg)
		} else {
			std::format!("ValidationError: {}", outer_msg)
		};

		spans
			.iter()
			.map(|(span, _)| {
				DiagnosticBuilder::new(kind)
					.range(span.as_range(Some(&doc.source)))
					.severity(DiagnosticSeverity::ERROR)
					.source("naga::validation")
					.message(message.clone())
					.build()
			})
			.collect()
	}
}

impl IntoDiagnostics for NagaParseError {
	fn into_diagnostics(self, kind: ErrorKind, doc: &Document) -> Vec<Diagnostic> {
		use gramatika::{Position, Span};

		let (start_line, start_col) = self.location(&doc.source);
		let pos = Position {
			line: start_line - 1,
			character: start_col - 1,
		};
		let needle_span = Span {
			start: pos,
			end: pos,
		};

		let full_span = doc
			.tokens
			.iter()
			.find_map(|token| {
				if token.span().contains(needle_span) {
					Some(token.span())
				} else {
					None
				}
			})
			.unwrap_or(needle_span);

		vec![DiagnosticBuilder::new(kind)
			.range(full_span.as_range(None))
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
