use lsp_types::{
	Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, DiagnosticTag, Range,
};
use serde_json as json;

use super::conv::ErrorKind;

#[derive(Debug)]
pub(super) struct DiagnosticBuilder {
	kind: ErrorKind,
	range: Option<Range>,
	severity: Option<DiagnosticSeverity>,
	source: Option<String>,
	message: Option<String>,
	related_information: Option<Vec<DiagnosticRelatedInformation>>,
	tags: Option<Vec<DiagnosticTag>>,
}

impl DiagnosticBuilder {
	pub(super) fn new(kind: ErrorKind) -> Self {
		Self {
			kind,
			range: None,
			severity: None,
			source: None,
			message: None,
			related_information: None,
			tags: None,
		}
	}

	pub(super) fn range(mut self, range: Range) -> Self {
		self.range = Some(range);
		self
	}

	pub(super) fn severity(mut self, severity: DiagnosticSeverity) -> Self {
		self.severity = Some(severity);
		self
	}

	pub(super) fn source<S>(mut self, source: S) -> Self
	where S: ToString {
		self.source = Some(source.to_string());
		self
	}

	pub(super) fn message<S>(mut self, message: S) -> Self
	where S: ToString {
		self.message = Some(message.to_string());
		self
	}

	#[allow(dead_code)]
	pub(super) fn related_information(mut self, info: Vec<DiagnosticRelatedInformation>) -> Self {
		self.related_information = Some(info);
		self
	}

	#[allow(dead_code)]
	pub(super) fn tags(mut self, tags: Vec<DiagnosticTag>) -> Self {
		self.tags = Some(tags);
		self
	}

	pub(super) fn build(self) -> Diagnostic {
		Diagnostic {
			range: self.range.expect("range field is uninitialized!"),
			severity: self.severity,
			code: None,
			code_description: None,
			source: self.source.or_else(|| Some("vscode-wgsl".into())),
			message: self.message.expect("message field is uninitialized!"),
			related_information: self.related_information,
			tags: self.tags,
			data: Some(json::to_value(&self.kind).unwrap()),
		}
	}
}
