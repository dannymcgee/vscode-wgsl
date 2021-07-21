use std::{
	sync::{Arc, Mutex},
	thread,
	time::Duration,
};

use crossbeam::channel::{self, Sender};
use dashmap::DashMap;
use itertools::Itertools;
use lsp_server::{Message, Notification};
use lsp_types::{
	notification::{Notification as Notif, PublishDiagnostics},
	Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, DiagnosticTag, Position,
	PublishDiagnosticsParams, Range, Url,
};
use naga::{
	front::wgsl,
	valid::{Capabilities, EntryPointError, ValidationFlags, Validator},
};
use parser::{ast::Decl, AstNode, GetRange, IsWithin, ParentGranularity, ParentOfRange};
use serde::{Deserialize, Serialize};
use serde_json as json;

use crate::documents;

static mut TX: Option<Sender<Message>> = None;
static mut DIRTY_NOTIFIER: Option<Sender<()>> = None;

lazy_static! {
	static ref COLLECTION: Arc<DashMap<Url, Vec<Diagnostic>>> = Arc::new(DashMap::default());
	static ref VALIDATOR: Mutex<Validator> =
		Mutex::new(Validator::new(ValidationFlags::all(), Capabilities::all()));
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub enum ErrorKind {
	ParseError,
	NagaParseError,
	NagaValidationError,
}

fn tx() -> Sender<Message> {
	unsafe {
		if TX.is_none() {
			panic!("Tried to retrieve diagnostics sender before initialization");
		}
		TX.as_ref().unwrap().clone()
	}
}

fn dirty_notifier() -> Sender<()> {
	unsafe {
		if DIRTY_NOTIFIER.is_none() {
			panic!("Tried to retrieve diagnostics events sender before initialization");
		}
		DIRTY_NOTIFIER.as_ref().unwrap().clone()
	}
}

/// This should only ever be called once, from the main thread, before `main_loop` is executed.
pub(crate) unsafe fn bootstrap(tx: Sender<Message>) {
	TX = Some(tx);

	let (dirty_notifier, dirty_events) = channel::bounded(1);
	DIRTY_NOTIFIER = Some(dirty_notifier);

	thread::spawn(move || loop {
		if let Ok(()) = dirty_events.try_recv() {
			self::publish();
		}

		thread::sleep(Duration::from_secs_f32(1.0 / 30.0));
	});
}

pub fn validate(uri: Url, content: String) {
	thread::spawn(move || match VALIDATOR.lock() {
		Ok(mut validator) => match wgsl::parse_str(&content) {
			Ok(ref module) => {
				self::clear_errors(&uri, Some(ErrorKind::NagaParseError));

				match validator.validate(module) {
					Ok(_) => self::clear_errors(&uri, Some(ErrorKind::NagaValidationError)),
					Err(err) => {
						self::clear_errors(&uri, Some(ErrorKind::NagaValidationError));
						self::report_error(&uri, err, ErrorKind::NagaValidationError);
					}
				}
			}
			Err(err) => {
				self::clear_errors(&uri, Some(ErrorKind::NagaParseError));
				self::report_error(&uri, err, ErrorKind::NagaParseError);
			}
		},
		Err(err) => eprintln!("Error retrieving validator: {:#?}", err),
	});
}

pub fn report_error<E>(uri: &Url, err: E, kind: ErrorKind)
where
	E: IntoDiagnostic,
{
	let diag = match kind {
		ErrorKind::ParseError => err.into_diagnostic(kind, None, None),
		ErrorKind::NagaValidationError => err.into_diagnostic(kind, Some(uri), None),
		ErrorKind::NagaParseError => {
			let src = match documents::read(uri) {
				Some(src) => src,
				None => return,
			};
			err.into_diagnostic(kind, Some(uri), Some(&src))
		}
	};

	if let Some(mut entry) = COLLECTION.get_mut(uri) {
		entry.value_mut().push(diag);
	} else {
		COLLECTION.insert(uri.clone(), vec![diag]);
	}

	let _ = dirty_notifier().try_send(());
}

pub fn clear_errors(uri: &Url, kind: Option<ErrorKind>) {
	if let Some(mut entry) = COLLECTION.get_mut(uri) {
		let diagnostics = entry.value_mut();

		if let Some(kind) = kind {
			let idxes = diagnostics
				.iter()
				.enumerate()
				.filter_map(|(idx, diag)| {
					let diag_kind_value = diag.data.as_ref().unwrap().clone();
					let diag_kind = json::from_value::<ErrorKind>(diag_kind_value).unwrap();

					if diag_kind == kind {
						Some(idx)
					} else {
						None
					}
				})
				.collect_vec();

			for idx in idxes {
				diagnostics.swap_remove(idx);
			}
		} else {
			diagnostics.clear();
		}

		let _ = dirty_notifier().try_send(());
	}
}

fn publish() {
	for entry in COLLECTION.clone().iter() {
		let uri = entry.key().clone();
		let diagnostics = entry.value().clone();

		let _ = tx().send(Message::Notification(Notification {
			method: PublishDiagnostics::METHOD.into(),
			params: json::to_value(&PublishDiagnosticsParams {
				diagnostics,
				uri,
				version: None,
			})
			.unwrap(),
		}));
	}
}

#[derive(Debug)]
struct DiagnosticBuilder {
	kind: ErrorKind,
	range: Option<Range>,
	severity: Option<DiagnosticSeverity>,
	message: Option<String>,
	related_information: Option<Vec<DiagnosticRelatedInformation>>,
	tags: Option<Vec<DiagnosticTag>>,
}

impl DiagnosticBuilder {
	fn new(kind: ErrorKind) -> Self {
		Self {
			kind,
			range: None,
			severity: None,
			message: None,
			related_information: None,
			tags: None,
		}
	}

	fn range(mut self, range: Range) -> Self {
		self.range = Some(range);
		self
	}

	fn severity(mut self, severity: DiagnosticSeverity) -> Self {
		self.severity = Some(severity);
		self
	}

	fn message<S>(mut self, message: S) -> Self
	where
		S: ToString,
	{
		self.message = Some(message.to_string());
		self
	}

	#[allow(dead_code)]
	fn related_information(mut self, info: Vec<DiagnosticRelatedInformation>) -> Self {
		self.related_information = Some(info);
		self
	}

	#[allow(dead_code)]
	fn tags(mut self, tags: Vec<DiagnosticTag>) -> Self {
		self.tags = Some(tags);
		self
	}

	fn build(self) -> Diagnostic {
		Diagnostic {
			range: self.range.expect("range field is uninitialized!"),
			severity: self.severity,
			code: None,
			code_description: None,
			source: Some("wgsl".into()),
			message: self.message.expect("message field is uninitialized!"),
			related_information: self.related_information,
			tags: self.tags,
			data: Some(json::to_value(&self.kind).unwrap()),
		}
	}
}

pub trait IntoDiagnostic {
	fn into_diagnostic(self, kind: ErrorKind, uri: Option<&Url>, src: Option<&str>) -> Diagnostic;
}

// NOTE: This entire impl was basically copy/pasted from pest's source
impl IntoDiagnostic for pest::error::Error<parser::Rule> {
	fn into_diagnostic(self, kind: ErrorKind, _: Option<&Url>, _: Option<&str>) -> Diagnostic {
		use pest::error::ErrorVariant::*;

		let enumerate = |rules: Vec<parser::Rule>| match rules.len() {
			1 => format!("{}", &rules[0]),
			2 => format!("{} or {}", &rules[0], &rules[1]),
			len => {
				let separated = rules
					.iter()
					.take(len - 1)
					.map(|rule| format!("{}", &rule))
					.join(", ");

				format!("{}, or {}", separated, &rules[len - 1])
			}
		};

		let message = match self.variant {
			ParsingError {
				positives,
				negatives,
			} => match (negatives.is_empty(), positives.is_empty()) {
				(false, false) => format!(
					"Unexpected {}; Expected {}",
					enumerate(negatives),
					enumerate(positives),
				),
				(false, true) => format!("Unexpected {}", enumerate(negatives)),
				(true, false) => format!("Expected {}", enumerate(positives)),
				(true, true) => "Unknown parsing error".into(),
			},
			CustomError { message } => message,
		};

		DiagnosticBuilder::new(kind)
			.range(self.line_col.as_range())
			.severity(DiagnosticSeverity::Error)
			.message(format!("ParseError: {}", message))
			.build()
	}
}

impl IntoDiagnostic for wgsl::ParseError {
	fn into_diagnostic(self, kind: ErrorKind, uri: Option<&Url>, src: Option<&str>) -> Diagnostic {
		let src = src.expect("Source content must be provided for naga ParseError");
		let uri = uri.expect("uri must be provided for naga ParseError");

		let (start_line, start_col) = self.location(src);
		let range = Range {
			start: Position {
				line: start_line as u32 - 1,
				character: start_col as u32 - 1,
			},
			end: Position {
				line: start_line as u32 - 1,
				character: start_col as u32 - 1,
			},
		};

		let full_range = documents::tokens(uri)
			.and_then(|tokens| {
				tokens.iter().find_map(|token| {
					let tok_range = token.range();
					if range.is_within(&tok_range) {
						Some(tok_range)
					} else {
						None
					}
				})
			})
			.or_else(|| {
				documents::parse(uri).and_then(|ast| {
					ast.parent_of(&range, ParentGranularity::Expr)
						.map(|node| match node {
							AstNode::Decl(decl) => decl.range(),
							AstNode::Stmt(stmt) => stmt.range(),
							AstNode::Expr(expr) => expr.range(),
						})
				})
			})
			.unwrap_or(range);

		DiagnosticBuilder::new(kind)
			.range(full_range)
			.severity(DiagnosticSeverity::Error)
			.message(format!("CompileError: {}", self))
			.build()
	}
}

macro_rules! lookup_token {
	($scopes:ident, $variant:ident($name:ident)) => {
		$scopes.iter().rev().find_map(|(_, map)| {
			map.get($name).and_then(|value| match value.as_ref() {
				Decl::$variant(ref decl) => Some(decl.name.clone()),
				_ => None,
			})
		})
	};
}

impl IntoDiagnostic for naga::valid::ValidationError {
	fn into_diagnostic(self, kind: ErrorKind, uri: Option<&Url>, _: Option<&str>) -> Diagnostic {
		use naga::valid::ValidationError::*;

		let first_line = Range {
			start: Position {
				line: 0,
				character: 0,
			},
			end: Position {
				line: 1,
				character: 0,
			},
		};

		let uri = uri.expect("uri must be provided for naga ParseError");
		let scopes = match documents::scopes(uri) {
			Some(scopes) => scopes,
			None => {
				return DiagnosticBuilder::new(kind)
					.range(first_line)
					.severity(DiagnosticSeverity::Error)
					.message(format!("ValidationError: {}", self))
					.build()
			}
		};

		let (token, message) = match &self {
			Layouter(_) => (None, self.to_string()),
			Type { error, .. } => {
				let token = None; // TODO
				eprintln!("{:#?}", error);
				(token, error.to_string())
			}
			Constant { name, error, .. } => {
				let token = lookup_token!(scopes, Const(name));
				eprintln!("{:#?}", error);
				(token, error.to_string())
			}
			GlobalVariable { name, error, .. } => {
				let token = lookup_token!(scopes, Var(name));
				eprintln!("{:#?}", error);
				(token, error.to_string())
			}
			Function { name, error, .. } => {
				let token = lookup_token!(scopes, Function(name));
				eprintln!("{:#?}", error);
				(token, error.to_string())
			}
			EntryPoint { name, error, .. } => {
				let token = lookup_token!(scopes, Function(name));
				(token, error.to_message())
			}
			Corrupted => (None, self.to_string()),
		};

		// FIXME: Use a real range once https://github.com/gfx-rs/naga/issues/358 is fixed upstream
		let range = match token {
			Some(token) => token.range(),
			None => first_line,
		};

		DiagnosticBuilder::new(kind)
			.range(range)
			.severity(DiagnosticSeverity::Error)
			.message(format!("ValidationError: {}", message))
			.build()
	}
}

trait ToMessage {
	fn to_message(&self) -> String;
}

impl ToMessage for EntryPointError {
	fn to_message(&self) -> String {
		use EntryPointError::*;

		match self {
			Argument(_, err) | Result(err) => err.to_string(),
			Function(err) => err.to_string(),
			_ => self.to_string(),
		}
	}
}

trait AsRange {
	fn as_range(&self) -> Range;
}

impl AsRange for pest::error::LineColLocation {
	fn as_range(&self) -> Range {
		use pest::error::LineColLocation::*;

		match self {
			Pos((line, col)) => Range {
				start: Position {
					line: *line as u32 - 1,
					character: *col as u32 - 1,
				},
				end: Position {
					line: *line as u32 - 1,
					character: *col as u32,
				},
			},
			Span((start_line, start_col), (end_line, end_col)) => Range {
				start: Position {
					line: *start_line as u32 - 1,
					character: *start_col as u32 - 1,
				},
				end: Position {
					line: *end_line as u32 - 1,
					character: *end_col as u32 - 1,
				},
			},
		}
	}
}
