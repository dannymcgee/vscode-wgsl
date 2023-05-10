use std::{
	sync::Arc,
	thread::{self, JoinHandle},
	time::Duration,
};

use crossbeam::channel::{self, Sender, TrySendError::Disconnected};
use dashmap::DashMap;
use gramatika::{once_cell::sync::OnceCell, SpannedError};
use lsp_server::{Message, Notification};
use lsp_types::{
	notification::{Notification as _, PublishDiagnostics},
	Diagnostic, PublishDiagnosticsParams, Url,
};
use naga::{
	front::wgsl as naga_wgsl,
	valid::{Capabilities, ValidationFlags, Validator},
};
use parking_lot::Mutex;
use parser_v2::traversal::{Visitor, Walk};
use serde_json as json;

use crate::documents::Document;

use self::conv::{ErrorKind, IntoDiagnostics};

mod builder;
mod conv;

static INSTANCE: OnceCell<Arc<Diagnostics>> = OnceCell::new();

#[derive(Clone, Debug)]
pub struct Diagnostics {
	ipc: Sender<Message>,
	dirty_notifier: Sender<()>,
	collection: Arc<DashMap<Url, Vec<Diagnostic>>>,
	validator: Arc<Mutex<Validator>>,
}

impl Diagnostics {
	pub fn global() -> Arc<Self> {
		Arc::clone(INSTANCE.get().unwrap())
	}

	pub fn init(ipc: Sender<Message>) -> JoinHandle<()> {
		let (dirty_notifier, dirty_events) = channel::bounded(1);
		INSTANCE
			.set(Arc::new(Self {
				ipc,
				dirty_notifier,
				collection: Default::default(),
				validator: Arc::new(Mutex::new(Validator::new(
					ValidationFlags::all(),
					Capabilities::all(),
				))),
			}))
			.unwrap();

		thread::spawn(|| {
			let this = Self::global();

			for _ in dirty_events {
				this.publish();
			}
		})
	}

	pub fn validate(&self, doc: &Document) {
		let parse_errors = ParseErrors::collect(doc);
		if !parse_errors.is_empty() {
			return self.report_error(doc, parse_errors, ErrorKind::Parse);
		}

		match self.validator.try_lock() {
			Some(mut validator) => match naga_wgsl::parse_str(&doc.source) {
				Ok(ref module) => match validator.validate(module) {
					Ok(_) => {
						self.clear_errors(doc.uri.as_ref());
					}
					Err(err) => {
						self.report_error(doc, err, ErrorKind::NagaValidation);
					}
				},
				Err(err) => {
					self.report_error(doc, err, ErrorKind::NagaParse);
				}
			},
			None => {
				thread::sleep(Duration::from_millis(1));
				self.validate(doc)
			}
		}
	}

	pub fn report_error<E>(&self, doc: &Document, err: E, kind: ErrorKind)
	where E: IntoDiagnostics {
		let diags = err.into_diagnostics(kind, doc);

		if let Some(mut entry) = self.collection.get_mut(&doc.uri) {
			*entry.value_mut() = diags;
		} else {
			self.collection.insert(doc.uri.as_ref().clone(), diags);
		}

		if let Err(Disconnected(err)) = self.dirty_notifier.try_send(()) {
			panic!("{:?}", err);
		}
	}

	pub fn clear_errors(&self, uri: &Url) {
		if let Some(mut entry) = self.collection.get_mut(uri) {
			entry.value_mut().clear();

			if let Err(Disconnected(err)) = self.dirty_notifier.try_send(()) {
				panic!("{:?}", err);
			}
		}
	}

	fn publish(&self) {
		for entry in self.collection.iter() {
			let uri = entry.key().clone();
			let diagnostics = entry.value().clone();

			self.ipc
				.send(Message::Notification(Notification {
					method: PublishDiagnostics::METHOD.into(),
					params: json::to_value(PublishDiagnosticsParams {
						diagnostics,
						uri,
						version: None,
					})
					.unwrap(),
				}))
				.unwrap();
		}
	}
}

#[derive(Default)]
struct ParseErrors {
	errors: Vec<SpannedError>,
}

impl ParseErrors {
	fn collect(document: &Document) -> Vec<SpannedError> {
		let mut visitor = Self::default();
		document.ast.walk(&mut visitor);

		visitor.errors
	}
}

impl Visitor for ParseErrors {
	fn visit_error(&mut self, expr: &SpannedError) {
		self.errors.push(expr.clone());
	}
}
