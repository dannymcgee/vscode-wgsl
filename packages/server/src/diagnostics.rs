use std::sync::Arc;

use crossbeam::channel::Sender;
use dashmap::DashMap;
use lsp_server::Message;
use lsp_types::{Diagnostic, Url};
use naga::valid::{Capabilities, ValidationFlags, Validator};

pub struct Diagnostics {
	ipc: Sender<Message>,
	collection: Arc<DashMap<Url, Vec<Diagnostic>>>,
	validator: Arc<Validator>,
}

impl Diagnostics {
	pub fn new(ipc: Sender<Message>) -> Self {
		Self {
			ipc,
			collection: Default::default(),
			validator: Arc::new(Validator::new(ValidationFlags::all(), Capabilities::all())),
		}
	}
}
