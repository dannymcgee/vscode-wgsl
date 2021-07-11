use std::{
	collections::HashMap,
	sync::{Arc, RwLock},
	thread,
	time::Duration,
};

use anyhow::{anyhow, Result};
use crossbeam_channel::{self as chan, Receiver, Sender, TryRecvError};
use lsp_types::{DidChangeTextDocumentParams, Url};
use ropey::Rope;

lazy_static! {
	static ref DOCS: Documents = Documents::default();
}

type DocumentReader = (Sender<()>, Receiver<String>);
type DocumentWriter = (
	Sender<()>,
	Sender<DidChangeTextDocumentParams>,
	Receiver<String>,
);

#[derive(Debug, Default)]
pub struct Documents {
	store: Arc<RwLock<HashMap<Url, DocumentWriter>>>,
}

unsafe impl Send for Documents {}
unsafe impl Sync for Documents {}

impl Clone for Documents {
	fn clone(&self) -> Self {
		Self {
			store: Arc::clone(&self.store),
		}
	}
}

impl Documents {
	pub fn new() -> Self {
		Self::default()
	}

	pub fn open(&self, uri: &Url, content: String) {
		let (event_writer, event_reader) = chan::unbounded();
		let mut doc = Document::new(event_reader, content);
		let (read_requester, update_receiver) = doc.reader();
		let mut store = self.store.write().unwrap();

		store.insert(uri.clone(), (read_requester, event_writer, update_receiver));
	}

	pub fn close(&self, _: &Url) {
		todo!()
	}

	pub fn read(&self, uri: &Url) -> Result<String> {
		let response = if let Some((tx, _, rx)) = self.store.read().unwrap().get(uri) {
			tx.send(())?;
			rx.recv().map_err(|err| anyhow!("{}", err))
		} else {
			Err(anyhow!("No entry found for uri {:?}", uri))
		};

		response
	}

	pub fn update(&self, params: DidChangeTextDocumentParams) -> Result<()> {
		eprintln!("{:#?}", params);

		if let Some((_, tx, _)) = self.store.read().unwrap().get(&params.text_document.uri) {
			tx.send(params).map_err(|err| anyhow!("{}", err))
		} else {
			Err(anyhow!(
				"No entry found for uri {:?}",
				params.text_document.uri
			))
		}
	}
}

#[derive(Debug)]
struct Document {
	content: Arc<RwLock<Rope>>,
	update_events: Receiver<DidChangeTextDocumentParams>,
	read_requests: Option<Receiver<()>>,
	updates: Option<Sender<String>>,
}

unsafe impl Send for Document {}
unsafe impl Sync for Document {}

impl Document {
	fn new(event_reader: Receiver<DidChangeTextDocumentParams>, content: String) -> Self {
		Self {
			content: Arc::new(RwLock::new(content.into())),
			update_events: event_reader,
			read_requests: None,
			updates: None,
		}
	}

	fn reader(&mut self) -> DocumentReader {
		let (request_sender, request_receiver) = chan::unbounded();
		let (update_writer, update_reader) = chan::unbounded();

		self.read_requests = Some(request_receiver);
		self.updates = Some(update_writer);
		self.start_loop();

		(request_sender, update_reader)
	}

	fn start_loop(&self) {
		let update_events = self.update_events.clone();
		let read_requests = self.read_requests.as_ref().unwrap().clone();
		let updates = self.updates.as_ref().unwrap().clone();
		let content = self.content.clone();

		thread::spawn(move || loop {
			// Check for update events
			match update_events.try_recv() {
				Ok(event) => {
					for update in event.content_changes {
						let mut writable = content.write().unwrap();
						let range = update.range.unwrap();

						let start_line = writable.line_to_char(range.start.line as usize);
						let edit_start = start_line + range.start.character as usize;

						let end_line = writable.line_to_char(range.end.line as usize);
						let edit_end = end_line + range.end.character as usize;

						if edit_end - edit_start > 0 {
							writable.remove(edit_start..edit_end);
						}
						writable.insert(edit_start, &update.text);
					}
				}
				Err(TryRecvError::Disconnected) => {
					// TODO: close all channels and break loop
				}
				_ => {}
			}

			// Check for read requests
			#[allow(clippy::redundant_pattern_matching)]
			match read_requests.try_recv() {
				Ok(_) => {
					let readable = content.read().unwrap();
					let update = readable.to_string();

					if let Err(_) = updates.send(update) {
						// TODO: close all channels and break loop
					}
				}
				Err(TryRecvError::Disconnected) => {
					// TODO: close all channels and break loop
				}
				_ => {}
			}

			// TODO: Sleep?
			thread::sleep(Duration::from_millis(17));
		});
	}
}
