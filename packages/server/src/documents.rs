use std::{
	collections::HashMap,
	fs, io,
	path::{Path, PathBuf},
	sync::{Arc, RwLock},
};

use anyhow::Result;
use lsp_types::{TextDocumentIdentifier, Url};

lazy_static! {
	static ref STORE: Store = Store::default();
}

#[derive(Debug, Default)]
struct Store {
	contents: Arc<RwLock<HashMap<Url, String>>>,
}

pub fn read(doc: &TextDocumentIdentifier) -> Result<String> {
	let readable = STORE.contents.read().unwrap();
	if let Some(content) = readable.get(&doc.uri) {
		eprintln!("Retrieving content from cache");
		return Ok(content.clone());
	}
	drop(readable);

	eprintln!("Reading from file system");
	let result = doc.read();
	if let Ok(ref content) = result {
		let mut writable = STORE.contents.write().unwrap();
		writable.insert(doc.uri.clone(), content.clone());
	}

	result.map_err(|err| err.into())
}

pub trait Reader {
	fn read(&self) -> io::Result<String>;
}

impl Reader for TextDocumentIdentifier {
	fn read(&self) -> io::Result<String> {
		let path = PathBuf::from_uri(&self.uri);
		fs::read_to_string(&path)
	}
}

trait FromUri {
	fn from_uri(input: &Url) -> Self;
}

impl FromUri for PathBuf {
	#[cfg(target_family = "windows")]
	fn from_uri(input: &Url) -> Self {
		let string = urlencoding::decode(input.path()).unwrap();
		Path::new(&string[1..]).to_path_buf()
	}

	#[cfg(not(target_family = "windows"))]
	fn from_uri(input: &Url) -> Self {
		Path::new(input.path()).to_path_buf()
	}
}
