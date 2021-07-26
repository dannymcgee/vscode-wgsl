use std::{collections::HashMap, sync::Arc, thread, time::Duration};

use anyhow::{bail, Result};
use crossbeam::channel::Sender;
use dashmap::DashMap;
use itertools::Itertools;
use lsp_server::{Message, Notification};
use lsp_types::{
	notification::Notification as NotificationTrait, DidChangeTextDocumentParams,
	DidOpenTextDocumentParams, Range, Url,
};
use parser::{
	ast::{Decl, Stmt, Token},
	FlatTokens, GetRange, NamespaceScope, Scope, Scopes,
};
use ropey::Rope;
use serde_json as json;

use crate::{
	diagnostics::{self, ErrorKind},
	extensions::{UnreadDependency, UnreadDependencyParams},
};

// TODO - This module desperately needs a refactor

lazy_static! {
	static ref DOCS: Arc<DashMap<Url, Document>> = Arc::new(DashMap::default());
	static ref PENDING: Arc<DashMap<Url, Document>> = Arc::new(DashMap::default());
}

fn docs() -> Arc<DashMap<Url, Document>> {
	Arc::clone(&DOCS)
}
fn docs_mut() -> Arc<DashMap<Url, Document>> {
	let count = Arc::strong_count(&DOCS);
	if count > 1 {
		eprintln!(
			"!! WARNING !! accessed DOCS mutably with {} existing references",
			count
		);
	}
	Arc::clone(&DOCS)
}

fn pending() -> Arc<DashMap<Url, Document>> {
	Arc::clone(&PENDING)
}
fn pending_mut() -> Arc<DashMap<Url, Document>> {
	let count = Arc::strong_count(&PENDING);
	if count > 1 {
		eprintln!(
			"!! WARNING !! accessed PENDING mutably with {} existing references",
			count
		);
	}
	Arc::clone(&PENDING)
}

macro_rules! wait_for {
	($pending:ident, $docs:ident[$key:ident] $adapter:ident => $func:ident()) => {
		loop {
			if !$pending().contains_key($key) && $docs().contains_key($key) {
				break $docs().get($key).$adapter(|entry| entry.value().$func());
			}
			thread::sleep(Duration::from_millis(10));
		}
	};
}

pub fn open(params: DidOpenTextDocumentParams, tx: Sender<Message>) {
	thread::spawn(move || {
		let uri = &params.text_document.uri;
		if docs().contains_key(uri) {
			return;
		}
		let document = Document::new(uri, &params.text_document.text);

		if !document.is_ready() {
			document.request_deps(tx);
			pending_mut().insert(uri.clone(), document);
		} else {
			docs_mut().insert(uri.clone(), document);

			let done = pending_mut()
				.iter_mut()
				.filter_map(|mut entry| {
					let doc = entry.value_mut();
					doc.update_deps();
					doc.update_scopes();

					if doc.is_ready() {
						Some(entry.key().clone())
					} else {
						None
					}
				})
				.collect_vec();

			for key in done {
				if let Some((_, doc)) = pending_mut().remove(&key) {
					doc.validate();
					docs_mut().insert(key, doc);
				}
			}
		}
	});
}

pub fn close(uri: &Url) {
	docs_mut().remove(uri);
}

pub fn read(uri: &Url) -> Option<String> {
	wait_for!(pending, docs[uri] map => read())
}

pub fn parse(uri: &Url) -> Option<Arc<Vec<Decl>>> {
	wait_for!(pending, docs[uri] and_then => parse())
}

pub fn scopes(uri: &Url) -> Option<Arc<Scopes>> {
	wait_for!(pending, docs[uri] and_then => scopes())
}

pub fn tokens(uri: &Url) -> Option<Arc<Vec<Token>>> {
	wait_for!(pending, docs[uri] and_then => tokens())
}

pub fn update(params: &DidChangeTextDocumentParams, tx: Sender<Message>) -> Result<()> {
	let uri = &params.text_document.uri;

	if let Some(mut doc) = docs_mut().get_mut(uri) {
		doc.value_mut().update(&params)?;
	} else if let Some(mut doc) = pending_mut().get_mut(uri) {
		doc.value_mut().update(&params)?;
	} else {
		bail!("No entry found for uri '{}'", uri);
	}

	if docs().contains_key(uri) && !docs().get(uri).unwrap().is_ready() {
		let (key, doc) = docs_mut().remove(uri).unwrap();
		doc.request_deps(tx);

		pending_mut().insert(key, doc);
	} else if pending().contains_key(uri) {
		if pending().get(uri).unwrap().is_ready() {
			let (key, doc) = pending_mut().remove(uri).unwrap();

			docs_mut().insert(key, doc);
		} else {
			pending().get(uri).unwrap().request_deps(tx);
		}
	}

	Ok(())
}

#[derive(Debug)]
struct Document {
	uri: Url,
	text: Rope,
	deps: Option<Arc<Vec<Url>>>,
	ast: Option<Arc<Vec<Decl>>>,
	scopes: Option<Arc<Scopes>>,
	tokens: Option<Arc<Vec<Token>>>,
}

impl Document {
	fn new(uri: &Url, input: &str) -> Self {
		let text: Rope = input.into();
		let ast = match parser::parse_ast(input) {
			Ok(ast) => Some(Arc::new(ast)),
			Err(err) => {
				diagnostics::report_error(uri, err, ErrorKind::ParseError);
				None
			}
		};

		let tokens = ast.as_ref().map(|ast| {
			let mut tokens = vec![];
			ast.flat_tokens(&mut tokens);

			Arc::new(tokens)
		});

		let deps = ast.as_ref().map(|ast| Arc::new(ast.resolve_deps(uri)));

		let scopes = ast
			.as_ref()
			.map(|ast| Arc::new(Scopes::new(ast.clone(), uri)));

		let result = Self {
			uri: uri.clone(),
			text,
			ast,
			deps,
			scopes,
			tokens,
		};

		if result.is_ready() {
			result.validate();
		}

		result
	}

	fn read(&self) -> String {
		self.text.clone().into()
	}

	fn parse(&self) -> Option<Arc<Vec<Decl>>> {
		self.ast.as_ref().map(|ast| Arc::clone(&ast))
	}

	fn scopes(&self) -> Option<Arc<Scopes>> {
		self.scopes.as_ref().map(|scopes| Arc::clone(&scopes))
	}

	fn tokens(&self) -> Option<Arc<Vec<Token>>> {
		self.tokens.as_ref().map(|tokens| Arc::clone(&tokens))
	}

	fn is_ready(&self) -> bool {
		self.deps
			.as_ref()
			.map(|deps| deps.iter().all(|dep| docs().contains_key(dep)))
			.unwrap_or(false)
	}

	fn is_compilable(&self) -> bool {
		self.ast
			.as_ref()
			.map(|ast| {
				ast.iter().any(|decl| match decl {
					Decl::Function(decl) => decl
						.attributes
						.as_ref()
						.map(|attributes| {
							attributes.iter().any(|attr| attr.name.as_str() == "stage")
						})
						.unwrap_or(false),
					_ => false,
				})
			})
			.unwrap_or(false)
	}

	fn update(&mut self, params: &DidChangeTextDocumentParams) -> Result<()> {
		diagnostics::clear_errors(&self.uri);

		for update in &params.content_changes {
			let range = update.range.unwrap();

			let start_line = self.text.line_to_char(range.start.line as usize);
			let edit_start = start_line + range.start.character as usize;

			let end_line = self.text.line_to_char(range.end.line as usize);
			let edit_end = end_line + range.end.character as usize;

			if edit_end - edit_start > 0 {
				self.text.remove(edit_start..edit_end);
			}
			self.text.insert(edit_start, &update.text);
		}

		self.ast = match parser::parse_ast(&self.text.to_string()) {
			Ok(ast) => Some(Arc::new(ast)),
			Err(err) => {
				diagnostics::report_error(&self.uri, err, ErrorKind::ParseError);
				None
			}
		};

		self.update_tokens();
		self.update_deps();
		self.update_scopes();

		if self.is_ready() {
			self.validate();
		}

		Ok(())
	}

	fn update_tokens(&mut self) {
		self.tokens = self.ast.as_ref().map(|ast| {
			let mut tokens = vec![];
			ast.flat_tokens(&mut tokens);

			Arc::new(tokens)
		});
	}

	fn update_deps(&mut self) {
		self.deps = self
			.ast
			.as_ref()
			.map(|ast| Arc::new(ast.resolve_deps(&self.uri)));
	}

	fn update_scopes(&mut self) {
		self.scopes = self
			.ast
			.as_ref()
			.map(|ast| Arc::new(Scopes::new(ast.clone(), &self.uri)));
	}

	fn request_deps(&self, tx: Sender<Message>) {
		if let Some(deps) = self.deps.as_ref() {
			for dep in deps.iter().filter(|dep| !docs().contains_key(*dep)) {
				let params = UnreadDependencyParams {
					dependency: dep.clone(),
					dependant: self.uri.clone(),
				};

				tx.send(Message::Notification(Notification {
					method: UnreadDependency::METHOD.into(),
					params: json::to_value(&params).unwrap(),
				}))
				.unwrap();
			}
		}
	}

	fn validate(&self) {
		if let Some(ref ast) = self.ast {
			if ast.iter().any(|decl| matches!(decl, Decl::Extension(_))) {
				use wgsl_plus::TranspileError::*;

				match self.transpile() {
					Ok(text) if self.is_compilable() => {
						diagnostics::validate(self.uri.clone(), text)
					}
					Err(Multiple(errs)) => {
						for err in errs {
							diagnostics::report_error(&self.uri, err, ErrorKind::TranspileError);
						}
					}
					Err(single) => {
						diagnostics::report_error(&self.uri, single, ErrorKind::TranspileError)
					}
					_ => {}
				}
			} else {
				diagnostics::validate(self.uri.clone(), self.text.to_string());
			}
		}
	}

	fn transpile(&self) -> wgsl_plus::Result<String> {
		let ast = self
			.ast
			.as_ref()
			.expect("Cannot transpile an unparsed document");

		let scopes = self
			.scopes
			.as_ref()
			.expect("Cannot transpile an unparsed document");

		wgsl_plus::transpile_parsed(ast.clone(), scopes.as_ref())
	}
}

trait FromAstAndUri {
	fn new(ast: Arc<Vec<Decl>>, uri: &Url) -> Self;
}

impl FromAstAndUri for Scopes {
	fn new(ast: Arc<Vec<Decl>>, uri: &Url) -> Self {
		if ast.is_empty() {
			return Self {
				ast: Arc::new(vec![]),
				namespaces: HashMap::default(),
				inner: vec![],
			};
		}

		let mut scopes: Vec<(Range, Arc<Scope>)> = vec![];

		let global_start = ast.first().unwrap().range().start;
		let global_end = ast.last().unwrap().range().end;

		let global_range = Range {
			start: global_start,
			end: global_end,
		};

		let mut global_scopes = HashMap::new();
		let mut namespaces = HashMap::new();

		for decl in ast.iter() {
			match decl {
				Decl::Module(decl) => {
					let name = decl.name.to_string();
					let (path, _) = decl.path.borrow_inner();
					let dep_uri = uri.resolve_import(&path[1..path.len() - 1]);

					if let Some(dep_ast) =
						docs().get(&dep_uri).and_then(|entry| entry.value().parse())
					{
						let scope = dep_ast
							.exports()
							.into_iter()
							.map(|decl| (decl.ident().to_string(), Arc::new(decl)))
							.collect();

						namespaces.insert(
							name,
							NamespaceScope {
								uri: dep_uri.clone(),
								scope,
							},
						);
					}
				}
				_ => {
					let name = decl.ident().to_string();
					global_scopes.insert(name, Arc::new(decl.clone())); // TODO
				}
			}
		}

		scopes.push((global_range, Arc::new(global_scopes)));

		for decl in ast.iter() {
			if let Decl::Function(inner) = decl {
				let mut current_scopes = HashMap::new();
				let range = inner.range;

				for param in &inner.signature.params {
					let name = param.name.to_string();
					current_scopes.insert(name, Arc::new(Decl::Param(param.clone()))); // TODO
				}

				let (_, stmts) = &inner.body;
				for stmt in stmts {
					if let Stmt::Variable(var_decl) = stmt {
						let name = var_decl.name.to_string();
						current_scopes.insert(name, Arc::new(Decl::Var(var_decl.clone()))); // TODO
					}
				}

				scopes.push((range, Arc::new(current_scopes)));
			}
		}

		scopes.reverse();

		Self {
			ast,
			namespaces,
			inner: scopes,
		}
	}
}

trait ResolveDeps {
	fn resolve_deps(&self, source: &Url) -> Vec<Url>;
}

impl ResolveDeps for Vec<Decl> {
	fn resolve_deps(&self, source: &Url) -> Vec<Url> {
		self.iter()
			.filter_map(|decl| match decl {
				Decl::Module(inner) => {
					let (quoted, _) = inner.path.borrow_inner();
					let slice = &quoted[1..quoted.len() - 1];
					let uri = source.resolve_import(slice);

					Some(uri)
				}
				_ => None,
			})
			.collect()
	}
}

trait GetExports {
	fn exports(&self) -> Vec<Decl>;
}

impl GetExports for Vec<Decl> {
	fn exports(&self) -> Vec<Decl> {
		self.iter()
			.filter(|decl| match decl {
				Decl::Struct(inner) => inner.storage_modifier.as_ref().map_or(false, |token| {
					let (kw, _) = token.borrow_inner();
					kw == "export"
				}),
				Decl::Function(inner) => inner.storage_modifier.as_ref().map_or(false, |token| {
					let (kw, _) = token.borrow_inner();
					kw == "export"
				}),
				_ => false,
			})
			.cloned()
			.collect()
	}
}

trait ResolveImportPath {
	fn resolve_import(&self, path: &str) -> Url;
}

impl ResolveImportPath for Url {
	fn resolve_import(&self, path: &str) -> Self {
		let mut resolved = self.clone();
		{
			let resolved_segs = &mut resolved.path_segments_mut().unwrap();
			resolved_segs.pop();

			let segments = path.split('/');
			for segment in segments {
				match segment {
					"." => {}
					".." => {
						resolved_segs.pop();
					}
					other => {
						resolved_segs.push(other);
					}
				}
			}
		}

		resolved.set_path(&format!("{}.wgsl", resolved.path()));

		resolved
	}
}
