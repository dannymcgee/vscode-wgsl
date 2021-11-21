use std::sync::Arc;

use gramatika::{Spanned, Token as _};
use lsp_server::{Message, RequestId, Response};
use lsp_types::{Location, Position, ReferenceParams, Url};
use parser_v2::{decl::Decl, utils::ToRange, Token, TokenKind};
use serde_json as json;
use wgsl_plus::ResolveImportPath;

use crate::documents::Documents;

pub fn handle(id: RequestId, params: ReferenceParams, docs: &Documents) -> Message {
	let uri = &params.text_document_position.text_document.uri;
	let pos = params.text_document_position.position;
	let include_decl = params.context.include_declaration;

	let result = find_all(uri, pos, include_decl, docs).unwrap_or_else(Vec::new);

	Message::Response(Response {
		id,
		result: Some(json::to_value(&result).unwrap()),
		error: None,
	})
}

pub fn find_all(
	uri: &Url,
	pos: Position,
	include_decl: bool,
	docs: &Documents,
) -> Option<Vec<Location>> {
	let document = docs.get(uri)?;
	let scopes = Arc::clone(&document.scopes);

	let needle = document.tokens.iter().find(|token| {
		let range = token.span().to_range();
		pos >= range.start && pos <= range.end
	})?;

	let found = docs.find_decl(uri, needle)?;
	let needle_decl = found.decl;

	let storage_modifier = match needle_decl.as_ref() {
		Decl::Struct(decl) => decl.storage_modifier.as_ref(),
		Decl::Function(decl) => decl.storage_modifier.as_ref(),
		_ => None,
	};

	let mut references = Vec::with_capacity(8);

	// If the declaration is an export, we need to check all dependent documents for references
	if matches!(
		storage_modifier.map(|t| t.as_matchable()),
		Some((TokenKind::Keyword, "export", _))
	) {
		// The "Find references" command could be invoked on an imported reference in one of
		// the dependent documents, so we need to be sure of the source URI
		let uri = if let Some(decl) = found.source_module {
			match decl.as_ref() {
				Decl::Module(module) => uri.resolve_import(module.path().as_str()),
				_ => unreachable!(),
			}
		} else {
			uri.clone()
		};

		// Find references to the needle in each of the dependent documents
		for uri in docs.get_dependents_of(&uri).iter() {
			let tokens = &docs.get(uri)?.tokens;

			references.extend(tokens.iter().filter_map(|token| {
				if token.is_reference(
					needle,
					needle_decl.as_ref(),
					uri,
					docs,
					include_decl,
					|_| true, // imports are global to the document, so no need to worry about scope
				) {
					Some(Location::at(uri, token))
				} else {
					None
				}
			}))
		}
	}
	// This is a plain, local declaration, so just find references in the current document
	else {
		let tokens = &document.tokens;
		let scope = scopes.find(needle_decl.name())?;

		references.extend(tokens.iter().filter_map(|token| {
			if token.is_reference(
				needle,
				needle_decl.as_ref(),
				uri,
				docs,
				include_decl,
				|token| scope.span().contains(token.span()),
			) {
				Some(Location::at(uri, token))
			} else {
				None
			}
		}));
	}

	references.sort_by(|a, b| {
		if a.uri != b.uri {
			a.uri.cmp(&b.uri)
		} else if a.range.start.line != b.range.start.line {
			a.range.start.line.cmp(&b.range.start.line)
		} else {
			a.range.start.character.cmp(&b.range.start.character)
		}
	});

	Some(references)
}

trait IsReference {
	fn is_reference(
		&self,
		needle: &Token,
		needle_decl: &Decl,
		uri: &Url,
		docs: &Documents,
		include_decl: bool,
		scope_predicate: impl Fn(&Token) -> bool,
	) -> bool;

	fn is_declaration(&self, decl: &Decl) -> bool;
}

impl IsReference for Token {
	fn is_reference(
		&self,
		needle: &Token,
		needle_decl: &Decl,
		uri: &Url,
		docs: &Documents,
		include_decl: bool,
		scope_predicate: impl Fn(&Token) -> bool,
	) -> bool {
		if self.is_declaration(needle_decl) {
			return include_decl;
		}

		self.lexeme() == needle.lexeme()
			&& scope_predicate(self)
			&& match docs.find_decl(uri, self) {
				Some(found) => found.decl.span() == needle_decl.span(),
				None => false,
			}
	}

	fn is_declaration(&self, decl: &Decl) -> bool {
		self.span() == decl.name().span()
	}
}

trait AtUriAndToken {
	fn at(uri: &Url, token: &Token) -> Self;
}

impl AtUriAndToken for Location {
	fn at(uri: &Url, token: &Token) -> Self {
		Self {
			uri: uri.clone(),
			range: token.span().to_range(),
		}
	}
}
