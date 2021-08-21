use std::fmt::Write;

use gramatika::Spanned;
use lsp_server::{Message, RequestId, Response};
use lsp_types::{Hover, HoverContents, HoverParams, MarkedString};
use parser_v2::{decl::Decl, utils::ToRange};
use serde_json as json;

use crate::documents_v2::Documents;

pub fn handle(id: RequestId, params: HoverParams, docs: &Documents) -> Message {
	let uri = params.text_document_position_params.text_document.uri;
	let pos = params.text_document_position_params.position;

	let result = docs.get(&uri).and_then(|document| {
		let tokens = &document.tokens;
		let needle = tokens.iter().find(|token| {
			let range = token.span().to_range();
			pos >= range.start && pos <= range.end
		})?;

		docs.find_decl(uri.clone(), *needle)
			.map(|found| (needle, found))
	});

	match result {
		Some((needle, found)) => {
			let mut contents = vec![];

			if let Some(module) = found.source_module {
				contents.push(MarkedString::from_language_code(
					"wgsl".into(),
					format!("{}", module.as_ref()),
				));
			}

			let main_content = match found.parent {
				Some(inner) => match inner.as_ref() {
					Decl::Struct(inner) => {
						let mut f = String::new();

						write!(&mut f, "{}", inner.storage).unwrap();

						if let Some(modifier) = inner.storage_modifier {
							write!(&mut f, "<{}>", modifier).unwrap();
						}

						writeln!(&mut f, " {} {{ \u{22ef}", inner.name).unwrap();
						writeln!(&mut f, "\t{}", found.decl.as_ref()).unwrap();
						writeln!(&mut f, "}};").unwrap();

						f
					}
					_ => unreachable!(),
				},
				_ => format!("{}", found.decl.as_ref()),
			};

			contents.push(MarkedString::from_language_code(
				"wgsl".into(),
				main_content,
			));

			let result = Hover {
				contents: HoverContents::Array(contents),
				range: Some(needle.span().to_range()),
			};

			Message::Response(Response {
				id,
				result: Some(json::to_value(&result).unwrap()),
				error: None,
			})
		}
		None => Message::Response(Response {
			id,
			result: Some(
				json::to_value(&Hover {
					contents: HoverContents::Array(vec![]),
					range: None,
				})
				.unwrap(),
			),
			error: None,
		}),
	}
}
