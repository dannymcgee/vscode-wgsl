import {
	CancellationToken,
	Location,
	Position,
	Range,
	ReferenceContext,
	ReferenceProvider,
	TextDocument,
	Uri,
} from "vscode";
import { ReferenceParams, ReferencesRequest } from "vscode-languageserver-protocol";
import ctx from "../context";

// prettier-ignore
export class References implements ReferenceProvider {
	async provideReferences(
		document: TextDocument,
		position: Position,
		context: ReferenceContext,
		_: CancellationToken
	) {
		let params: ReferenceParams = {
			context,
			textDocument: {
				...document,
				uri: document.uri.toString(),
			},
			position,
		};

		let result = await ctx.get()?.client.sendRequest(
			ReferencesRequest.type,
			params
		);

		return result.map(({ uri, range }) => new Location(
			Uri.parse(uri),
			new Range(
				new Position(range.start.line, range.start.character),
				new Position(range.end.line, range.end.character),
			),
		));
	}
}
