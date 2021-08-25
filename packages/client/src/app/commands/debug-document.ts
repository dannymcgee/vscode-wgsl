import { ViewColumn, window, workspace } from "vscode";

import {
	DebugAstProvider,
	DebugDocumentProvider,
	DebugTokensProvider,
} from "../contentProviders";
import { Ctor } from "../util";

async function debugDocument(Provider: Ctor<DebugDocumentProvider>) {
	let provider = new Provider();
	let uri = provider.uri;
	let document = await workspace.openTextDocument(uri);

	provider.emitter.fire(uri);

	await window.showTextDocument(document, {
		viewColumn: ViewColumn.Two,
		preserveFocus: true,
	});
}

export function debugAst() {
	return debugDocument(DebugAstProvider);
}

export function debugTokens() {
	return debugDocument(DebugTokensProvider);
}
