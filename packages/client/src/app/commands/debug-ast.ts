import { ViewColumn, window, workspace } from "vscode";
import { DebugAstDocument } from "../providers";

export async function debugAst() {
	let provider = new DebugAstDocument();
	let uri = provider.uri;
	let document = await workspace.openTextDocument(uri);

	provider.emitter.fire(uri);

	await window.showTextDocument(document, {
		viewColumn: ViewColumn.Two,
		preserveFocus: true,
	});
}
