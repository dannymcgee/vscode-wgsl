import {
	CancellationToken,
	EventEmitter,
	TextDocumentChangeEvent,
	TextDocumentContentProvider,
	TextEditor,
	Uri,
	window,
	workspace,
} from "vscode";

import ctx from "../context";
import ext, { DebugAstParams } from "../extensions";

export class DebugAstDocument implements TextDocumentContentProvider {
	readonly uri = Uri.parse("wgsl://debug/syntax-tree.wgsl-ast");
	readonly emitter = new EventEmitter<Uri>();

	// prettier-ignore
	constructor() {
		ctx.get()?.subscriptions.push(
			workspace.onDidChangeTextDocument(this.onDidChangeTextDocument, this),
			window.onDidChangeActiveTextEditor(this.onDidChangeActiveTextEditor, this)
		);
	}

	get onDidChange() {
		return this.emitter.event;
	}

	async onDidChangeTextDocument(event: TextDocumentChangeEvent) {
		if (event.document.languageId !== "wgsl") return;

		await sleep(10);
		this.emitter.fire(this.uri);
	}

	onDidChangeActiveTextEditor(editor?: TextEditor) {
		if (editor?.document.languageId !== "wgsl") return;

		this.emitter.fire(this.uri);
	}

	async provideTextDocumentContent(_: Uri, onCancel: CancellationToken) {
		let editor = window.activeTextEditor;
		if (!editor || editor.document.languageId !== "wgsl") {
			return "";
		}

		let params: DebugAstParams = {
			textDocument: {
				uri: editor.document.uri.toString(),
			},
		};

		return ctx.get()?.client.sendRequest(ext.DEBUG_AST, params, onCancel) ?? "";
	}
}

function sleep(ms: number) {
	return new Promise<void>(resolve => {
		setTimeout(resolve, ms);
	});
}
