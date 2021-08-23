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
import ext, { DebugDocumentParams } from "../extensions";
import { sleep } from "../util";

export abstract class DebugDocumentProvider implements TextDocumentContentProvider {
	readonly uri: Uri;
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

		let params: DebugDocumentParams = {
			textDocument: {
				uri: editor.document.uri.toString(),
			},
		};

		return this.sendRequest(params, onCancel);
	}

	abstract sendRequest(
		params: DebugDocumentParams,
		onCancel: CancellationToken
	): Promise<string>;
}

export class DebugAstProvider extends DebugDocumentProvider {
	readonly uri = Uri.parse("wgsl-ast://debug/syntax-tree.wgsl-ast");

	async sendRequest(params: DebugDocumentParams, onCancel: CancellationToken) {
		return ctx.get()?.client.sendRequest(ext.DEBUG_AST, params, onCancel) ?? "";
	}
}

export class DebugTokensProvider extends DebugDocumentProvider {
	readonly uri = Uri.parse("wgsl-tokens://debug/document-tokens.wgsl-ast");

	async sendRequest(params: DebugDocumentParams, onCancel: CancellationToken) {
		return ctx.get()?.client.sendRequest(ext.DEBUG_TOKENS, params, onCancel) ?? "";
	}
}
