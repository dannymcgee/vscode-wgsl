import { ExtensionContext, Uri, workspace } from "vscode";
import {
	LanguageClient,
	ProtocolRequestType,
	TransportKind,
} from "vscode-languageclient/node";
import * as path from "path";

import { unreadDependency } from "./extensions";

let client: LanguageClient;

export function activate(_: ExtensionContext) {
	console.log("WGSL Language Support activated");

	let exePath = path.join(__dirname, "./server.exe");
	console.log("exe path:", exePath);

	let exe = { command: exePath };
	// prettier-ignore
	client = new LanguageClient("WGSL Language Support", {
		transport: TransportKind.stdio,
		run: exe,
		debug: exe,
	}, {
		documentSelector: [{ scheme: "file", language: "wgsl" }],
		synchronize: {
			fileEvents: workspace.createFileSystemWatcher("**/*.wgsl"),
		},
	});

	client.start();

	client.onReady().then(() => {
		client.onNotification(unreadDependency, async params => {
			let dependency = Uri.parse(params.dependency);
			// TODO: report error if path is invalid
			await workspace.openTextDocument(dependency);
		});

		client.sendRequest(new ProtocolRequestType("initialize"), {});
	});
}

export function deactivate() {
	if (!client) return;

	return client.stop();
}
