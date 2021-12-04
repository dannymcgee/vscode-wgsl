import * as path from "path";
import { Uri, workspace } from "vscode";
import {
	DidChangeConfigurationNotification,
	LanguageClient,
	ProtocolRequestType,
	TransportKind,
} from "vscode-languageclient/node";

import { capabilities } from "./capabilities";
import { Configuration } from "./configuration";
import ctx from "./context";
import ext from "./extensions";

namespace client {
	export function create() {
		let exePath = path.join(__dirname, "./server.exe");
		let exe = { command: exePath };

		// prettier-ignore
		let client = new LanguageClient("WGSL Language Server", {
			transport: TransportKind.stdio,
			run: exe,
			debug: exe,
		}, {
			documentSelector: [{ scheme: "file", language: "wgsl" }],
			synchronize: {
				fileEvents: workspace.createFileSystemWatcher("**/*.wgsl"),
			}
		});

		client.start();
		client.onReady().then(() => {
			client.onNotification(ext.UNREAD_DEPENDENCY, async params => {
				let dependency = Uri.parse(params.dependency);
				// TODO: report error if path is invalid
				await workspace.openTextDocument(dependency);
			});

			ctx.get()?.subscriptions.push(
				workspace.onDidChangeConfiguration(change => {
					if (change.affectsConfiguration("wgsl")) {
						client.sendNotification(DidChangeConfigurationNotification.type, {
							settings: workspace.getConfiguration().get<Configuration>("wgsl"),
						});
					}
				})
			);

			client.sendRequest(new ProtocolRequestType("initialize"), {
				capabilities,
			});
		});

		return client;
	}
}

export default client;
