import { ExtensionContext } from "vscode";

import ctx from "./context";
import client from "./client";
import providers, { DebugAstDocument } from "./providers";
import cmd, { debugAst } from "./commands";

export function activate(context: ExtensionContext) {
	console.log("WGSL Language Support activated");

	ctx.bootstrap(context, client.create());
	providers.register("TextDocumentContent", "wgsl", new DebugAstDocument());
	cmd.register("debugAst", debugAst);
}

export function deactivate() {
	return ctx.get()?.client.stop();
}
