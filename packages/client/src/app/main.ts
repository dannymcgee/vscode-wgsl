import { ExtensionContext } from "vscode";

import ctx from "./context";
import client from "./client";
import cmd, { debugAst, debugTokens } from "./commands";
import providers, { DebugAstProvider, DebugTokensProvider } from "./providers";

// prettier-ignore
export function activate(context: ExtensionContext) {
	console.log("WGSL Language Support activated");

	ctx.bootstrap(context, client.create());

	providers.register("TextDocumentContent", "wgsl-ast", new DebugAstProvider());
	providers.register("TextDocumentContent", "wgsl-tokens", new DebugTokensProvider());

	cmd.register("debugAst", debugAst);
	cmd.register("debugTokens", debugTokens);
}

export function deactivate() {
	return ctx.get()?.client.stop();
}
