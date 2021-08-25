import { ExtensionContext } from "vscode";

import ctx from "./context";
import client from "./client";
import cmd, { debugAst, debugTokens, resolveLensReferences } from "./commands";
import contentProviders, {
	DebugAstProvider,
	DebugTokensProvider,
} from "./contentProviders";

// prettier-ignore
export function activate(context: ExtensionContext) {
	console.log("WGSL Language Support activated");

	ctx.bootstrap(context, client.create());

	contentProviders.register("wgsl-ast", DebugAstProvider);
	contentProviders.register("wgsl-tokens", DebugTokensProvider);

	cmd.register("debugAst", debugAst);
	cmd.register("debugTokens", debugTokens);
	cmd.register("resolveLensReferences", resolveLensReferences);
}

export function deactivate() {
	return ctx.get()?.client.stop();
}
