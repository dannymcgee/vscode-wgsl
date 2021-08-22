import { commands } from "vscode";

import ctx from "../context";
export * from "./debug-ast";

interface CommandHandler {
	// eslint-disable-next-line @typescript-eslint/no-explicit-any
	(...args: any[]): any;
}

namespace cmd {
	export function register(id: string, handler: CommandHandler) {
		ctx.get()?.subscriptions.push(commands.registerCommand(`wgsl.${id}`, handler));
	}
}

export default cmd;
