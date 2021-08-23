import { commands } from "vscode";

import ctx from "../context";

export * from "./debug-document";

interface CommandHandler {
	(...args: any[]): any;
}

namespace cmd {
	export function register(id: string, handler: CommandHandler) {
		ctx.get()?.subscriptions.push(commands.registerCommand(`wgsl.${id}`, handler));
	}
}

export default cmd;
