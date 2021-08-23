import { workspace } from "vscode";
import ctx from "../context";

export * from "./debug-document";

namespace providers {
	// eslint-disable-next-line @typescript-eslint/no-explicit-any
	export function register(id: string, ...args: any[]) {
		let registerProvider = workspace[`register${id}Provider`];
		if (typeof registerProvider !== "function") {
			throw new Error(`workspace.register${id}Provider is not a function!`);
		}

		ctx.get()?.subscriptions.push(registerProvider(...args));
	}
}

export default providers;
