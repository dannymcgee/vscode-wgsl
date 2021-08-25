import { languages } from "vscode";
import ctx from "../context";
import { Ctor } from "../util";

export * from "./references";

namespace providers {
	export function register(id: string, Provider: Ctor<any>) {
		let registerProvider = languages[`register${id}Provider`];
		let selector = { language: "wgsl", scheme: "file" };

		if (typeof registerProvider !== "function") {
			throw new Error(`languages.register${id}Provider is not a function!`);
		}

		ctx.get()?.subscriptions.push(registerProvider(selector, new Provider()));
	}
}

export default providers;
