import { TextDocumentContentProvider, workspace } from "vscode";

import ctx from "../context";
import { Ctor } from "../util";

export * from "./debug-document";

namespace contentProviders {
	export function register(
		scheme: string,
		Provider: Ctor<TextDocumentContentProvider>
	) {
		let provider = new Provider();

		// prettier-ignore
		ctx.get()?.subscriptions.push(
			workspace.registerTextDocumentContentProvider(scheme, provider)
		);
	}
}

export default contentProviders;
