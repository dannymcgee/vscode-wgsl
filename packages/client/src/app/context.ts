import { ExtensionContext } from "vscode";
import { LanguageClient } from "vscode-languageclient/node";

// prettier-ignore
class Context implements ExtensionContext {
	static instance?: Context;

	inner: ExtensionContext;
	client: LanguageClient;

	constructor(ctx: ExtensionContext, client: LanguageClient) {
		this.inner = ctx;
		this.client = client;

		Context.instance = this;
	}

	get subscriptions() { return this.inner.subscriptions }
	get workspaceState() { return this.inner.workspaceState }
	get globalState() { return this.inner.globalState }
	get extensionUri() { return this.inner.extensionUri }
	get extensionPath() { return this.inner.extensionPath }
	get environmentVariableCollection() { return this.inner.environmentVariableCollection }
	get storageUri() { return this.inner.storageUri }
	/** @deprecated */
	get storagePath() { return this.inner.storagePath }
	get globalStorageUri() { return this.inner.globalStorageUri }
	/** @deprecated */
	get globalStoragePath() { return this.inner.globalStoragePath }
	get logUri() { return this.inner.logUri }
	/** @deprecated */
	get logPath() { return this.inner.logPath }
	get extensionMode() { return this.inner.extensionMode }
	get secrets() { return this.inner.secrets }
	get extension() { return this.inner.extension }

	get asAbsolutePath() { return this.inner.asAbsolutePath }
}

namespace ctx {
	export function bootstrap(ctx: ExtensionContext, client: LanguageClient) {
		void new Context(ctx, client);
	}

	export function get() {
		return Context.instance;
	}
}

export default ctx;
