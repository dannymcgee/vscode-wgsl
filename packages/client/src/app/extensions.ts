import {
	NotificationType,
	RequestType,
	TextDocumentIdentifier,
} from "vscode-languageclient/node";

export interface UnreadDependencyParams {
	dependency: string;
	dependant: string;
}

export interface DebugAstParams {
	textDocument: TextDocumentIdentifier;
}

// prettier-ignore
namespace ext {
	/* eslint-disable prefer-let/prefer-let */
	export const UNREAD_DEPENDENCY =
		new NotificationType<UnreadDependencyParams>("wgsl/unreadDependency");
	export const DEBUG_AST =
		new RequestType<DebugAstParams, string, void>("wgsl/debugAst");
}

export default ext;
