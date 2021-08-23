import {
	NotificationType,
	RequestType,
	TextDocumentIdentifier,
} from "vscode-languageclient/node";

export interface UnreadDependencyParams {
	dependency: string;
	dependant: string;
}

export interface DebugDocumentParams {
	textDocument: TextDocumentIdentifier;
}

// prettier-ignore
namespace ext {
	/* eslint-disable prefer-let/prefer-let */
	export const UNREAD_DEPENDENCY =
		new NotificationType<UnreadDependencyParams>("wgsl/unreadDependency");
	export const DEBUG_AST =
		new RequestType<DebugDocumentParams, string, void>("wgsl/debugAst");
	export const DEBUG_TOKENS =
		new RequestType<DebugDocumentParams, string, void>("wgsl/debugTokens");
}

export default ext;
