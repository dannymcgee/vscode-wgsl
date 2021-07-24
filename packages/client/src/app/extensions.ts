import { NotificationType } from "vscode-languageclient/node";

export interface UnreadDependencyParams {
	dependency: string;
	dependant: string;
}

// prettier-ignore
export const unreadDependency =
	new NotificationType<UnreadDependencyParams>("wgsl/unreadDependency");
