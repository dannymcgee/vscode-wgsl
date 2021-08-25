import { CodeLensClientCapabilities } from "vscode-languageserver-protocol";

const codeLens: CodeLensClientCapabilities = {
	dynamicRegistration: true,
};

export const capabilities = {
	"textDocument.codeLens": codeLens,
};
