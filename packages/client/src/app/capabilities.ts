import {
	CodeLensClientCapabilities,
	DidChangeConfigurationClientCapabilities,
} from "vscode-languageserver-protocol";

const codeLens: CodeLensClientCapabilities = {
	dynamicRegistration: true,
};

const didChangeConfiguration: DidChangeConfigurationClientCapabilities = {
	dynamicRegistration: true,
};

export const capabilities = {
	"textDocument.codeLens": codeLens,
	"workspace.configuration": true,
	"workspace.didChangeConfiguration": didChangeConfiguration,
};
