import { TMGrammar } from "@vscode-devkit/grammar";

import repository from "./repository";

export const grammar: TMGrammar = {
	name: "wgsl-ast",
	scopeName: "source.wgsl-ast",
	// prettier-ignore
	patterns: [
		{ include: "#nodeKind" },
		{ include: "#node" },
	],
	repository,
};
