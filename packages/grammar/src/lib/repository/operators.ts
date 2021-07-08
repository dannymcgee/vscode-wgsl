import { TMGrammarScope } from "@vscode-devkit/grammar";

export const operator: TMGrammarScope = {
	patterns: [
		{
			name: "keyword.operator.wgsl",
			match: /->/,
		},
		{
			name: "keyword.operator.wgsl",
			match: /[-+*\/&|=<>]/,
		},
	],
};
