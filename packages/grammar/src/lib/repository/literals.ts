import { TMGrammarScope } from "@vscode-devkit/grammar";

export const literal: TMGrammarScope = {
	patterns: [
		{
			name: "string.wgsl",
			begin: /"/,
			beginCaptures: {
				0: { name: "punctuation.definition.string.begin.wgsl" },
			},
			end: /(?<!\\)(")|\\\\(")/,
			endCaptures: {
				1: { name: "punctuation.definition.string.end.wgsl" },
				2: { name: "punctuation.definition.string.end.wgsl" },
			},
		},
		{
			name: "constant.numeric.wgsl",
			match: /[0-9]+[ui]?/,
		},
	],
};
