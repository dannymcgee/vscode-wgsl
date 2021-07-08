import { TMGrammarScope } from "@vscode-devkit/grammar";

export const punctuation: TMGrammarScope = {
	patterns: [
		{
			match: /[()]/,
			name: "punctuation.brace.round.wgsl",
		},
		{
			match: /[{}]/,
			name: "punctuation.brace.curly.wgsl",
		},
		{
			match: /[\[\]]/,
			name: "punctuation.brace.square.wgsl",
		},
		{
			match: /,/,
			name: "punctuation.separator.comma.wgsl",
		},
		{
			match: /:/,
			name: "punctuation.separator.key-value.wgsl",
		},
		{
			match: /\./,
			name: "punctuation.accessor.wgsl",
		},
		{
			match: /;/,
			name: "punctuation.terminator.wgsl",
		},
	],
};
