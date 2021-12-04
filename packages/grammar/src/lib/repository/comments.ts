import { TMGrammarScope } from "@vscode-devkit/grammar";

export const comment: TMGrammarScope = {
	patterns: [
		{
			begin: /\/\//,
			beginCaptures: {
				0: { name: "punctuation.definition.comment.line.wgsl" },
			},
			end: /(?=$)/,
			name: "comment.line.wgsl",
		},
		{
			begin: /#/,
			end: /(?=$)/,
			name: "preprocessor.directive.wgsl",
		}
	],
};
