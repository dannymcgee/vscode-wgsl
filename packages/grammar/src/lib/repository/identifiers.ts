import { TMGrammarScope, regex } from "@vscode-devkit/grammar";

export const IDENT = /[a-zA-Z][_a-zA-Z0-9]*/;

export const identifier: TMGrammarScope = {
	patterns: [
		{
			match: regex`/\b(${IDENT})\s*(?=\()/`,
			captures: {
				1: { name: "entity.name.function.wgsl" },
			},
		},
		{
			match: regex`/(?<=\.)(${IDENT})/`,
			captures: {
				1: { name: "variable.other.property.wgsl" },
			},
		},
		{
			match: /\b([A-Z][_a-zA-Z0-9]*)\b/,
			captures: {
				1: { name: "entity.name.type.struct.wgsl" },
			},
		},
		{
			match: /\b([a-z][_a-zA-Z0-9]*)\b/,
			captures: {
				1: { name: "variable.other.wgsl" },
			},
		},
	],
};
