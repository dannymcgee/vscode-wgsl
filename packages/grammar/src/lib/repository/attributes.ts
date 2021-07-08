import { TMGrammarScope } from "@vscode-devkit/grammar";
import { IDENT } from "./identifiers";

export const attribute: TMGrammarScope = {
	patterns: [
		{
			name: "meta.attribute.wgsl",
			begin: /\[\[/,
			beginCaptures: {
				0: { name: "punctuation.definition.attribute.begin.wgsl" },
			},
			end: /\]\]/,
			endCaptures: {
				0: { name: "punctuation.definition.attribute.end.wgsl" },
			},
			patterns: [{ include: "#attributeBody" }],
		},
	],
};

export const attributeBody: TMGrammarScope = {
	patterns: [
		{
			name: "support.annotation.wgsl",
			match: IDENT,
		},
		{ include: "#punctuation" },
		{ include: "#literal" },
	],
};
