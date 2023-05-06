import { TMGrammarScope, regex } from "@vscode-devkit/grammar";
import { IDENT } from "./identifiers";

export const attribute: TMGrammarScope = {
	patterns: [
		{
			name: "meta.attribute.wgsl",
			begin: regex`/(@)(${IDENT})(\()/`,
			beginCaptures: {
				1: { name: "punctuation.definition.attribute.begin.wgsl" },
				2: { name: "support.annotation.wgsl" },
				3: { name: "punctuation.brace.round.wgsl" },
			},
			end: /\)/,
			endCaptures: {
				0: { name: "punctuation.brace.round.wgsl" },
			},
			patterns: [{ include: "#attributeParams" }],
		},
		{
			name: "meta.attribute.wgsl",
			match: regex`/(@)(${IDENT})/`,
			captures: {
				1: { name: "punctuation.definition.attribute.begin.wgsl" },
				2: { name: "support.annotation.wgsl" },
			},
		},
	],
};

export const attributeParams: TMGrammarScope = {
	patterns: [
		{
			name: "entity.other.attribute.wgsl",
			match: IDENT,
		},
		{ include: "#punctuation" },
		{ include: "#literal" },
	],
};
