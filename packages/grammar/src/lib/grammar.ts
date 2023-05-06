import { TMGrammar, regex } from "@vscode-devkit/grammar";

import {
	attribute,
	attributeParams,
	comment,
	IDENT,
	identifier,
	keyword,
	literal,
	operator,
	punctuation,
} from "./repository";

export const grammar: TMGrammar = {
	name: "wgsl",
	scopeName: "source.wgsl",
	patterns: [
		{
			name: "meta.struct.wgsl",
			begin: regex`/(struct)\s+(${IDENT})\s*(\{)/`,
			beginCaptures: {
				1: { name: "storage.type.struct.wgsl" },
				2: { name: "entity.name.type.struct.wgsl" },
				3: { name: "punctuation.brace.curly.wgsl" },
			},
			end: /\}/,
			endCaptures: {
				0: { name: "punctuation.brace.curly.wgsl" },
			},
			patterns: [
				{
					name: "variable.other.property.wgsl",
					match: regex`/${IDENT}\s*(?=:)/`,
				},
				{
					include: "source.wgsl",
				},
			],
		},
		{ include: "#comment" },
		{ include: "#keyword" },
		{ include: "#attribute" },
		{ include: "#operator" },
		{ include: "#literal" },
		{ include: "#punctuation" },
		{ include: "#identifier" },
	],
	repository: {
		attribute,
		attributeParams,
		comment,
		identifier,
		keyword,
		literal,
		operator,
		punctuation,
	},
};
