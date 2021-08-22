import { regex, TMGrammarScope } from "@vscode-devkit/grammar";

const IDENT = /[a-zA-Z][a-zA-Z0-9_]*/;
const PASCAL = /[A-Z][a-zA-Z0-9]*/;

const node: TMGrammarScope = {
	name: "meta.syntax-node.wgsl-ast",
	begin: regex`/(\()(${PASCAL})/`,
	beginCaptures: {
		1: { name: "punctuation.definition.syntax-node.begin.wgsl-ast" },
		2: { name: "entity.name.type.struct.wgsl-ast" },
	},
	end: /\)/,
	endCaptures: {
		0: { name: "punctuation.definition.syntax-node.end.wgsl-ast" },
	},
	patterns: [{ include: "#nodeField" }],
};

const nodeKind: TMGrammarScope = {
	name: "meta.syntax-node.kinded.wgsl-ast",
	begin: regex`/(\()(${PASCAL})(::)(${PASCAL})/`,
	beginCaptures: {
		1: { name: "punctuation.definition.syntax-node.begin.wgsl-ast" },
		2: { name: "entity.name.type.enum.wgsl-ast" },
		3: { name: "punctuation.accessor.enummember.wgsl-ast" },
		4: { name: "variable.other.enummember.wgsl-ast" },
	},
	end: /\)/,
	endCaptures: {
		0: { name: "punctuation.definition.syntax-node-end.wgsl-ast" },
	},
	// prettier-ignore
	patterns: [
		{ include: "#nodeKind" },
		{ include: "#node" },
		{ include: "#token" },
		{ include: "#punctuation" },
	],
};

const nodeField: TMGrammarScope = {
	name: "meta.syntax-node.child.wgsl-ast",
	begin: regex`/(${IDENT})(:)/`,
	beginCaptures: {
		1: { name: "variable.other.property.wgsl-ast" },
		2: { name: "punctuation.separator.key-value.wgsl-ast" },
	},
	end: /,/,
	endCaptures: {
		0: { name: "punctuation.separator.comma.wgsl-ast" },
	},
	patterns: [
		{ include: "#nodeKind" },
		{ include: "#node" },
		{ include: "#token" },
		{ include: "#array" },
		{ include: "#punctuation" },
	],
};

const LEXEME = /(`)(.+)(`)/;

const token: TMGrammarScope = {
	name: "meta.token.wgsl-ast",
	begin: regex`/${LEXEME} (\()(${PASCAL})/`,
	beginCaptures: {
		1: { name: "punctuation.definition.string.begin.wgsl-ast" },
		2: { name: "meta.token.lexeme.wgsl-ast string.wgsl-ast" },
		3: { name: "punctuation.definition.string.end.wgsl-ast" },
		4: { name: "punctuation.brace.round.wgsl-ast" },
		5: { name: "meta.token.kind.wgsl-ast keyword.other.token-kind.wgsl-ast" },
	},
	end: /\)/,
	endCaptures: {
		0: { name: "punctuation.brace.round.wgsl-ast" },
	},
	// prettier-ignore
	patterns: [
		{ include: "#span" },
		{ include: "#punctuation" },
	],
};

const span: TMGrammarScope = {
	name: "meta.token.span.wgsl-ast",
	begin: /\((?=[0-9])/,
	beginCaptures: {
		0: { name: "punctuation.brace.round.wgsl-ast" },
	},
	end: /\)/,
	endCaptures: {
		0: { name: "punctuation.brace.round.wgsl-ast" },
	},
	// prettier-ignore
	patterns: [
		{ include: "#numbers" },
		{ include: "#punctuation" },
	],
};

const array: TMGrammarScope = {
	name: "meta.array.wgsl-ast",
	begin: /\[/,
	beginCaptures: {
		0: { name: "punctuation.definition.array.begin.wgsl-ast" },
	},
	end: /\]/,
	endCaptures: {
		0: { name: "punctuation.definition.array.end.wgsl-ast" },
	},
	patterns: [
		{ include: "#nodeKind" },
		{ include: "#node" },
		{ include: "#token" },
		{ include: "#punctuation" },
	],
};

const punctuation: TMGrammarScope = {
	patterns: [
		{
			name: "punctuation.brace.round.wgsl-ast",
			match: /[()]/,
		},
		{
			name: "punctuation.brace.square.wgsl-ast",
			// eslint-disable-next-line no-useless-escape
			match: /[\[\]]/,
		},
		{
			name: "punctuation.separator.comma.wgsl-ast",
			match: /,/,
		},
		{
			name: "punctuation.accessor.enum-variant.wgsl-ast",
			match: /::/,
		},
		{
			name: "punctuation.separator.key-value.wgsl-ast",
			match: /:/,
		},
		{
			name: "keyword.operator.range.wgsl-ast",
			match: /\.\.\./,
		},
	],
};

const numbers: TMGrammarScope = {
	name: "constant.numeric.wgsl-ast",
	match: /[0-9]+/,
};

export default {
	array,
	node,
	nodeKind,
	nodeField,
	numbers,
	punctuation,
	span,
	token,
};
