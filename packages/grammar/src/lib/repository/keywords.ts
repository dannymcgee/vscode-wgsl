import { TMGrammarScope } from "@vscode-devkit/grammar";

export const keyword: TMGrammarScope = {
	patterns: [
		{
			match:
				/\b(break|case|continu(e|ing)|default|else|enable|fallthrough|for|if|import|loop|return|switch|from)\b/,
			name: "keyword.control.$1.wgsl",
		},
		{
			match: /\b(struct|fn|const|var|let|type)\b/,
			name: "storage.type.$1.wgsl",
		},
		{
			match: /\b(uniform|storage|export|read|write|read_write)\b/,
			name: "storage.modifier.$1.wgsl",
		},
		{
			match: /\b([fiu](?:8|16|32|64))\b/,
			name: "support.type.primitive.wgsl",
		},
		{
			match: /\b(vec[234])\b/,
			name: "entity.name.type.wgsl",
		},
		{
			match: /\b(mat[234]x[234])\b/,
			name: "entity.name.type.wgsl",
		},
		{
			match: /\b(texture_[23]d)\b/,
			name: "entity.name.type.wgsl",
		},
		{
			match: /\b(sampler|array)\b/,
			name: "entity.name.type.wgsl",
		},
		{
			match: /\b(true|false)\b/,
			name: "constant.language.boolean.wgsl",
		},
	],
};
