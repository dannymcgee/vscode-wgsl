import { TMGrammarScope } from "@vscode-devkit/grammar";

export const keyword: TMGrammarScope = {
	patterns: [
		{
			match: /\b(if|else|loop|continu(?:e|ing)|break|return)\b/,
			name: "keyword.control.$1.wgsl",
		},
		{
			match: /\b(struct|fn|var|let|uniform|storage)\b/,
			name: "storage.type.$1.wgsl",
		},
		{
			match: /\b([fiu](?:8|16|32|64))\b/,
			name: "support.type.primitive.wgsl",
		},
		{
			match: /\b(vec[234])\b/,
			name: "entity.name.type.struct.wgsl",
		},
		{
			match: /\b(mat[234]x[234])\b/,
			name: "entity.name.type.struct.wgsl",
		},
		{
			match: /\b(texture_[23]d)\b/,
			name: "entity.name.type.struct.wgsl",
		},
		{
			match: /\b(sampler)\b/,
			name: "entity.name.type.struct.wgsl",
		},
	],
};
