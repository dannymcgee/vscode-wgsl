import { TMGrammarScope } from "@vscode-devkit/grammar";

export const keyword: TMGrammarScope = {
	patterns: [
		{
			match:
				/\b(if|else|elseif|loop|continu(?:e|ing)|switch|case|break|fallthrough|return|enable|import|from)\b/,
			name: "keyword.control.$1.wgsl",
		},
		{
			match: /\b(struct|fn|var|let|uniform|storage|type|export)\b/,
			name: "storage.type.$1.wgsl",
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
			match: /\b(sampler)\b/,
			name: "entity.name.type.wgsl",
		},
	],
};
