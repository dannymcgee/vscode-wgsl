export interface Configuration {
	preprocessor: Preprocessor;
}

export interface Preprocessor {
	includeKeyword: string;
	includePaths: string[];
	includeModuleAliases: ModuleAlias[];
}

export interface ModuleAlias {
	name: string;
	path: string;
}
