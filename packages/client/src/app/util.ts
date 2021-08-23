export interface Ctor<T, Args extends any[] = []> {
	new (...args: Args): T;
}

export function sleep(ms: number) {
	return new Promise<void>(resolve => {
		setTimeout(resolve, ms);
	});
}
