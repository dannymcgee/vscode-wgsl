import { commands } from "vscode";
import { ReferenceParams, ReferencesRequest } from "vscode-languageserver-protocol";

import ctx from "../context";

export async function resolveLensReferences(params: ReferenceParams) {
	try {
		let client = ctx.get()?.client;
		if (!client) return;

		let result = await client.sendRequest(ReferencesRequest.type, params);
		if (!result) return;

		let locations = result.map(loc => client.protocol2CodeConverter.asLocation(loc));
		let uri = client.protocol2CodeConverter.asUri(params.textDocument.uri);
		let startPos = locations[0].range.start;

		await commands.executeCommand(
			"editor.action.peekLocations",
			uri,
			startPos,
			locations,
			"peek",
			"No references found"
		);
	} catch (err) {
		console.error(err);
	}
}
