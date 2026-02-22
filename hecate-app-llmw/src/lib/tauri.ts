// Access Tauri APIs from plugin Web Component context.
// The component runs inside the Tauri webview, so window.__TAURI__ is available.

interface TauriGlobal {
	core: {
		invoke: <T>(cmd: string, args?: Record<string, unknown>) => Promise<T>;
	};
	event: {
		listen: <T>(event: string, handler: (event: { payload: T }) => void) => Promise<() => void>;
	};
}

function getTauri(): TauriGlobal {
	const w = window as unknown as { __TAURI__?: TauriGlobal };
	if (!w.__TAURI__) {
		throw new Error('Tauri API not available (not running inside Tauri webview)');
	}
	return w.__TAURI__;
}

export function invoke<T>(cmd: string, args?: Record<string, unknown>): Promise<T> {
	return getTauri().core.invoke<T>(cmd, args);
}

export function listen<T>(
	event: string,
	handler: (event: { payload: T }) => void
): Promise<() => void> {
	return getTauri().event.listen<T>(event, handler);
}
