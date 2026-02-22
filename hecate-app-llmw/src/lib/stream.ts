import type { ChatMessage, StreamChunk } from './types.js';
import { invoke, listen } from './tauri.js';

export interface ChatStream {
	onChunk(fn: (chunk: StreamChunk) => void): ChatStream;
	onDone(fn: (chunk: StreamChunk) => void): ChatStream;
	onError(fn: (error: string) => void): ChatStream;
	start(): Promise<void>;
	cancel(): void;
}

export function createChatStream(
	model: string,
	messages: ChatMessage[],
	opts?: { temperature?: number; max_tokens?: number; tools?: unknown[] }
): ChatStream {
	const streamId = crypto.randomUUID();
	let chunkHandler: ((chunk: StreamChunk) => void) | null = null;
	let doneHandler: ((chunk: StreamChunk) => void) | null = null;
	let errorHandler: ((error: string) => void) | null = null;
	const unlisteners: (() => void)[] = [];
	let cancelled = false;

	const stream: ChatStream = {
		onChunk(fn) {
			chunkHandler = fn;
			return stream;
		},
		onDone(fn) {
			doneHandler = fn;
			return stream;
		},
		onError(fn) {
			errorHandler = fn;
			return stream;
		},
		async start() {
			if (cancelled) return;

			const unChunk = await listen<StreamChunk>(`chat-chunk-${streamId}`, (event) => {
				if (!cancelled && chunkHandler) chunkHandler(event.payload);
			});
			unlisteners.push(unChunk);

			const unDone = await listen<StreamChunk>(`chat-done-${streamId}`, (event) => {
				if (!cancelled && doneHandler) doneHandler(event.payload);
				cleanup();
			});
			unlisteners.push(unDone);

			const unError = await listen<StreamChunk>(`chat-error-${streamId}`, (event) => {
				if (!cancelled && errorHandler) {
					errorHandler(event.payload.error || 'Unknown streaming error');
				}
				cleanup();
			});
			unlisteners.push(unError);

			try {
				await invoke('chat_stream', {
					streamId,
					model,
					messages,
					temperature: opts?.temperature ?? null,
					maxTokens: opts?.max_tokens ?? null,
					tools: opts?.tools ?? null,
					plugin: 'llm'
				});
			} catch (e) {
				if (errorHandler) errorHandler(String(e));
				cleanup();
			}
		},
		cancel() {
			cancelled = true;
			cleanup();
		}
	};

	function cleanup() {
		for (const un of unlisteners) un();
		unlisteners.length = 0;
	}

	return stream;
}
