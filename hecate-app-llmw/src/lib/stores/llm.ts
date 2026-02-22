import { writable, get } from 'svelte/store';
import type { ChatMessage, Model, StreamChunk, Usage } from '../types.js';
import type { ChatStream } from '../stream.js';
import { createChatStream } from '../stream.js';

export interface PluginApi {
	get: <T>(path: string) => Promise<T>;
	post: <T>(path: string, body: unknown) => Promise<T>;
	del: <T>(path: string) => Promise<T>;
}

let pluginApi: PluginApi | null = null;

export function setApi(api: PluginApi) {
	pluginApi = api;
}

export const models = writable<Model[]>([]);
export const selectedModel = writable<string>('');
export const messages = writable<ChatMessage[]>([]);
export const isStreaming = writable(false);
export const streamingContent = writable('');
export const lastUsage = writable<Usage | null>(null);
export const streamError = writable<string | null>(null);

let activeStream: ChatStream | null = null;

let prevModel: string | null = null;
selectedModel.subscribe((model) => {
	if (prevModel !== null && prevModel !== model) {
		if (activeStream) {
			activeStream.cancel();
			activeStream = null;
		}
		messages.set([]);
		streamingContent.set('');
		isStreaming.set(false);
		lastUsage.set(null);
		streamError.set(null);
	}
	prevModel = model;
});

const PREFERRED_CLOUD_MODEL = 'llama-3.3-70b-versatile';
const MIN_LOCAL_PARAMS_B = 20;

function parseParamBillions(s?: string): number {
	if (!s) return 0;
	const m = s.match(/^(\d+(?:\.\d+)?)\s*[Bb]/);
	return m ? parseFloat(m[1]) : 0;
}

function selectBestModel(available: Model[]): string {
	const local = available.filter((m) => m.provider === 'ollama');
	const capable = local.filter((m) => parseParamBillions(m.parameter_size) >= MIN_LOCAL_PARAMS_B);
	if (capable.length > 0) {
		const best = [...capable].sort((a, b) => (b.size_bytes ?? 0) - (a.size_bytes ?? 0));
		return best[0].name;
	}
	const cloud = available.find((m) => m.name === PREFERRED_CLOUD_MODEL);
	if (cloud) return cloud.name;
	if (local.length > 0) return local[0].name;
	return available[0]?.name ?? '';
}

let fetchRetries = 0;

export async function fetchModels(): Promise<void> {
	if (!pluginApi) return;
	try {
		const resp = await pluginApi.get<{ ok: boolean; models: Model[] }>('/api/llm/models');
		if (resp.ok && resp.models) {
			models.set(resp.models);
			const best = selectBestModel(resp.models);
			if (best) selectedModel.set(best);

			const hasPreferred = resp.models.some((m) => m.name === PREFERRED_CLOUD_MODEL);
			if (!hasPreferred && fetchRetries < 5) {
				fetchRetries++;
				setTimeout(() => fetchModels(), 3000);
			}
		}
	} catch {
		models.set([]);
	}
}

export function clearChat(): void {
	messages.set([]);
	streamingContent.set('');
	lastUsage.set(null);
	streamError.set(null);
}

export async function sendMessage(content: string): Promise<void> {
	const model = get(selectedModel);
	if (!model || !content.trim()) return;

	const userMsg: ChatMessage = { role: 'user', content: content.trim() };
	messages.update((msgs) => [...msgs, userMsg]);

	const allMessages: ChatMessage[] = [...get(messages)];

	isStreaming.set(true);
	streamingContent.set('');
	streamError.set(null);
	lastUsage.set(null);

	let accumulated = '';
	let receivedAnyEvent = false;

	const stream = createChatStream(model, allMessages);
	activeStream = stream;

	const timeout = setTimeout(() => {
		if (get(isStreaming) && !receivedAnyEvent) {
			stream.cancel();
			finishWithError('Stream timeout -- no response from daemon. Check daemon logs.');
		}
	}, 45000);

	function finishWithError(error: string) {
		clearTimeout(timeout);
		activeStream = null;
		const errorMsg: ChatMessage = { role: 'assistant', content: `Error: ${error}` };
		messages.update((msgs) => [...msgs, errorMsg]);
		streamingContent.set('');
		streamError.set(error);
		isStreaming.set(false);
	}

	stream
		.onChunk((chunk: StreamChunk) => {
			receivedAnyEvent = true;
			if (chunk.content) {
				accumulated += chunk.content;
				streamingContent.set(accumulated);
			}
		})
		.onDone((chunk: StreamChunk) => {
			receivedAnyEvent = true;
			clearTimeout(timeout);
			activeStream = null;
			if (chunk.content) accumulated += chunk.content;
			const assistantMsg: ChatMessage = {
				role: 'assistant',
				content: accumulated || '(empty response)'
			};
			messages.update((msgs) => [...msgs, assistantMsg]);
			streamingContent.set('');
			isStreaming.set(false);
			if (chunk.usage) lastUsage.set(chunk.usage);
		})
		.onError((error: string) => {
			receivedAnyEvent = true;
			clearTimeout(timeout);
			finishWithError(error);
		});

	try {
		await stream.start();
	} catch (e) {
		clearTimeout(timeout);
		finishWithError(String(e));
	}
}
