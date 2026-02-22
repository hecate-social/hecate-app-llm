export type Role = 'system' | 'user' | 'assistant' | 'tool';

export interface ChatMessage {
	role: Role;
	content: string;
	tool_calls?: ToolCall[];
	tool_call_id?: string;
}

export interface ToolCall {
	id: string;
	name: string;
	arguments: Record<string, unknown>;
}

export interface ToolSchema {
	name: string;
	description: string;
	input_schema: Record<string, unknown>;
}

export interface StreamChunk {
	content: string;
	done: boolean;
	model?: string;
	usage?: Usage;
	tool_use?: ToolCall;
	error?: string;
}

export interface Usage {
	prompt_tokens?: number;
	completion_tokens?: number;
}

export interface Model {
	name: string;
	provider: string;
	size?: string;
	size_bytes?: number;
	parameter_size?: string;
	family?: string;
	format?: string;
	context_length?: number;
	quantization_level?: string;
}

export interface ModelsResponse {
	ok: boolean;
	models: Model[];
}

export interface Provider {
	name: string;
	type: string;
	url?: string;
	enabled: boolean;
}

export interface LLMHealth {
	ok: boolean;
	status: 'healthy' | 'unhealthy';
	providers: Record<string, string>;
}
