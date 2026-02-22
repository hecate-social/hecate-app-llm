<script lang="ts">
	import { onMount } from 'svelte';
	import { fetchModels, setApi } from '$lib/stores/llm.js';
	import LlmStudio from '$lib/LlmStudio.svelte';

	// In dev mode, create a mock API that talks directly via fetch
	const devApi = {
		get: async <T>(path: string): Promise<T> => {
			const res = await fetch(`http://localhost:8080${path}`);
			return res.json();
		},
		post: async <T>(path: string, body: unknown): Promise<T> => {
			const res = await fetch(`http://localhost:8080${path}`, {
				method: 'POST',
				headers: { 'Content-Type': 'application/json' },
				body: JSON.stringify(body)
			});
			return res.json();
		},
		del: async <T>(path: string): Promise<T> => {
			const res = await fetch(`http://localhost:8080${path}`, { method: 'DELETE' });
			return res.json();
		}
	};
</script>

<div class="h-screen">
	<p class="text-center text-xs text-surface-500 py-2">
		Dev mode: LLM Studio standalone. Streaming requires Tauri webview.
	</p>
</div>
