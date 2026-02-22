<script lang="ts">
	import type { ChatMessage as ChatMsg } from '../types.js';

	let { message }: { message: ChatMsg } = $props();

	const isUser = $derived(message.role === 'user');
	const isError = $derived(message.role === 'assistant' && message.content.startsWith('Error:'));

	interface ContentPart {
		type: 'text' | 'think';
		content: string;
	}

	const parts = $derived.by((): ContentPart[] => {
		if (isUser || isError) return [{ type: 'text', content: message.content }];

		const result: ContentPart[] = [];
		let remaining = message.content;

		while (remaining.length > 0) {
			const openIdx = remaining.indexOf('<think>');
			if (openIdx === -1) {
				if (remaining.trim()) result.push({ type: 'text', content: remaining });
				break;
			}
			if (openIdx > 0) {
				const before = remaining.slice(0, openIdx);
				if (before.trim()) result.push({ type: 'text', content: before });
			}
			const closeIdx = remaining.indexOf('</think>', openIdx);
			if (closeIdx === -1) {
				const thinkContent = remaining.slice(openIdx + 7);
				if (thinkContent.trim()) result.push({ type: 'think', content: thinkContent });
				break;
			}
			const thinkContent = remaining.slice(openIdx + 7, closeIdx);
			if (thinkContent.trim()) result.push({ type: 'think', content: thinkContent });
			remaining = remaining.slice(closeIdx + 8);
		}

		return result.length > 0 ? result : [{ type: 'text', content: message.content }];
	});
</script>

<div class="flex {isUser ? 'justify-end' : 'justify-start'} mb-3">
	<div
		class="max-w-[80%] rounded-lg px-4 py-2.5 text-sm leading-relaxed
			{isUser
			? 'bg-hecate-600/20 border border-hecate-500/30 text-surface-100'
			: isError
				? 'bg-danger-500/10 border border-danger-500/30 text-danger-400'
				: 'bg-surface-700 border border-surface-600 text-surface-100'}"
	>
		<div class="text-[10px] text-surface-400 mb-1">
			{isUser ? 'You' : 'Assistant'}
		</div>

		{#each parts as part}
			{#if part.type === 'think'}
				<details class="mb-2 group">
					<summary class="text-[11px] text-surface-400 cursor-pointer hover:text-surface-300
						select-none flex items-center gap-1">
						<span class="text-[10px] transition-transform group-open:rotate-90">{'\u{25B6}'}</span>
						Reasoning
					</summary>
					<div class="mt-1 pl-3 border-l-2 border-surface-600 text-xs text-surface-400
						whitespace-pre-wrap break-words leading-relaxed">
						{part.content.trim()}
					</div>
				</details>
			{:else}
				<div class="whitespace-pre-wrap break-words">{part.content.trim()}</div>
			{/if}
		{/each}
	</div>
</div>
