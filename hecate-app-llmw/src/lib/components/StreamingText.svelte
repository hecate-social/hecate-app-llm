<script lang="ts">
	let { content }: { content: string } = $props();

	const isInThink = $derived(
		content.includes('<think>') && !content.includes('</think>')
	);
	const hasCompletedThink = $derived(content.includes('</think>'));

	const visibleContent = $derived.by(() => {
		if (!hasCompletedThink) {
			if (isInThink) return '';
			return content;
		}
		const afterThink = content.split('</think>').pop() || '';
		return afterThink.trim();
	});

	const thinkContent = $derived.by(() => {
		const openIdx = content.indexOf('<think>');
		if (openIdx === -1) return '';
		const closeIdx = content.indexOf('</think>');
		if (closeIdx === -1) return content.slice(openIdx + 7);
		return content.slice(openIdx + 7, closeIdx);
	});
</script>

<div class="flex justify-start mb-3">
	<div
		class="max-w-[80%] rounded-lg px-4 py-2.5 text-sm leading-relaxed
			bg-surface-700 border border-hecate-500/30 text-surface-100"
	>
		<div class="text-[10px] text-hecate-400 mb-1 flex items-center gap-1">
			<span class="animate-pulse">{'\u{25CF}'}</span>
			Assistant
		</div>

		{#if isInThink}
			<div class="flex items-center gap-2 text-surface-400 mb-1">
				<span class="flex gap-1">
					<span class="w-1.5 h-1.5 rounded-full bg-accent-500/60 animate-bounce" style="animation-delay: 0ms"></span>
					<span class="w-1.5 h-1.5 rounded-full bg-accent-500/60 animate-bounce" style="animation-delay: 150ms"></span>
					<span class="w-1.5 h-1.5 rounded-full bg-accent-500/60 animate-bounce" style="animation-delay: 300ms"></span>
				</span>
				<span class="text-xs text-accent-400/70">Reasoning...</span>
			</div>
			{#if thinkContent.trim()}
				<details class="group">
					<summary class="text-[11px] text-surface-500 cursor-pointer hover:text-surface-400
						select-none flex items-center gap-1">
						<span class="text-[10px] transition-transform group-open:rotate-90">{'\u{25B6}'}</span>
						Show reasoning
					</summary>
					<div class="mt-1 pl-3 border-l-2 border-surface-600 text-xs text-surface-400
						whitespace-pre-wrap break-words leading-relaxed max-h-40 overflow-y-auto">
						{thinkContent.trim()}<span class="inline-block w-1 h-3 bg-accent-400/50 animate-pulse ml-0.5 align-text-bottom"></span>
					</div>
				</details>
			{/if}
		{:else if visibleContent}
			{#if hasCompletedThink && thinkContent.trim()}
				<details class="mb-2 group">
					<summary class="text-[11px] text-surface-500 cursor-pointer hover:text-surface-400
						select-none flex items-center gap-1">
						<span class="text-[10px] transition-transform group-open:rotate-90">{'\u{25B6}'}</span>
						Reasoning
					</summary>
					<div class="mt-1 pl-3 border-l-2 border-surface-600 text-xs text-surface-400
						whitespace-pre-wrap break-words leading-relaxed max-h-40 overflow-y-auto">
						{thinkContent.trim()}
					</div>
				</details>
			{/if}
			<div class="whitespace-pre-wrap break-words">{visibleContent}<span
					class="inline-block w-1.5 h-4 bg-hecate-400 animate-pulse ml-0.5 align-text-bottom"
				></span></div>
		{:else}
			<div class="flex items-center gap-2 text-surface-400">
				<span class="flex gap-1">
					<span class="w-1.5 h-1.5 rounded-full bg-hecate-500 animate-bounce" style="animation-delay: 0ms"></span>
					<span class="w-1.5 h-1.5 rounded-full bg-hecate-500 animate-bounce" style="animation-delay: 150ms"></span>
					<span class="w-1.5 h-1.5 rounded-full bg-hecate-500 animate-bounce" style="animation-delay: 300ms"></span>
				</span>
				<span class="text-xs">Thinking...</span>
			</div>
		{/if}
	</div>
</div>
