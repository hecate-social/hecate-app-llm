<svelte:options customElement={{ tag: "llm-studio", shadow: "none" }} />

<script lang="ts">
	import { onMount } from 'svelte';
	import type { Model } from './types.js';
	import ChatView from './components/ChatView.svelte';
	import ChatMessageBubble from './components/ChatMessage.svelte';
	import StreamingText from './components/StreamingText.svelte';
	import {
		models,
		selectedModel,
		fetchModels,
		setApi
	} from './stores/llm.js';

	interface PluginApi {
		get: <T>(path: string) => Promise<T>;
		post: <T>(path: string, body: unknown) => Promise<T>;
		del: <T>(path: string) => Promise<T>;
	}

	let { api }: { api: PluginApi } = $props();

	let activeApp: string | null = $state(null);
	let searchText: string = $state('');
	let selectedProvider: string | null = $state(null);
	let selectedSpecialty: string | null = $state(null);

	const SPECIALTIES = ['General Purpose', 'Coding', 'Research'] as const;

	const CODING_PATTERNS = /code|coder|codestral|starcoder/i;
	const RESEARCH_PATTERNS = /thinking|think|deepthink|\br1\b|\bo1\b|\bo3\b/i;

	function specialtyOf(m: Model): string {
		if (CODING_PATTERNS.test(m.name) || CODING_PATTERNS.test(m.family ?? '')) return 'Coding';
		if (RESEARCH_PATTERNS.test(m.name) || RESEARCH_PATTERNS.test(m.family ?? '')) return 'Research';
		return 'General Purpose';
	}

	function costOf(m: Model): string {
		if (m.provider === 'ollama') return '\u20AC';
		if (m.provider === 'groq') return '\u20AC';
		if (m.provider === 'google') {
			if (/flash/i.test(m.name)) return '\u20AC';
			return '\u20AC\u20AC';
		}
		if (m.provider === 'anthropic') {
			if (/haiku/i.test(m.name)) return '\u20AC\u20AC';
			return '\u20AC\u20AC\u20AC';
		}
		return '\u20AC\u20AC';
	}

	const providers = $derived(
		[...new Set($models.map((m) => m.provider))].sort()
	);

	const filteredModels = $derived(
		$models.filter((m) => {
			const matchesSearch = !searchText || m.name.toLowerCase().includes(searchText.toLowerCase());
			const matchesProvider = !selectedProvider || m.provider === selectedProvider;
			const matchesSpecialty = !selectedSpecialty || specialtyOf(m) === selectedSpecialty;
			return matchesSearch && matchesProvider && matchesSpecialty;
		})
	);

	const hasActiveFilters = $derived(!!searchText || !!selectedProvider || !!selectedSpecialty);

	onMount(() => {
		setApi(api);
		fetchModels();
	});

	function openChat(modelName: string) {
		$selectedModel = modelName;
		activeApp = 'chat';
	}

	function toggleProvider(p: string) {
		selectedProvider = selectedProvider === p ? null : p;
	}

	function toggleSpecialty(s: string) {
		selectedSpecialty = selectedSpecialty === s ? null : s;
	}

	function clearFilters() {
		searchText = '';
		selectedProvider = null;
		selectedSpecialty = null;
	}
</script>

{#if activeApp === 'chat'}
	<ChatView onBack={() => activeApp = null} />
{:else}
	<div class="flex flex-col items-center h-full overflow-y-auto py-8 px-6 gap-6">
		<div class="flex flex-col items-center gap-2">
			<h2
				class="text-xl font-bold tracking-wide"
				style="background: linear-gradient(135deg, #fbbf24, #a875ff); -webkit-background-clip: text; -webkit-text-fill-color: transparent; background-clip: text;"
			>
				LLM Studio
			</h2>
			<p class="text-surface-400 text-xs text-center">
				Chat with AI models across providers
			</p>
		</div>

		{#if $models.length === 0}
			<div class="text-center py-8">
				<div class="text-2xl mb-2 text-surface-500">&#9670;</div>
				<div class="text-xs text-surface-400">No models available</div>
				<div class="text-[10px] text-surface-500 mt-1">Check daemon connection</div>
			</div>
		{:else}
			<div class="flex flex-col gap-3 max-w-2xl w-full">
				<div class="relative">
					<input
						type="text"
						bind:value={searchText}
						placeholder="Search models..."
						class="w-full px-3 py-2 pl-8 text-xs rounded-lg
							bg-surface-800/80 border border-surface-600/50
							text-surface-100 placeholder:text-surface-500
							focus:outline-none focus:border-accent-500/50
							transition-colors"
					/>
					<span class="absolute left-2.5 top-1/2 -translate-y-1/2 text-surface-500 text-xs">
						&#128269;
					</span>
				</div>

				{#if providers.length > 1}
					<div class="flex flex-wrap gap-1.5">
						<span class="text-[10px] text-surface-500 self-center mr-1">Provider</span>
						{#each providers as provider}
							<button
								onclick={() => toggleProvider(provider)}
								class="px-2.5 py-1 text-[11px] rounded-full border transition-colors
									{selectedProvider === provider
										? 'bg-accent-500/20 border-accent-500/50 text-accent-400'
										: 'bg-surface-800/60 border-surface-600/50 text-surface-300 hover:border-surface-500 hover:text-surface-200'}"
							>
								{provider}
							</button>
						{/each}
					</div>
				{/if}

				<div class="flex flex-wrap gap-1.5">
					<span class="text-[10px] text-surface-500 self-center mr-1">Specialty</span>
					{#each SPECIALTIES as specialty}
						<button
							onclick={() => toggleSpecialty(specialty)}
							class="px-2.5 py-1 text-[11px] rounded-full border transition-colors
								{selectedSpecialty === specialty
									? 'bg-hecate-500/20 border-hecate-500/50 text-hecate-400'
									: 'bg-surface-800/60 border-surface-600/50 text-surface-300 hover:border-surface-500 hover:text-surface-200'}"
						>
							{specialty}
						</button>
					{/each}
				</div>

				<div class="flex items-center justify-between text-[10px] text-surface-500">
					<span>Showing {filteredModels.length} of {$models.length} models</span>
					{#if hasActiveFilters}
						<button
							onclick={clearFilters}
							class="text-surface-400 hover:text-accent-400 transition-colors"
						>
							Clear filters
						</button>
					{/if}
				</div>
			</div>

			<div class="grid grid-cols-2 lg:grid-cols-3 gap-3 max-w-2xl w-full">
				{#each filteredModels as model}
					<button
						onclick={() => openChat(model.name)}
						class="group relative flex flex-col items-center gap-2.5 p-5 rounded-xl
							bg-surface-800/80 border border-surface-600/50
							hover:border-accent-500/30 hover:bg-surface-700/80
							transition-all duration-200 cursor-pointer"
					>
						<span
							class="text-2xl transition-transform duration-200 group-hover:scale-110
								group-hover:drop-shadow-[0_0_8px_rgba(245,158,11,0.4)]"
						>
							&#9670;
						</span>
						<span class="text-sm font-medium text-surface-100 group-hover:text-accent-400 transition-colors">
							{model.name}
						</span>
						<div class="flex flex-col items-center gap-1">
							<div class="flex items-center gap-1.5">
								<span class="text-[10px] px-1.5 py-0.5 rounded-full bg-surface-700 text-hecate-400">
									{model.provider}
								</span>
								<span class="text-[10px] text-amber-400/80" title="Estimated cost">
									{costOf(model)}
								</span>
							</div>
							{#if model.parameter_size}
								<span class="text-[10px] text-surface-400">{model.parameter_size}</span>
							{/if}
						</div>
					</button>
				{/each}
			</div>

			{#if filteredModels.length === 0 && hasActiveFilters}
				<div class="text-center py-4">
					<div class="text-xs text-surface-400">No models match your filters</div>
					<button
						onclick={clearFilters}
						class="text-[11px] text-accent-400 hover:text-accent-300 mt-1 transition-colors"
					>
						Clear all filters
					</button>
				</div>
			{/if}
		{/if}
	</div>
{/if}
