<script lang="ts">
	import '../app.css';

	let { children } = $props();
</script>

<div class="min-h-screen bg-surface-900 text-surface-100">
	{@render children()}
</div>
