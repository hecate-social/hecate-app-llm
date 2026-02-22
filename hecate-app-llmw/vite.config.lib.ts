import { svelte } from '@sveltejs/vite-plugin-svelte';
import { defineConfig } from 'vite';

export default defineConfig({
	plugins: [svelte({
		compilerOptions: {
			css: 'injected',
			customElement: true
		}
	})],
	build: {
		lib: {
			entry: 'src/lib/LlmStudio.svelte',
			formats: ['es'],
			fileName: () => 'component.js'
		},
		outDir: 'dist',
		emptyOutDir: true
	}
});
