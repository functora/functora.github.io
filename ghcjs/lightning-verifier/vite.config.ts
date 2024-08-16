import { defineConfig } from "vite";
import { resolve } from "path";

export default defineConfig({
  root: "./.",
  build: {
    outDir: "./static",
    minify: false,
    emptyOutDir: false,
    rollupOptions: {
      input: {
        main: resolve(__dirname, "static/cap.js"),
      },
      output: {
        entryFileNames: `[name].js`,
        chunkFileNames: `[name].js`,
        assetFileNames: `[name].[ext]`,
      },
    },
  },
});
