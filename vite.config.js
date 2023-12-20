import { defineConfig } from "vite";
import { ViteEjsPlugin } from "vite-plugin-ejs";

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [
    ViteEjsPlugin((config) => {
      return {
        isDev: config.mode === "development",
      };
    }),
  ],
  server: {
    port: 8080,
  },
});
