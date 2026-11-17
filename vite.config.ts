import elm from "vite-plugin-elm";
import css from "@tailwindcss/vite";
import { defineConfig, loadEnv } from "vite";
import { api } from "./api/_ts/dev.ts";

export default defineConfig(({ mode }) => {
  const env = loadEnv(mode, process.cwd(), "");
  return {
    define: {
      __PADDLE_CLIENT_TOKEN__: JSON.stringify(env.PADDLE_CLIENT_TOKEN || null),
      __PADDLE_ENVIRONMENT__: JSON.stringify(env.PADDLE_ENVIRONMENT || null),
    },
    plugins: [css(), elm(), api()],
    // The browser app, which Vercel serves as static files (vercel.json outputDirectory).
    build: { outDir: "dist/client" },
    worker: { format: "es" },
    environments: {
      ssr: {
        build: {
          // Not under dist/client, so the server bundle is never served publicly.
          outDir: "dist/server",
          target: "esnext",
          rollupOptions: {
            input: { app: "api/_ts/app.ts" },
          },
        },
      },
    },
    builder: {
      async buildApp(builder) {
        await builder.build(builder.environments.ssr);
        await builder.build(builder.environments.client);
      },
    },
  };
});
