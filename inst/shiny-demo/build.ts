import chokidar from "chokidar";
import * as esbuild from "esbuild";
import tailwindPlugin from "esbuild-plugin-tailwindcss";

const production = process.argv.includes("--production");
const watch = process.argv.includes("--watch");

async function main() {
  const config: esbuild.BuildOptions = {
    entryPoints: ["srcts/main.tsx"],
    outfile: "r/www/main.js",
    bundle: true,
    format: "esm",
    minify: production,
    sourcemap: production ? undefined : "linked",
    alias: { react: "react" },
    logLevel: "info",
    plugins: [tailwindPlugin()],
  };

  if (watch) {
    const context = await esbuild.context(config);
    await context.rebuild();
    chokidar
      .watch(["srcts/"], {
        ignored: ["**/node_modules/**"],
        ignoreInitial: true,
      })
      .on("all", async () => {
        await context.rebuild();
      });
  } else {
    await esbuild.build(config);
  }
}

main();
