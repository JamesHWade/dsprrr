import chokidar from "chokidar";
import { createHash } from "node:crypto";
import { readFile, readdir, writeFile } from "node:fs/promises";
import { join } from "node:path";
import * as esbuild from "esbuild";
import tailwindPlugin from "esbuild-plugin-tailwindcss";

const production = process.argv.includes("--production");
const watch = process.argv.includes("--watch");
const provenanceFile = "r/www/bundle-provenance.json";

function sha256(contents: Uint8Array | string) {
  return createHash("sha256").update(contents).digest("hex");
}

async function packageForInput(inputPath: string) {
  const parts = inputPath.split("/");
  const nodeModulesIndex = parts.lastIndexOf("node_modules");
  if (nodeModulesIndex < 0) return null;

  const packageStart = nodeModulesIndex + 1;
  const packageLength = parts[packageStart]?.startsWith("@") ? 2 : 1;
  const packageRoot = parts.slice(0, packageStart + packageLength).join("/");
  const packageJson = JSON.parse(
    await readFile(join(process.cwd(), packageRoot, "package.json"), "utf8")
  );

  return {
    name: packageJson.name,
    version: packageJson.version,
    license: packageJson.license ?? null,
    packageRoot,
  };
}

async function writeBundleProvenance(metafile: esbuild.Metafile) {
  const outputEntries: Array<[string, { bytes: number; sha256: string }]> = [];
  for (const outputPath of Object.keys(metafile.outputs).sort()) {
    const contents = await readFile(join(process.cwd(), outputPath));
    outputEntries.push([
      outputPath,
      { bytes: contents.byteLength, sha256: sha256(contents) },
    ]);
  }

  const jsOutput = Object.entries(metafile.outputs).find(
    ([outputPath, output]) => outputPath.endsWith(".js") && output.entryPoint
  );
  if (!jsOutput) throw new Error("Production build did not emit a JavaScript entry point.");

  const runtimeInputs = await Promise.all(
    Object.entries(jsOutput[1].inputs)
      .filter(([, input]) => input.bytesInOutput > 0)
      .map(async ([inputPath, input]) => {
        const contents = await readFile(join(process.cwd(), inputPath));
        const pkg = await packageForInput(inputPath);
        return {
          path: inputPath,
          bytes_in_output: input.bytesInOutput,
          sha256: sha256(contents),
          ...(pkg
            ? {
                package: pkg.name,
                version: pkg.version,
                license: pkg.license,
                package_path: pkg.packageRoot,
              }
            : {}),
        };
      })
  );

  const packageMap = new Map<string, {
    name: string;
    version: string;
    license: string | null;
    package_path: string;
    files: string[];
  }>();
  for (const input of runtimeInputs) {
    if (!input.package || !input.version || !input.package_path) continue;
    const key = `${input.package}@${input.version}:${input.package_path}`;
    const current = packageMap.get(key) ?? {
      name: input.package,
      version: input.version,
      license: input.license,
      package_path: input.package_path,
      files: [],
    };
    current.files.push(input.path);
    packageMap.set(key, current);
  }

  const cssOutput = Object.entries(metafile.outputs).find(([outputPath]) =>
    outputPath.endsWith(".css")
  );
  const cssSources = cssOutput
    ? await Promise.all(
        Object.entries(cssOutput[1].inputs)
          .filter(([, input]) => input.bytesInOutput > 0)
          .map(async ([inputPath, input]) => ({
            path: inputPath,
            bytes_in_output: input.bytesInOutput,
            sha256: sha256(await readFile(inputPath)),
          }))
      )
    : [];
  const cssSourceText = await Promise.all(
    cssSources.map(async ({ path }) => [path, await readFile(path, "utf8")])
  );
  const cssImports = cssSourceText.flatMap(([, source]) =>
    [...source.matchAll(/@import\s+["']([^"']+)["']/g)].map((match) => match[1])
  );
  const readToolVersion = async (packagePath: string) => {
    const packageJson = JSON.parse(
      await readFile(join(process.cwd(), "node_modules", packagePath, "package.json"), "utf8")
    );
    return {
      name: packageJson.name,
      version: packageJson.version,
      license: packageJson.license ?? null,
    };
  };
  const [tailwind, tailwindPostcss, autoprefixer, postcss, tailwindPluginPackage] =
    await Promise.all([
      readToolVersion("tailwindcss"),
      readToolVersion("@tailwindcss/postcss"),
      readToolVersion("autoprefixer"),
      readToolVersion("postcss"),
      readToolVersion("esbuild-plugin-tailwindcss"),
    ]);
  const packageLock = await readFile("package-lock.json");
  const componentsConfig = JSON.parse(await readFile("components.json", "utf8"));
  const shadcnUiFiles = (await readdir("srcts/components/ui"))
    .filter((file) => file.endsWith(".tsx"))
    .sort()
    .map((file) => `srcts/components/ui/${file}`);
  const shadcnSourcePaths = [
    ...shadcnUiFiles,
    "srcts/globals.css",
    "srcts/lib/utils.ts",
  ].sort();
  const shadcnSources = await Promise.all(
    shadcnSourcePaths.map(async (inputPath) => {
      const contents = await readFile(inputPath);
      const jsInput = runtimeInputs.find((input) => input.path === inputPath);
      const cssInput = cssSources.find((input) => input.path === inputPath);
      let scope = "shadcn/ui component source; locally adapted";
      if (inputPath === "srcts/globals.css") {
        scope = "shadcn/ui Tailwind theme scaffold with local tokens and app-specific rules";
      } else if (inputPath === "srcts/lib/utils.ts") {
        scope = "shadcn/ui cn() helper only; other helpers in this file are application-authored";
      }
      return {
        path: inputPath,
        scope,
        sha256: sha256(contents),
        included_in_javascript_bundle: Boolean(jsInput),
        javascript_bytes_in_output: jsInput?.bytes_in_output ?? 0,
        included_in_css_bundle: Boolean(cssInput),
        css_bytes_in_output: cssInput?.bytes_in_output ?? 0,
      };
    })
  );

  const provenance = {
    schema_version: 1,
    build: {
      command: "npm run build",
      entry_point: "srcts/main.tsx",
      node: process.version,
      platform: `${process.platform}/${process.arch}`,
      esbuild: esbuild.version,
      package_lock_sha256: sha256(packageLock),
      bundle: true,
      format: "esm",
      minify: true,
      legal_comments: "none",
      third_party_notices: "../../../THIRD-PARTY-NOTICES.md",
      banners: {
        js: "/*! Full third-party notices: ../../../THIRD-PARTY-NOTICES.md */",
        css: `/*! Tailwind CSS v${tailwind.version} (MIT); full text: ../../../THIRD-PARTY-NOTICES.md */`,
      },
    },
    outputs: Object.fromEntries(outputEntries),
    runtime_packages: [...packageMap.values()]
      .map((pkg) => ({ ...pkg, files: pkg.files.sort() }))
      .sort((a, b) =>
        `${a.name}@${a.version}:${a.package_path}`.localeCompare(
          `${b.name}@${b.version}:${b.package_path}`
        )
      ),
    runtime_inputs: runtimeInputs.sort((a, b) => a.path.localeCompare(b.path)),
    local_source_attributions: [
      {
        name: "shadcn/ui",
        repository: "https://github.com/shadcn-ui/ui",
        license: "MIT",
        copyright: "Copyright (c) 2023 shadcn",
        license_file: "https://github.com/shadcn-ui/ui/blob/main/LICENSE.md",
        configuration: "components.json",
        style: componentsConfig.style,
        source_revision: "not recorded; local components may be modified",
        files: shadcnSources,
      },
    ],
    css: {
      source_files: cssSources,
      imports: cssImports,
      generation_tools: [
        tailwindPluginPackage,
        tailwindPostcss,
        tailwind,
        postcss,
        autoprefixer,
      ],
    },
  };
  await writeFile(provenanceFile, `${JSON.stringify(provenance, null, 2)}\n`);
}

async function main() {
  const tailwindPackage = JSON.parse(
    await readFile("node_modules/tailwindcss/package.json", "utf8")
  );
  const config: esbuild.BuildOptions = {
    entryPoints: ["srcts/main.tsx"],
    outfile: "r/www/main.js",
    bundle: true,
    format: "esm",
    minify: production,
    sourcemap: production ? undefined : "linked",
    legalComments: production ? "none" : undefined,
    alias: { react: "react" },
    metafile: production,
    banner: production
      ? {
          js: "/*! Full third-party notices: ../../../THIRD-PARTY-NOTICES.md */",
          css: `/*! Tailwind CSS v${tailwindPackage.version} (MIT); full text: ../../../THIRD-PARTY-NOTICES.md */`,
        }
      : undefined,
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
    const result = await esbuild.build(config);
    if (production && result.metafile) {
      await writeBundleProvenance(result.metafile);
    }
  }
}

main();
