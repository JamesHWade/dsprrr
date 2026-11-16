# Shiny demo asset provenance

## Rebuilding the browser assets

From the repository root, run:

```sh
cd inst/shiny-demo
npm ci
npm run build
```

`npm ci` installs the exact versions in `package-lock.json`. The production build writes `r/www/main.js`, `r/www/main.css`, and `r/www/bundle-provenance.json`. The manifest is generated from esbuild's production metafile and records the lockfile hash, Node/esbuild versions, relative input paths and hashes, package versions and license identifiers, CSS generation tools, and output hashes. It does not contain machine-specific absolute paths or timestamps.

The `runtime_packages` list includes only package instances with source files that have positive `bytesInOutput` in the production metafile. The current build records 52 runtime package instances across 115 contributing JavaScript input files; `runtime_inputs` includes both package files and local `srcts` files. Tailwind runs through `esbuild-plugin-tailwindcss` and PostCSS before esbuild emits the stylesheet, so esbuild's CSS metafile names `srcts/globals.css` but does not expose Tailwind's generated virtual input. The manifest records the `@import "tailwindcss"` source and exact CSS generator versions separately. Production emits no source map; the manifest records input paths and hashes instead.

Production esbuild legal comments are disabled because the upstream React and Lucide comments refer to their source-tree `LICENSE` files. The JavaScript and CSS banners now point to the full texts in [`../THIRD-PARTY-NOTICES.md`](../THIRD-PARTY-NOTICES.md); the CSS banner also names the generated Tailwind CSS version.

The previous checked-in JS and CSS did not match a no-write production build from the locked dependency tree. They have been regenerated from that tree without changing `package.json` or `package-lock.json`. Two consecutive builds in this audit produced identical asset and manifest hashes; see `r/www/bundle-provenance.json` for the resulting values.

## Locally copied UI source

The source audit found shadcn/ui New York component templates in `srcts/components/ui/`, the adapted Tailwind theme scaffold in `srcts/globals.css`, and the shadcn `cn()` helper in `srcts/lib/utils.ts`. The manifest records relative paths, SHA-256 hashes, bundling status, and emitted byte counts for these files. It marks the components as locally adapted and limits the `utils.ts` attribution to `cn()`; the other helpers there are app-authored. `components.json` records the New York style, but the exact upstream component revision was not retained, so the manifest does not claim one. Full MIT text and the file-by-file scope are in [`../THIRD-PARTY-NOTICES.md`](../THIRD-PARTY-NOTICES.md).

The other demo components, hooks, and phase/type helpers were inspected and appear app-authored; no additional copied source was identified. The retained bslib, Bootstrap, and Bootswatch trace excerpts are covered in the notices and audited separately below.

## Recorded bslib traces

`inst/scripts/record-demo-traces.R` clones `rstudio/bslib` from the moving `main` branch and supplies its R, SCSS, Sass, and CSS files as context. The saved trace files retain selected source excerpts in REPL outputs, and record the source size and file count. They do not record the clone commit or bslib package version, so the exact historical source revision cannot be recovered. The recursive fixture has 73 distinct source-file markers across bslib R files, Bootstrap 4/5 Sass, and Bootswatch 4/5 Sass. It contains an explicit 5.3.1 Bootstrap source label.

The smallest future provenance improvement is to add the cloned bslib commit SHA and package version to each trace when it is recorded. The existing fixtures cannot be backfilled with a verified SHA from their current metadata. [`../THIRD-PARTY-NOTICES.md`](../THIRD-PARTY-NOTICES.md) includes the full bslib, Bootstrap, and Bootswatch license texts for the retained excerpts.
