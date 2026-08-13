# Persist Complete dsprrr Programs

Program artifacts are versioned, transport-independent manifests for
complete dsprrr module graphs. They preserve signatures, declarative
configuration, demos, optimization provenance, compiled state, pipeline
mappings, wrappers, ensembles, and shared module identity. Runtime
chats, credentials, generated prompts, caches, and execution history are
excluded.

Callables and runtime objects are never captured implicitly. Supply a
named `registry` to store stable IDs, or set `trusted = TRUE` to embed
them. Embedded values are restored only when `trusted = TRUE` is also
supplied while loading. Registry IDs are the recommended contract for
tools, custom functions, retrievers, stores, code runners, and
interpreter factories. Format version 5 adds graph-visible RLM action
and extraction predictors. Version 4 records exactly one runner or
factory for each code-executing module. Valid version 3 runner-only and
version 4 manifests are checked against their original schema and
integrity digest. Non-RLM manifests are upgraded in memory. A legacy
childless RLM is restored under its original schema with fresh default
child predictors and becomes a complete version 5 graph when it is next
saved. Other historical versions are rejected. Factories are never
invoked during write or restore.

Declarative ellmer text, JSON, inline/remote image, and PDF content is
stored through a closed codec. Remote content URLs must be stable HTTPS
URLs without user information, query strings, fragments, or recognizable
signed-path credentials. Demo fields with credential-like names are
rejected instead of being silently removed from the program. Thinking,
tool-call, uploaded, and other runtime content still requires a registry
or trusted embedding. The payload digest detects changes but is not an
authenticity or trust signal.

Artifacts currently reject cyclic module graphs with a typed error.
Shared acyclic nodes are represented once and reconstructed with
identical R6 identity at every edge.

## Usage

``` r
program_artifact(program, registry = list(), trusted = FALSE)

save_program(program, path, registry = list(), trusted = FALSE)

load_program(path, registry = list(), trusted = FALSE)
```

## Arguments

- program:

  A dsprrr `Module` program.

- registry:

  A named list of functions or runtime objects. Artifact records contain
  registry names, never the registered values themselves.

- trusted:

  Whether arbitrary runtime values may be embedded or restored. This is
  `FALSE` by default and should be enabled only for artifacts and code
  you trust.

- path:

  An artifact path on a stable local filesystem, in a containing
  directory trusted against hostile concurrent mutation.
  `save_program()` stages and validates a private temporary file in the
  same directory, then publishes it with a same-filesystem atomic move.
  An ordinary move failure leaves an existing destination unchanged, or
  publishes no destination. If verification after a successful move
  fails, `save_program()` errors but the new destination may already be
  present.

## Value

- `program_artifact()` returns a `dsprrr_program_artifact` manifest.

- `save_program()` invisibly returns `path`.

- `load_program()` returns the reconstructed root module.

## Examples

``` r
mod <- module(signature("text -> answer"))
artifact <- program_artifact(mod)
restored <- restore_module_config(artifact)
#> ✔ Restored program artifact
#> ℹ Root module: <PredictModule>
#> ℹ Artifact version: 5

path <- tempfile(fileext = ".rds")
save_program(mod, path)
restored <- load_program(path)
unlink(path)
```
