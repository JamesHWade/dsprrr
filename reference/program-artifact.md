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
interpreter factories. Format version 5 is the sole supported format. It
records exactly one runner or factory for each code-executing module,
preserves the complete Flex runtime contract, and stores graph-visible
RLM action and extraction predictors. Manifests with any other format
version are rejected before module construction. Factories are never
invoked during write or restore.

Declarative ellmer text, JSON, inline/remote image, and PDF content is
stored through a closed codec. Remote content URLs must be stable HTTPS
URLs without user information, query strings, fragments, or recognizable
signed-path credentials. Demo fields with credential-like names are
rejected instead of being silently removed from the program. Thinking,
tool-call, uploaded, and other runtime content still requires a registry
or trusted embedding. The payload digest detects changes but is not an
authenticity or trust signal. Registry-backed implementations are
identified by their registry name and interface digest, not by their
function bodies, so registry names should be immutable and versioned.
Structural exclusion records participate in the digest even though
excluded runtime values do not.

Artifacts currently reject cyclic module graphs with a typed error.
Shared acyclic nodes are represented once and reconstructed with
identical R6 identity at every edge.

## Usage

``` r
program_artifact(program, registry = list(), trusted = FALSE)

program_artifact_id(x, registry = list())

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

- x:

  A dsprrr `Module` or a `dsprrr_program_artifact` manifest.

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

- `program_artifact_id()` returns the artifact integrity digest as a
  scalar string prefixed with `"sha256:"`. The ID names the validated
  artifact payload; it is an integrity check, not an authenticity or
  trust signal. When `x` is a Module, registry entries actually
  referenced by the validated artifact are retained as detached runtime
  bindings so subsequent execution and module copies can recover the
  same verified ID without re-supplying the registry. A Module
  reconstructed from a validated current-format artifact retains that
  source artifact ID while its current serialization remains unchanged.
  This keeps the exact source producer environment represented in
  execution traces. Calling `program_artifact()` or `save_program()`
  creates a new artifact, whose ID can differ from the retained source
  ID. Semantic mutation switches the Module to the newly serialized
  artifact ID. Artifact inputs are checked for structure and integrity
  without requiring their recorded dependency versions to be installed.
  A Module restored from embedded trusted values is intentionally
  different: retain the validated artifact's ID, or explicitly create
  and identify a new artifact with
  `program_artifact(module, trusted = TRUE)`. Automatic execution
  metadata may omit the program ID when safe serialization cannot
  reproduce it without that explicit trust decision.

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
