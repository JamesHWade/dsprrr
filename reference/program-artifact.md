# Save and load complete programs

`save_program()` writes a program to an `.rds` file and `load_program()`
rebuilds it, including nested modules, demonstrations, optimization
results and compiled state. `program_artifact()` returns the same
content as an R object, a versioned manifest, for example to pin with
[`pin_module_config()`](https://jameshwade.github.io/dsprrr/reference/pin_module_config.md)
or to rebuild with
[`restore_module_config()`](https://jameshwade.github.io/dsprrr/reference/restore_module_config.md).
`program_artifact_id()` returns a digest that identifies an artifact's
content.

Chats, credentials, generated prompts, caches and execution history are
never saved: give the restored program a chat when you run it.

## Usage

``` r
program_artifact(program, registry = list(), trusted = FALSE)

program_artifact_id(x, registry = list())

save_program(program, path, registry = list(), trusted = FALSE)

load_program(path, registry = list(), trusted = FALSE)
```

## Arguments

- program:

  A dsprrr module or composed program.

- registry:

  A named list of functions or runtime objects. Artifacts store the
  names, never the objects.

- trusted:

  Whether arbitrary runtime values may be embedded or restored (default
  `FALSE`). Enable it only for artifacts and code you trust.

- x:

  A module or a `dsprrr_program_artifact` manifest.

- path:

  Path of the `.rds` file, on a local file system, in a directory that
  other processes do not change at the same time. `save_program()`
  writes a private temporary file in the same directory, checks it, and
  moves it into place, so a failed move leaves any existing file
  unchanged. If the check after the move fails, `save_program()` errors,
  but the new file may already be in place.

## Value

- `program_artifact()` returns a `dsprrr_program_artifact` manifest.

- `program_artifact_id()` returns the digest as a string starting with
  `"sha256:"`. Artifacts are checked for structure and integrity,
  without requiring the recorded dependency versions to be installed. A
  module rebuilt from an artifact keeps reporting that artifact's ID
  until it is changed, so execution traces point to the exact source;
  saving it again creates a new artifact whose ID can differ. Registry
  entries that a module's artifact uses stay attached to the module, so
  later calls and copies recover the same ID without the registry. A
  module rebuilt from trusted embedded values is different: execution
  metadata may omit its ID unless you create a new artifact with
  `program_artifact(module, trusted = TRUE)`.

- `save_program()` returns `path`, invisibly.

- `load_program()` returns the rebuilt program.

## Details

### Functions and other runtime objects

Tools, custom functions, retrievers, stores, code runners and
interpreter factories are never captured implicitly. List them in a
named `registry`: the artifact stores only their names, and loading with
the same registry puts them back. An entry is identified by its name and
interface digest, not by its function body, so keep registry names
stable and versioned. Alternatively, `trusted = TRUE` embeds the objects
themselves; they are restored only when `trusted = TRUE` is also passed
when loading, so use it only for artifacts and code you trust. Factories
are never called while saving or loading.

### Content rules

Declarative ellmer content (text, JSON, inline or remote images, and
PDFs) is stored as data. Remote URLs must be stable HTTPS URLs without
user information, query strings, fragments or signed-path credentials.
Demonstration fields with credential-like names are rejected rather than
silently dropped. Thinking, tool-call, uploaded and other runtime
content needs a registry or trusted embedding.

### Format and identity

Format version 6 is the only supported format; artifacts with another
version are rejected before any module is built. It records exactly one
runner or factory for each code-executing module, the complete Flex
runtime contract, and the action and extraction predictors of RLM
modules. Cyclic module graphs are rejected. A module shared by several
parents is stored once and restored as one object everywhere it is used.

The digest detects changes; it is not a sign of authenticity or trust.
Registry entries count toward it through their names and interface
digests, and records of excluded runtime values count too, although the
values do not.

## See also

Other persistence:
[`export_module_code()`](https://jameshwade.github.io/dsprrr/reference/export_module_code.md),
[`pin_module_config()`](https://jameshwade.github.io/dsprrr/reference/pin_module_config.md),
[`pin_trace()`](https://jameshwade.github.io/dsprrr/reference/pin_trace.md),
[`pin_vitals_log()`](https://jameshwade.github.io/dsprrr/reference/pin_vitals_log.md),
[`restore_module_config()`](https://jameshwade.github.io/dsprrr/reference/restore_module_config.md)

## Examples

``` r
mod <- module(signature("text -> answer"))

path <- tempfile(fileext = ".rds")
save_program(mod, path)
restored <- load_program(path)
restored
#> 
#> ── PredictModule ──
#> 
#> ── Signature 
#> 
#> ── Signature ──
#> 
#> ── Inputs 
#> • text: "string" - Input: text
#> 
#> ── Output 
#> Type: "object(answer: string)"
#> 
#> ── Instructions 
#> Given the fields `text`, produce the fields `answer`.

artifact <- program_artifact(mod)
program_artifact_id(artifact)
#> [1] "sha256:d2af4010a1b84a7c04c034962a7996ccb00e8eaeab2ad949933663b27b7fefd5"
identical(program_artifact_id(restored), program_artifact_id(artifact))
#> [1] TRUE
unlink(path)
```
