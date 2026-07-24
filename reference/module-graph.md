# Traverse and Transform Module Graphs

Module graphs provide one cycle-safe protocol for inspecting and
transforming nested dsprrr programs. Built-in adapters understand
pipelines, wrappers, ensembles, and multi-chain modules. Custom `Module`
subclasses can opt in by implementing two public methods:

- `graph_children()` returns a list whose leaves are child `Module`
  objects. Lists can be nested and can be named or unnamed.

- `set_graph_children(children)` replaces that complete child structure
  and returns the program invisibly. This method is required only for
  replacement and mapping operations; read-only traversal needs only
  `graph_children()`.

A custom module can additionally implement `graph_is_parameter()` and
return `TRUE` or `FALSE` to control whether `named_parameters()`
includes it.

Paths use a JSON-pointer-like form. `"$"` is the root, named list
elements use their names, and unnamed or ambiguously named lists use
one-based numeric positions. `/` and `~` in names are escaped as `~1`
and `~0`.

With `boundaries = "respect"`, compiled and frozen modules are included
as boundary nodes but neither they nor their descendants are mutated. A
shared module reachable through any protected path is protected at every
alias. Mapping visits each R6 identity once in post-order and rewires
all aliases to the same replacement. `replace_module()` defaults to
replacing one path and can opt into identity-wide replacement with
`shared = "all"`. Mapping can safely traverse cycles, but replacing a
module that participates in a cycle is rejected; use path-specific
`replace_module()` to break a cycle edge.

This is primarily useful when implementing or testing a custom module
graph adapter. Custom modules should normally implement
`graph_children()` rather than call this function from that method.

## Usage

``` r
module_graph(
  program,
  boundaries = c("cross", "respect"),
  cycles = c("record", "error")
)

named_modules(
  program,
  include_root = TRUE,
  aliases = FALSE,
  boundaries = c("cross", "respect"),
  cycles = c("record", "error")
)

named_parameters(
  program,
  include_root = TRUE,
  boundaries = c("respect", "cross")
)

map_modules(
  program,
  .fn,
  ...,
  include_root = TRUE,
  boundaries = c("respect", "cross")
)

replace_module(
  program,
  path,
  replacement,
  shared = c("path", "all"),
  boundaries = c("respect", "cross")
)

freeze_modules(program, paths = "$", recursive = TRUE, frozen = TRUE)

is_module_frozen(module)

set_module_lm(
  program,
  chat,
  include_root = TRUE,
  boundaries = c("cross", "respect"),
  clone = TRUE
)

module_children(module)
```

## Arguments

- program:

  A dsprrr `Module` object.

- boundaries:

  Whether compiled and frozen modules are crossed. Inspection defaults
  to `"cross"`, graph rewrites default to `"respect"`, and recursive LM
  propagation defaults to `"cross"` so runtime configuration reaches the
  complete program.

- cycles:

  Whether cycles are recorded or raise an error.

- include_root:

  Whether to include or transform the root module.

- aliases:

  Whether `named_modules()` includes shared and cyclic aliases.

- .fn:

  A function called as `.fn(module, path, ...)`. It must return a
  `Module`, or `NULL` to leave that module unchanged.

- ...:

  Additional arguments passed to `.fn`.

- path:

  A stable path returned by `module_graph()` or `named_modules()`.

- replacement:

  A replacement `Module`.

- shared:

  Whether replacement affects only `path` or every reference to the same
  R6 object.

- paths:

  One or more graph paths to freeze or unfreeze.

- recursive:

  Whether freezing applies to every descendant by identity.

- frozen:

  Logical; `TRUE` freezes and `FALSE` unfreezes.

- module:

  A dsprrr `Module` object.

- chat:

  An ellmer `Chat` object, or `NULL` to clear stored Chats.

- clone:

  Whether to give each module an independent deep clone of `chat`. The
  default avoids sharing mutable conversation history.

## Value

- `module_graph()` returns a tibble with one row per graph occurrence.

- `named_modules()` and `named_parameters()` return named lists.

- `map_modules()` and `replace_module()` return the resulting root
  module.

- `freeze_modules()` and `set_module_lm()` return `program` invisibly.

- `is_module_frozen()` returns one logical value.

- `module_children()` returns a list whose leaves are child modules.

## Examples

``` r
first <- module(signature("text -> answer"))
second <- module(signature("answer -> summary"))
program <- pipeline(first = first, second = second)

names(named_modules(program))
#> [1] "$"              "$/steps/first"  "$/steps/second"
names(named_parameters(program))
#> [1] "$/steps/first"  "$/steps/second"

program <- map_modules(program, function(module, path) {
  module$config$graph_path <- path
  module
})

freeze_modules(program, "$/steps/first")
is_module_frozen(first)
#> [1] TRUE
```
