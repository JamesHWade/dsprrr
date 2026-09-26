# Inspect and transform nested programs

These functions list, inspect and rewrite the modules inside a composed
program, such as a pipeline, ensemble or wrapper:

- `module_graph()` returns one row per module occurrence.

- `named_modules()` returns the modules by path.

- `named_parameters()` returns the tunable leaf modules by path: modules
  without children that optimizers can change, such as Predict modules.

- `map_modules()` applies a function to every module and swaps in the
  results.

- `replace_module()` replaces the module at one path.

- `freeze_modules()` and `is_module_frozen()` mark modules that rewrites
  should leave alone, and check the mark.

- `set_module_lm()` stores a chat on every module.

- `module_children()` returns the direct children of one module.

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

  A `Module` object.

- chat:

  An ellmer `Chat` object, or `NULL` to clear stored Chats.

- clone:

  Whether to give each module its own deep clone of `chat` (default
  `TRUE`), so that modules do not share conversation history.

## Value

- `module_graph()` returns a tibble with one row per module occurrence.

- `named_modules()` and `named_parameters()` return named lists.

- `map_modules()` and `replace_module()` return the resulting root
  module.

- `freeze_modules()` and `set_module_lm()` return `program` invisibly.

- `is_module_frozen()` returns one logical value.

- `module_children()` returns a list whose leaves are child modules.

## Details

### Paths

Paths look like JSON pointers. `"$"` is the root, named list elements
use their names, and unnamed or ambiguously named elements use one-based
positions, as in `"$/steps/first"`. `/` and `~` in names are escaped as
`~1` and `~0`.

### Boundaries

With `boundaries = "respect"`, compiled and frozen modules are listed as
boundary nodes, but neither they nor their descendants are changed. A
shared module reachable through any protected path is protected
everywhere. `map_modules()` visits each module object once, children
before parents, and rewires every reference to the same replacement.
`replace_module()` replaces one path by default; `shared = "all"`
replaces every reference to the same object. Mapping can traverse
cycles, but replacing a module that is part of a cycle is rejected; use
`replace_module()` on one path to break the cycle first.

### Custom modules

Built-in adapters understand pipelines, wrappers, ensembles and
multi-chain modules. A custom `Module` subclass can join the graph by
implementing two public methods:

- `graph_children()` returns a list, possibly nested and named or
  unnamed, whose leaves are the child `Module` objects.

- `set_graph_children(children)` replaces the whole child structure and
  returns the module invisibly. Only replacement and mapping need it.

It can also implement `graph_is_parameter()`, returning `TRUE` or
`FALSE`, to control whether `named_parameters()` includes it.
`module_children()` reports what these methods declare, which helps when
writing or testing an adapter; call it from outside `graph_children()`,
not inside it.

## See also

Other composition:
[`as_reward_fn()`](https://jameshwade.github.io/dsprrr/reference/as_reward_fn.md),
[`best_of_n()`](https://jameshwade.github.io/dsprrr/reference/best_of_n.md),
[`ensemble()`](https://jameshwade.github.io/dsprrr/reference/ensemble.md),
[`pipeline()`](https://jameshwade.github.io/dsprrr/reference/pipeline.md),
[`reduce_best_by_metric()`](https://jameshwade.github.io/dsprrr/reference/reduce_best_by_metric.md),
[`reduce_first()`](https://jameshwade.github.io/dsprrr/reference/reduce_first.md),
[`reduce_majority()`](https://jameshwade.github.io/dsprrr/reference/reduce_majority.md),
[`reduce_weighted_vote()`](https://jameshwade.github.io/dsprrr/reference/reduce_weighted_vote.md),
[`refine()`](https://jameshwade.github.io/dsprrr/reference/refine.md),
[`step()`](https://jameshwade.github.io/dsprrr/reference/step.md),
[`with_assertions()`](https://jameshwade.github.io/dsprrr/reference/with_assertions.md)

## Examples

``` r
first <- module(signature("text -> answer"))
second <- module(signature("answer -> summary"))
program <- pipeline(first = first, second = second)

module_graph(program)
#> # A tibble: 3 × 13
#>   path          parent_path depth class id    canonical_path shared cycle frozen
#>   <chr>         <chr>       <int> <chr> <chr> <chr>          <lgl>  <lgl> <lgl> 
#> 1 $             NA              0 Pipe… 0x55… $              FALSE  FALSE FALSE 
#> 2 $/steps/first $               1 Pred… 0x55… $/steps/first  FALSE  FALSE FALSE 
#> 3 $/steps/seco… $               1 Pred… 0x55… $/steps/second FALSE  FALSE FALSE 
#> # ℹ 4 more variables: compiled <lgl>, protected <lgl>, boundary <chr>,
#> #   module <list>
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
