# Configure one pipeline step

`step()` wraps a module for
[`pipeline()`](https://jameshwade.github.io/dsprrr/reference/pipeline.md)
when its inputs do not line up with the previous step's outputs by name,
when only some of its outputs should go on to the next step, or when it
needs fixed inputs.

## Usage

``` r
step(module, map = list(), select = character(0), ...)
```

## Arguments

- module:

  The module to run at this step.

- map:

  A named character vector (or list) that renames fields coming from the
  previous step. Names are the previous step's output fields and values
  are this module's input fields: `map = c(summary = "passage")` passes
  the upstream `summary` as `passage`. In the first step, `map` renames
  the pipeline's inputs.

- select:

  Names of this step's output fields to pass to the next step. By
  default all of them are passed. For the last step, `select` also
  limits what the pipeline returns.

- ...:

  Fixed inputs for this step, as `name = value`: values for input fields
  of `module` that stay the same on every call. They are inputs, not
  settings, so something like `system_prompt = "Be brief"` would reach
  the prompt as an extra input field. They override mapped values with
  the same name.

## Value

A pipeline step (an S7 object of class `PipelineStep`) to pass to
[`pipeline()`](https://jameshwade.github.io/dsprrr/reference/pipeline.md).

## See also

Other composition:
[`as_reward_fn()`](https://jameshwade.github.io/dsprrr/reference/as_reward_fn.md),
[`best_of_n()`](https://jameshwade.github.io/dsprrr/reference/best_of_n.md),
[`ensemble()`](https://jameshwade.github.io/dsprrr/reference/ensemble.md),
[`module-graph`](https://jameshwade.github.io/dsprrr/reference/module-graph.md),
[`pipeline()`](https://jameshwade.github.io/dsprrr/reference/pipeline.md),
[`reduce_best_by_metric()`](https://jameshwade.github.io/dsprrr/reference/reduce_best_by_metric.md),
[`reduce_first()`](https://jameshwade.github.io/dsprrr/reference/reduce_first.md),
[`reduce_majority()`](https://jameshwade.github.io/dsprrr/reference/reduce_majority.md),
[`reduce_weighted_vote()`](https://jameshwade.github.io/dsprrr/reference/reduce_weighted_vote.md),
[`refine()`](https://jameshwade.github.io/dsprrr/reference/refine.md),
[`with_assertions()`](https://jameshwade.github.io/dsprrr/reference/with_assertions.md)

## Examples

``` r
summarize <- module_fn("text -> summary", function(text) substr(text, 1, 30))
translate <- module_fn(
  "passage, language -> translation, n_chars: int",
  function(passage, language) {
    list(translation = paste0("[", language, "] ", passage), n_chars = nchar(passage))
  }
)
shout <- module_fn("translation -> reply", function(translation) toupper(translation))

p <- pipeline(
  summarize,
  # `summary` becomes `passage`, `language` is fixed, and only
  # `translation` goes on to `shout`
  step(translate, map = c(summary = "passage"), select = "translation", language = "French"),
  shout
)
run(p, text = "dsprrr turns language model calls into programs.")
#> $reply
#> [1] "[FRENCH] DSPRRR TURNS LANGUAGE MODEL CA"
#> 
```
