# Chain modules into a pipeline

`pipeline()` builds a module that runs other modules one after another,
feeding each step's outputs to the next step as inputs. `lhs %>>% rhs`
chains two modules the same way, or adds a module to the end of a
pipeline.

## Usage

``` r
lhs %>>% rhs

pipeline(...)
```

## Arguments

- lhs:

  A module or a pipeline. A pipeline is extended with `rhs`.

- rhs:

  A module. A pipeline on the right is added as a single step.

- ...:

  Modules, or steps made with
  [`step()`](https://jameshwade.github.io/dsprrr/reference/step.md), in
  the order they run.

## Value

A module (an R6 object of class `PipelineModule`) to use with
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md),
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md),
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
and
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md).

## Details

The first step receives the inputs given to
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md). Every
later step receives only the outputs of the step before it, plus its own
fixed inputs: the pipeline's inputs are not passed further down. Outputs
connect to inputs of the same name; use
[`step()`](https://jameshwade.github.io/dsprrr/reference/step.md) to
rename them (`map`), to pass on only some of them (`select`) or to give
a step fixed inputs. A step whose inputs are not all available fails
with an error that lists what was available.

The pipeline returns the last step's output. With
`.return_format = "structured"`, its metadata adds up tokens, cost and
latency over the steps and keeps each step's metadata in
`step_metadata`.
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
with
[`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md)
optimizes the demos of all steps together.

## See also

Other composition:
[`as_reward_fn()`](https://jameshwade.github.io/dsprrr/reference/as_reward_fn.md),
[`best_of_n()`](https://jameshwade.github.io/dsprrr/reference/best_of_n.md),
[`ensemble()`](https://jameshwade.github.io/dsprrr/reference/ensemble.md),
[`module-graph`](https://jameshwade.github.io/dsprrr/reference/module-graph.md),
[`reduce_best_by_metric()`](https://jameshwade.github.io/dsprrr/reference/reduce_best_by_metric.md),
[`reduce_first()`](https://jameshwade.github.io/dsprrr/reference/reduce_first.md),
[`reduce_majority()`](https://jameshwade.github.io/dsprrr/reference/reduce_majority.md),
[`reduce_weighted_vote()`](https://jameshwade.github.io/dsprrr/reference/reduce_weighted_vote.md),
[`refine()`](https://jameshwade.github.io/dsprrr/reference/refine.md),
[`step()`](https://jameshwade.github.io/dsprrr/reference/step.md),
[`with_assertions()`](https://jameshwade.github.io/dsprrr/reference/with_assertions.md)

## Examples

``` r
# Function-backed steps, so the example runs offline
summarize <- module_fn("text -> summary", function(text) substr(text, 1, 30))
translate <- module_fn(
  "passage, language -> translation",
  function(passage, language) paste0("[", language, "] ", passage)
)

# `summary` feeds the `passage` input; `language` is fixed for this step
p <- pipeline(
  summarize,
  step(translate, map = c(summary = "passage"), language = "French")
)
p
#> 
#> ── PipelineModule ──
#> 
#> ── Steps (2) 
#> • [1] FnModule: text -> summary
#> • [2] FnModule: passage, language -> translation (map: summary -> passage)
#> 
#> ── Composite Signature 
#> 
#> ── Signature ──
#> 
#> ── Inputs 
#> • text: "string" - Input: text
#> 
#> ── Output 
#> Type: "object(translation: string)"
#> 
#> ── Instructions 
#> Given the fields `text`, produce the fields `summary`.
run(p, text = "dsprrr turns language model calls into programs you can test.")
#> $translation
#> [1] "[French] dsprrr turns language model ca"
#> 

# With %>>%, outputs and inputs connect by name
shout <- module_fn("summary -> reply", function(summary) toupper(summary))
run(summarize %>>% shout, text = "Matching names connect on their own.")
#> $reply
#> [1] "MATCHING NAMES CONNECT ON THEI"
#> 

if (FALSE) { # \dontrun{
outline <- module(signature("topic -> outline"))
draft <- chain_of_thought("outline -> article")
writer <- outline %>>% draft
run(
  writer,
  topic = "Testing R code that calls language models",
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)
} # }
```
