# How dsprrr works

dsprrr treats each LLM call as a small program with a declared
interface, an implementation you can inspect, and a score you can
measure. This page explains that model and the two objects that carry
it: signatures, which declare what goes in and comes out, and modules,
which hold everything that can change. By the end you will know what
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md),
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
and
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
do to a module, and when a plain prompt is the better tool.

``` r

library(dsprrr)
```

## Programs you can measure

A hand-written prompt mixes three things in one string: the task’s
interface (a review goes in, a label comes out), the wording that gets a
model to do it, and perhaps a few examples. When you change the wording
or switch models, you re-read a handful of outputs and decide whether
they look better, with no record of how often the old version was right.

dsprrr follows [DSPy](https://dspy.ai) (Declarative Self-improving
Python; see the [paper](https://arxiv.org/abs/2310.03714)) and keeps
those parts apart. A signature declares the interface. A module holds
the wording, the examples and the settings. A metric scores outputs
against examples you trust, and an optimizer searches over the module’s
wording and examples for the version with the best score.

If you model data in R, the split is familiar. A formula such as `y ~ x`
states the model, [`lm()`](https://rdrr.io/r/stats/lm.html) fits it to
data, and you judge the fit on rows it has not seen. A signature states
the task,
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
fits the prompt to labeled examples, and
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
scores the result on held-out rows.

## Before and after

Here is a sentiment classifier written as a plain ellmer prompt:

``` r

chat <- ellmer::chat_openai(model = "gpt-6-luna")
chat$chat(
  "Is this review positive, negative or neutral? The battery died after a week."
)
```

The reply is a sentence you have to parse, the wording is frozen inside
the string, and you have no measure of how often it is right. The same
task in dsprrr:

``` r

classify <- module(signature(
  "review -> sentiment: enum('positive', 'negative', 'neutral')",
  instructions = "Classify the sentiment of a product review."
))

run(classify, review = "The battery died after a week.", .llm = chat)
```

[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) returns
a named list such as `list(sentiment = "negative")`. The signature’s
output type is sent to the provider as a JSON schema, so the answer
comes back as one of the three labels rather than a sentence to parse.
With a few labeled reviews you can measure the module and let an
optimizer improve it:

``` r

reviews <- tibble::tibble(
  review = c(
    "The battery died after a week.",
    "Does exactly what it says on the box.",
    "Arrived on time. Nothing special.",
    "Best purchase I have made this year.",
    "The strap broke on day two.",
    "Fine for the price.",
    "Customer service never replied.",
    "Works well, and setup took two minutes."
  ),
  sentiment = c(
    "negative", "positive", "neutral", "positive",
    "negative", "neutral", "negative", "positive"
  )
)
splits <- split_dataset(reviews, prop = 0.5, seed = 1)
metric <- metric_exact_match(field = "sentiment")

before <- evaluate(classify, splits$val, metric, .llm = chat)

tuned <- classify |>
  compile(LabeledFewShot(k = 3L), splits$train, .llm = chat)
after <- evaluate(tuned, splits$val, metric, .llm = chat)

c(before = before$mean_score, after = after$mean_score)
```

Eight rows are enough to show the calls, not to trust the numbers;
[Metrics and
evaluation](https://jameshwade.github.io/dsprrr/articles/concepts-why-metrics-matter.md)
covers how much data a comparison needs.

## Signatures declare the interface

A signature lists the inputs, the output fields with their types, and
the instructions. Printing one shows what the module will ask for:

``` r

sig <- signature(
  "review -> sentiment: enum('positive', 'negative', 'neutral')",
  instructions = "Classify the sentiment of a product review."
)
sig
#> 
#> ── Signature ──
#> 
#> ── Inputs
#> • review: "string" - Input: review
#> 
#> ── Output
#> Type: "object(sentiment: enum(positive, negative, neutral))"
#> 
#> ── Instructions
#> Classify the sentiment of a product review.
```

The string notation accepts `str`, `int`, `float`, `bool`,
`enum('a', 'b')` (or `Literal['a', 'b']`), `list[...]` and
`Optional[...]`, and several outputs separated by commas:
`"review -> sentiment: enum('positive', 'negative'), confidence: float"`.
For input descriptions or nested objects, pass
`inputs = list(input(...))` and an ellmer type such as
[`ellmer::type_object()`](https://ellmer.tidyverse.org/reference/type_boolean.html);
see [Tutorial 3: Extract structured
data](https://jameshwade.github.io/dsprrr/articles/tutorial-structured-outputs.md).

Signatures are S7 objects, and S7 objects behave like values: modifying
one produces a new signature and leaves every other copy unchanged. That
holds for the helpers
[`with_instructions()`](https://jameshwade.github.io/dsprrr/reference/with_instructions.md),
[`append_instructions()`](https://jameshwade.github.io/dsprrr/reference/with_instructions.md)
and
[`with_reasoning()`](https://jameshwade.github.io/dsprrr/reference/with_reasoning.md),
and for direct assignment with `@<-`:

``` r

terse <- append_instructions(sig, "Reply with the label only.")
terse@instructions
#> [1] "Classify the sentiment of a product review.\n\nReply with the label only."
sig@instructions
#> [1] "Classify the sentiment of a product review."

edited <- sig
edited@instructions <- "Label the review."
sig@instructions
#> [1] "Classify the sentiment of a product review."
```

This is what lets an optimizer try many instruction variants safely.
Each candidate gets its own signature; the one held by your baseline
module never changes underneath it.

## Modules hold everything that can change

A module is an R6 object that wraps a signature together with the parts
that vary between versions of the program:

| Field | Holds | Changed by |
|----|----|----|
| `mod$signature` | The interface | Optimizers that tune instructions swap in a new signature |
| `mod$demos` | Few-shot examples | [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md) with [`LabeledFewShot()`](https://jameshwade.github.io/dsprrr/reference/LabeledFewShot.md) or [`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md) |
| `mod$config` | Settings such as `temperature` | You, or [`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md) |
| `mod$chat` | An optional ellmer chat for this module | You, via `module(sig, chat = ...)` |
| `mod$state` | Traces and optimization results | [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md), [`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md) and optimizers |

R6 objects have reference semantics. Assigning a module to a new name
gives you a second name for the same object, so a change through either
name shows up in both. `mod$copy()` makes an independent module with the
same signature, demos and config, and no traces:

``` r

classify <- module(sig)

same <- classify
same$config$temperature <- 0
classify$config$temperature
#> [1] 0

twin <- classify$copy()
twin$config$temperature <- 1
classify$config$temperature
#> [1] 0
```

State is what makes a module inspectable. Every
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) appends
a trace with the timestamp, latency, token counts, cost, model, prompt
and response. `mod$get_traces()` returns them as a tibble with one row
per call, `summarize_traces(mod)` totals them, and
[`get_last_prompt()`](https://jameshwade.github.io/dsprrr/reference/get_last_prompt.md)
prints the most recent prompt and response.

## Running, evaluating and compiling

Five functions cover the life of a module:

| Function | What it does | Returns | Changes the module |
|----|----|----|----|
| `run(mod, ...)` | Calls the model for the given inputs | A named list of outputs | Adds traces |
| `run_dataset(mod, data)` | Calls the model once per row | `data` plus a `result` list-column | Adds traces |
| `evaluate(mod, data, metric)` | Runs every row and scores it with `metric` | `mean_score`, `scores`, `predictions` and more | Adds traces |
| `compile(mod, teleprompter, trainset)` | Searches demos or instructions | A new, compiled module | No |
| `optimize_grid(mod, data, metric, parameters = )` | Scores every combination of settings | The module, updated | Yes, in place |

Most of what an optimizer tunes is discrete: which instruction wording,
which examples to show. So optimizers propose candidates, score each
with your metric on your data, and keep the best. `mod$is_compiled()`
reports whether a module came out of that process. [How optimization
works](https://jameshwade.github.io/dsprrr/articles/concepts-optimization-theory.md)
describes the individual optimizers.

## Other module types

[`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)
makes a single prediction. Other constructors change how a call is
carried out while keeping the same
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) and
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
interface:
[`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md)
adds a reasoning field,
[`react()`](https://jameshwade.github.io/dsprrr/reference/react.md) lets
the model call tools,
[`best_of_n()`](https://jameshwade.github.io/dsprrr/reference/best_of_n.md)
and
[`refine()`](https://jameshwade.github.io/dsprrr/reference/refine.md)
retry against a reward function,
[`multi_chain_comparison()`](https://jameshwade.github.io/dsprrr/reference/multi_chain_comparison.md)
and
[`ensemble()`](https://jameshwade.github.io/dsprrr/reference/ensemble.md)
combine several answers, and
[`pipeline()`](https://jameshwade.github.io/dsprrr/reference/pipeline.md)
chains modules. [Choose a module
type](https://jameshwade.github.io/dsprrr/articles/advanced-modules.md)
compares them.

When a step is plain R logic, or needs to call other modules in a way a
pipeline cannot express, wrap a function with
[`module_fn()`](https://jameshwade.github.io/dsprrr/reference/module_fn.md):

``` r

count_words <- module_fn(
  "text -> n_words: int",
  function(text) list(n_words = lengths(strsplit(text, "\\s+")))
)
run(count_words, text = "The battery died after a week.")
#> $n_words
#> [1] 6
```

Function modules work with
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md),
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
and
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md).
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
refuses them, because an optimizer cannot see inside an arbitrary R
function.

## When plain prompting is still fine

Not every LLM call needs a signature, a dataset and an optimizer. A
direct ellmer call is often the better choice when the task is simple
and unlikely to change, when you are still exploring and do not yet know
what a good output looks like, or when the prompt runs once and there is
nothing to reuse.

The structure pays off once a call runs repeatedly, has to keep working
when you change models, or needs evidence that a change helped. That
evidence does not always require hand-labeled answers:
[`metric_contains()`](https://jameshwade.github.io/dsprrr/reference/metric_contains.md)
checks for a fixed pattern, a custom metric can check format or length,
and a second model can grade outputs against a rubric (see [Evaluate
with
vitals](https://jameshwade.github.io/dsprrr/articles/vitals-recipes.md)).

The next page, [Metrics and
evaluation](https://jameshwade.github.io/dsprrr/articles/concepts-why-metrics-matter.md),
covers how to choose a metric and how far to trust a score.
