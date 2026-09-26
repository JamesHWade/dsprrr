# dsprrr

dsprrr lets you write LLM features in R as small programs instead of
prompt strings. You declare a task’s inputs and typed outputs; dsprrr
builds the prompt, calls the model, and returns an R list with the types
you asked for. Once you have labeled examples, you can score the program
with a metric and let an optimizer tune its instructions and few-shot
examples against that score.

The design follows [DSPy](https://dspy.ai). Model calls go through
[ellmer](https://ellmer.tidyverse.org), so any provider ellmer supports
works. dsprrr is experimental and not yet on CRAN; the API may still
change.

## Installation

``` r

# install.packages("pak")
pak::pak("JamesHWade/dsprrr")
```

You also need credentials for a model provider, for example
`OPENAI_API_KEY` in your `.Renviron`.

## Example

A signature names the inputs and outputs of a task.
[`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)
turns it into something you can run.

``` r

library(dsprrr)
chat <- ellmer::chat_openai(model = "gpt-6-luna")

analyzer <- module(signature(
  "review -> sentiment: enum('positive', 'negative', 'neutral'), stars: int, summary: string"
))

result <- run(
  analyzer,
  review = "I've been using this blender for 6 months now. It's incredibly powerful and easy to clean. The only downside is it's quite loud. Overall, I'm very happy with it.",
  .llm = chat
)
str(result)
#> List of 3
#>  $ sentiment: chr "positive"
#>  $ stars    : int 4
#>  $ summary  : chr "Powerful and easy-to-clean blender, but a bit loud."
```

`stars` comes back as an integer and `sentiment` is always one of the
three labels, so the result can go straight into a data frame. The
output above was recorded from a real call to `gpt-4.1` in [Tutorial
3](https://jameshwade.github.io/dsprrr/articles/tutorial-structured-outputs.md);
`gpt-6-luna` may word the summary differently.

## How the pieces fit

### Signatures

**Declare the task.** Inputs on the left of the arrow, typed outputs on
the right, and optional instructions. The prompt is generated from this.

[How dsprrr works
→](https://jameshwade.github.io/dsprrr/articles/concepts-signatures-modules.md)

``` r

sig <- signature(
  "ticket -> urgency: enum('low', 'high'), team: string",
  instructions = "Route the support ticket."
)
```

### Modules

**Choose how it runs.** The same signature can be answered in one call,
with step-by-step reasoning first, or by a loop that calls R functions
as tools.

[Choose a module type
→](https://jameshwade.github.io/dsprrr/articles/advanced-modules.md)

``` r

route <- module(sig)            # one call
route <- chain_of_thought(sig)  # reason, then answer

lookup <- ellmer::tool(
  function(query) "Duplicate charges are refunded by billing.",
  description = "Look up support policy",
  arguments = list(query = ellmer::type_string())
)
route <- react(sig, tools = list(lookup))  # call tools as needed
```

### Metrics

**Measure it.** Run the module over labeled rows and score each
prediction. You get the mean score, per-row scores, and the predictions
to inspect.

[Metrics and evaluation
→](https://jameshwade.github.io/dsprrr/articles/concepts-why-metrics-matter.md)

``` r

tickets <- tibble::tibble(
  ticket = c(
    "I was charged twice for my order",
    "How do I change my profile photo?",
    "The app deletes my files when I sync",
    "Can I get last year's invoice?"
  ),
  urgency = c("high", "low", "high", "low"),
  team = c("billing", "account", "engineering", "billing")
)

scores <- evaluate(
  route,
  tickets,
  metric = metric_exact_match(field = "urgency"),
  .llm = chat
)
scores$mean_score
```

### Optimizers

**Improve it with data.**
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
returns a new module whose instructions or examples were chosen to raise
the metric on your training rows. Check the result on rows it did not
see.

[Compile and optimize
→](https://jameshwade.github.io/dsprrr/articles/compilation-optimization.md)

``` r

optimizer <- BootstrapFewShot(
  metric = metric_exact_match(field = "urgency"),
  max_bootstrapped_demos = 4L
)

split <- split_dataset(tickets, prop = 0.5, seed = 1)

optimized <- route |>
  compile(optimizer, trainset = split$train, .llm = chat)

evaluate(
  optimized,
  split$val,
  metric = metric_exact_match(field = "urgency"),
  .llm = chat
)
```

## Where to go next

| To | Read |
|----|----|
| Learn the basics one step at a time | [Tutorial 1: Your first LLM call](https://jameshwade.github.io/dsprrr/articles/tutorial-hello-world.md) |
| Pick a module for your task | [Choose a module type](https://jameshwade.github.io/dsprrr/articles/advanced-modules.md) |
| Measure quality with metrics | [Metrics and evaluation](https://jameshwade.github.io/dsprrr/articles/concepts-why-metrics-matter.md) |
| Improve a module with labeled data | [Compile and optimize](https://jameshwade.github.io/dsprrr/articles/compilation-optimization.md) |
| Decide which optimizer to use | [Choose an optimizer](https://jameshwade.github.io/dsprrr/articles/advanced-optimization.md) |
| Translate DSPy code to R | [dsprrr for DSPy users](https://jameshwade.github.io/dsprrr/articles/dspy-comparison.md) |
| Look up a function | [Reference](https://jameshwade.github.io/dsprrr/reference/index.md) |

Some features are experimental:
[`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md)
lets a model explore a large R object by writing code ([How RLM
works](https://jameshwade.github.io/dsprrr/articles/how-rlm-works.md)),
[`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md) lets
GEPA rewrite a whole program
([Flex](https://jameshwade.github.io/dsprrr/articles/flex-optimization.md)),
and [`Omni()`](https://jameshwade.github.io/dsprrr/reference/Omni.md)
and the agentic harnesses compose optimizers
([Omni](https://jameshwade.github.io/dsprrr/articles/omni-meta-optimization.md),
[agentic
harnesses](https://jameshwade.github.io/dsprrr/articles/agentic-optimization-harnesses.md)).

dsprrr is inspired by [DSPy](https://dspy.ai) from Stanford NLP. It
builds on [ellmer](https://ellmer.tidyverse.org) for model access and
bridges to [vitals](https://vitals.tidyverse.org) for evaluation.
