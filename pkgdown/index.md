# dsprrr <img src="man/figures/logo.png" align="right" width="120" alt="dsprrr hex sticker" />

dsprrr lets you write LLM features in R as small programs instead of prompt
strings. You declare a task's inputs and typed outputs; dsprrr builds the
prompt, calls the model through [ellmer](https://ellmer.tidyverse.org), and
returns an R list with the types you asked for. Once you have labeled examples,
you can score the program with a metric and let an optimizer tune its
instructions and few-shot examples against that score.

If you have a prompt that already works and no data to measure it against,
plain ellmer is enough. dsprrr earns its keep when you want typed outputs
across many inputs, a score you can track, or a prompt tuned on examples
instead of by hand. The design follows [DSPy](https://dspy.ai), and any
provider ellmer supports works.

## Installation

Install the released version from CRAN:

```r
install.packages("dsprrr")
```

Or install the development version from GitHub:

```r
# install.packages("pak")
pak::pak("JamesHWade/dsprrr")
```

Installing from source requires a C compiler (Rtools on Windows, Xcode Command
Line Tools on macOS, or your Linux distribution's build tools); the compiled
helper preserves exact filesystem identities for safe persistence.

You also need credentials for a model provider, for example `OPENAI_API_KEY`
in your `.Renviron`.

## Example

A signature names the inputs and outputs of a task. `module()` turns it into
something you can run.

```r
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

`stars` comes back as an integer and `sentiment` is always one of the three
labels, so the result can go straight into a data frame. The output above was
recorded from a real call to `gpt-4.1` in
[Tutorial 3](articles/tutorial-structured-outputs.html); `gpt-6-luna` may word
the summary differently.

## How the pieces fit

<div class="row align-items-center g-4 my-4 primitive-row">
<div class="col-md-5">

### Signatures

**Declare the task.** Inputs on the left of the arrow, typed outputs on the
right, and optional instructions. The prompt is generated from this.

[How dsprrr works &rarr;](articles/concepts-signatures-modules.html)

</div>
<div class="col-md-7">

```r
sig <- signature(
  "ticket -> urgency: enum('low', 'high'), team: string",
  instructions = "Route the support ticket."
)
```

</div>
</div>

<div class="row align-items-center g-4 my-4 primitive-row">
<div class="col-md-5">

### Modules

**Choose how it runs.** The same signature can be answered in one call, with
step-by-step reasoning first, or by a loop that calls R functions as tools.

[Choose a module type &rarr;](articles/advanced-modules.html)

</div>
<div class="col-md-7">

```r
route <- module(sig)                # one call
route_cot <- chain_of_thought(sig)  # reason, then answer

lookup <- ellmer::tool(
  function(query) "Duplicate charges are refunded by billing.",
  description = "Look up support policy",
  arguments = list(query = ellmer::type_string())
)
route_agent <- react(sig, tools = list(lookup))  # call tools as needed
```

</div>
</div>

<div class="row align-items-center g-4 my-4 primitive-row">
<div class="col-md-5">

### Metrics

**Measure it.** Run the module over labeled rows and score each prediction.
You get the mean score, per-row scores, and the predictions to inspect.

[Metrics and evaluation &rarr;](articles/concepts-why-metrics-matter.html)

</div>
<div class="col-md-7">

```r
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

</div>
</div>

<div class="row align-items-center g-4 my-4 primitive-row">
<div class="col-md-5">

### Optimizers

**Improve it with data.** `compile()` returns a new module whose examples or
instructions were tuned to raise the metric on your training rows. Check the
result on rows it did not see.

[Compile and optimize &rarr;](articles/compilation-optimization.html)

</div>
<div class="col-md-7">

```r
optimizer <- BootstrapFewShot(
  metric = metric_exact_match(field = "urgency"),
  max_bootstrapped_demos = 2L,
  max_labeled_demos = 0L
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

</div>
</div>

## Where to go next

| To | Read |
|---|---|
| Learn the basics one step at a time | [Tutorial 1: Your first LLM call](articles/tutorial-hello-world.html) |
| Pick a module for your task | [Choose a module type](articles/advanced-modules.html) |
| Measure quality with metrics | [Metrics and evaluation](articles/concepts-why-metrics-matter.html) |
| Improve a module with labeled data | [Compile and optimize](articles/compilation-optimization.html) |
| Decide which optimizer to use | [Choose an optimizer](articles/advanced-optimization.html) |
| Translate DSPy code to R | [dsprrr for DSPy users](articles/dspy-comparison.html) |
| Look up a function | [Reference](reference/index.html) |

Some features are experimental: `rlm_module()` lets a model explore a large R
object by writing code ([How RLM works](articles/how-rlm-works.html)), `flex()`
lets GEPA rewrite a whole program ([Flex](articles/flex-optimization.html)),
`with_decisions()` and `ReAnchor()` calibrate probability-backed decision
thresholds ([Calibrated decisions](articles/calibrated-decisions.html)), and
`Omni()` and the agentic harnesses compose optimizers
([Omni](articles/omni-meta-optimization.html),
[agentic harnesses](articles/agentic-optimization-harnesses.html)).

dsprrr is experimental; the API may change. It is inspired by
[DSPy](https://dspy.ai) from Stanford NLP, builds on
[ellmer](https://ellmer.tidyverse.org) for model access, and bridges to
[vitals](https://vitals.tidyverse.org) for evaluation.
