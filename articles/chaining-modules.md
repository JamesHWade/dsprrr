# Chain modules into pipelines

A pipeline runs modules in order and passes each step’s outputs to the
next step. This page shows how to build one, how data moves between
steps, and how to run, inspect and compile the result. A pipeline is
itself a module, so
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md),
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md),
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
and
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
work on it.

## How data moves between steps

Step 1 receives the inputs you pass to
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md). Every
later step receives only the previous step’s outputs, plus any static
inputs you attach to that step. Pipeline inputs are not forwarded past
step 1.

The example on this page turns release notes into a headline for
customers:

| Step | Module | Receives | Produces |
|----|----|----|----|
| 1 | `list_changes` | `notes`, the pipeline input | `changes` |
| 2 | `write_summary` | `changes` from step 1, and `audience`, a static input | `summary` |
| 3 | `write_headline` | `text`, which is step 2’s `summary` renamed | `headline` |

## Build the steps

Each step is an ordinary module that does one thing:

``` r

library(dsprrr)

list_changes <- module(signature(
  "notes -> changes: list[str]",
  instructions = "List each user-visible change as one short sentence."
))
write_summary <- module(signature(
  "changes, audience -> summary",
  instructions = "Summarize the changes in two sentences for the given audience."
))
write_headline <- module(signature(
  "text -> headline",
  instructions = "Write a headline of at most eight words for the text."
))
```

## Connect them with pipeline() and step()

[`pipeline()`](https://jameshwade.github.io/dsprrr/reference/pipeline.md)
takes modules in order. Wrap a module in
[`step()`](https://jameshwade.github.io/dsprrr/reference/step.md) when
it needs wiring:

``` r

release <- pipeline(
  list_changes,
  step(write_summary, audience = "customers"),
  step(write_headline, map = c(summary = "text"))
)
release
#> 
#> ── PipelineModule ──
#> 
#> ── Steps (3)
#> • [1] PredictModule: notes -> changes
#> • [2] PredictModule: changes, audience -> summary
#> • [3] PredictModule: text -> headline (map: summary -> text)
#> 
#> ── Composite Signature
#> 
#> ── Signature ──
#> 
#> ── Inputs
#> • notes: "string" - Input: notes
#> 
#> ── Output
#> Type: "object(headline: string)"
#> 
#> ── Instructions
#> List each user-visible change as one short sentence.
```

[`step()`](https://jameshwade.github.io/dsprrr/reference/step.md) takes
three kinds of wiring:

| Argument | Effect | Example |
|----|----|----|
| Named values | Static inputs, the same on every run. They win over an upstream field with the same name. | `audience = "customers"` |
| `map` | Renames fields from the previous step. Names are that step’s outputs, values are this step’s inputs. | `map = c(summary = "text")` |
| `select` | Keeps only the listed fields of this step’s outputs for the next step. | `select = "summary"` |

Run the pipeline like any module. The result holds the last step’s
outputs, here a list with one element, `headline`:

``` r

llm <- ellmer::chat_openai(model = "gpt-6-luna")
notes <- paste(
  "v2.3: Rewrote CSV export, now twice as fast.",
  "Added a dark mode.",
  "Fixed a bug that logged users out after five minutes."
)
run(release, notes = notes, .llm = llm)
```

## Chain with %\>\>%

When each module’s inputs are exactly the previous module’s outputs,
`%>>%` builds the same kind of pipeline with less typing:

``` r

brief <- list_changes %>>%
  module(signature("changes -> summary")) %>>%
  module(signature("summary -> headline"))
run(brief, notes = notes, .llm = llm)
```

`%>>%` accepts only modules, so use
[`pipeline()`](https://jameshwade.github.io/dsprrr/reference/pipeline.md)
when a step needs static inputs, `map` or `select`.

## Drop fields with select

Every output of a step moves on to the next one, and a Predict module
without a template writes every input it receives into its prompt.
[`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md)
adds a `reasoning` output, so without `select` the headline step below
would also read the summary’s reasoning:

``` r

careful <- pipeline(
  list_changes,
  step(chain_of_thought("changes -> summary"), select = "summary"),
  module(signature("summary -> headline"))
)
```

## When a later step needs an original input

Suppose step 1 extracts facts from a document and step 2 answers a
question about those facts. As a pipeline,
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) stops
with “Missing inputs for pipeline step 2”, because `question` went only
to step 1. If the value is the same on every run, attach it as a static
input with
[`step()`](https://jameshwade.github.io/dsprrr/reference/step.md).
Otherwise, write the flow as a function with
[`module_fn()`](https://jameshwade.github.io/dsprrr/reference/module_fn.md),
which passes the active chat to a `.llm` argument:

``` r

facts_from <- module(signature("document -> facts: list[str]"))
answer_from <- module(signature("facts: list[str], question -> answer"))

qa <- module_fn(
  "document, question -> answer",
  function(document, question, .llm) {
    facts <- run(facts_from, document = document, .llm = .llm)$facts
    run(answer_from, facts = facts, question = question, .llm = .llm)
  }
)
run(qa, document = notes, question = "Is there a dark mode now?", .llm = llm)
```

The trade-off is optimization:
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
cannot see inside a function, so it refuses
[`module_fn()`](https://jameshwade.github.io/dsprrr/reference/module_fn.md)
programs. Compile `facts_from` and `answer_from` on their own instead.

## Run many inputs

[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
runs the pipeline once per row and returns the input columns plus a
`result` list-column:

``` r

releases <- tibble::tibble(
  notes = c(notes, "v2.4: Added single sign-on. Search is faster.")
)
results <- run_dataset(release, releases, .llm = llm)
results$result
```

## See what each step did

The result keeps only the last step’s outputs. Each run also records
every step’s inputs and outputs in the pipeline’s traces:

``` r

run(release, notes = notes, .llm = llm)
latest <- release$state$traces[[length(release$state$traces)]]
latest$step_inputs
latest$step_outputs
```

A structured result reports usage for the whole run (`n_steps`,
`provider_calls`, `total_tokens`, `cost`) and keeps each step’s own
metadata, including the prompt it sent, in `step_metadata`:

``` r

result <- run(release, notes = notes, .llm = llm, .return_format = "structured")
result$metadata$total_tokens
cat(result$metadata$step_metadata[[2]]$prompt)
```

## Compile the whole pipeline

[`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md)
compiles a pipeline jointly: it runs the whole pipeline on each training
row and scores the final output with your metric. When a run passes,
every step’s inputs and outputs from that run become a demonstration for
that step, so only the final output needs a label:

``` r

past_releases <- tibble::tibble(
  notes = c(
    "v2.1: Added two-factor login. Fixed slow search on large projects.",
    "v2.2: New PDF export. Removed the legacy API."
  ),
  headline = c(
    "Two-factor login and faster search",
    "PDF export arrives, legacy API retires"
  )
)

compiled <- compile(
  release,
  BootstrapFewShot(
    metric = metric_f1(field = "headline"),
    metric_threshold = 0.5,
    max_bootstrapped_demos = 2L
  ),
  past_releases,
  .llm = llm
)
```

[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
returns a new pipeline and leaves `release` unchanged. A useful training
set has more than two rows; [Compile and
optimize](https://jameshwade.github.io/dsprrr/articles/compilation-optimization.md)
covers data, metrics and the other teleprompters.
