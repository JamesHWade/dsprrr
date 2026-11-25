# Tutorial 6: Save and reuse a module

Compiling a module produces demos, instructions, and tuned settings that
you do not want to recompute every time R restarts. This tutorial shows
how to save a compiled module to disk with
[`save_program()`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md),
share versioned copies on a [pins](https://pins.rstudio.com) board,
restore it cleanly in a new session, and inspect the traces it records
as it runs.

``` r

library(dsprrr)
library(ellmer)

chat <- chat_openai(model = "gpt-6-luna")
```

*Recorded outputs on the package website were captured with `gpt-4.1`;
`gpt-6-luna` may answer slightly differently.*

## Compile a module

Start from the sentiment classifier in Tutorial 5, compiled offline with
[`LabeledFewShot()`](https://jameshwade.github.io/dsprrr/reference/LabeledFewShot.md):

``` r

sig <- signature(
  "review -> sentiment: enum('positive', 'negative', 'neutral')",
  instructions = "Classify the sentiment of this product review."
)

trainset <- tibble::tribble(
  ~review,            ~sentiment,
  "Love it!",         "positive",
  "Hate it!",         "negative",
  "It's okay.",       "neutral",
  "Amazing!",         "positive",
  "Terrible!",        "negative",
  "Meh.",             "neutral",
  "Best ever!",       "positive",
  "Worst purchase!",  "negative",
  "Fine I guess.",    "neutral"
)

classifier <- module(sig) |> compile(LabeledFewShot(k = 3L), trainset)
```

Verify that the compiled classifier runs:

``` r

run(classifier, review = "This product is fantastic!", .llm = chat)
#> $sentiment
#> [1] "positive"
```

## Save to a file

The simplest persistence path needs no extra packages:
[`save_program()`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md)
writes a validated, versioned program artifact (signatures, demos,
config, and optimization state) to an `.rds` file, and
[`load_program()`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md)
rebuilds it in any later session.
[`program_artifact_id()`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md)
returns its SHA-256 content identity:

``` r

artifact_path <- tempfile(fileext = ".rds")
save_program(classifier, artifact_path)

loaded <- load_program(artifact_path)
identical(loaded$demos, classifier$demos)
#> [1] TRUE
identical(program_artifact_id(loaded), program_artifact_id(classifier))
#> [1] TRUE
```

The chat is intentionally not stored in the artifact—credentials never
touch disk—so pass `.llm` (or configure a default chat) when you run the
restored module:

``` r

run(loaded, review = "Worst product ever!", .llm = chat)
#> $sentiment
#> [1] "negative"
```

## Share versions with `pins`

When you want version history or shared team storage (such as S3, Google
Cloud Storage, or Posit Connect),
[`pin_module_config()`](https://jameshwade.github.io/dsprrr/reference/pin_module_config.md)
writes the same program artifact to a `pins` board. The board below is a
temporary folder that R deletes when the session ends:

``` r

board <- pins::board_temp()

pin_module_config(board, "sentiment-classifier", classifier)
#> Creating new version '20261125T120041Z-4f092'
#> Writing to pin 'sentiment-classifier'
#> ✔ Pinned program artifact: "sentiment-classifier"
#> ℹ Root module: <PredictModule>
#> ℹ Graph nodes: 1
#> ℹ Compiled: TRUE
```

For real use, pick a board that outlives the session:

``` r

board <- pins::board_local()   # a folder in your user data directory
board <- pins::board_folder("pins") # or a folder in your project
```

Shared boards such as
[`pins::board_connect()`](https://pins.rstudio.com/reference/board_connect.html),
[`pins::board_s3()`](https://pins.rstudio.com/reference/board_s3.html),
and
[`pins::board_gcs()`](https://pins.rstudio.com/reference/board_gcs.html)
work the same way. Each call to
[`pin_module_config()`](https://jameshwade.github.io/dsprrr/reference/pin_module_config.md)
with the same name saves a new version:
`pins::pin_versions(board, "sentiment-classifier")` lists them, and
`pins::pin_read(board, "sentiment-classifier", version = "<id>")` reads
an older one.

In a new session, connect to the same board, read the pin, and rebuild
the module:

``` r

config <- pins::pin_read(board, "sentiment-classifier")
restored <- restore_module_config(config)
#> ✔ Restored program artifact
#> ℹ Root module: <PredictModule>
#> ℹ Artifact version: 6

identical(restored$demos, classifier$demos)
#> [1] TRUE
```

[`restore_module_config()`](https://jameshwade.github.io/dsprrr/reference/restore_module_config.md)
and
[`load_program()`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md)
read the current artifact format (version 6), so keep the code that
compiled the module: if a future dsprrr release updates the format,
compile and save again. A module that holds R functions or tools also
needs a `registry` when you save and restore it; see
[`?program_artifact`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md).
For automated `targets` pipelines, Quarto reports, and approval
workflows, see [Run dsprrr in
pipelines](https://jameshwade.github.io/dsprrr/articles/orchestration.md).

## Save and inspect traces

A module records a trace for each call it makes, with token counts,
latency, cost, and model. Run the module a few times, then pin its
traces next to the configuration:

``` r

run(classifier, review = "Great product!", .llm = chat)
#> $sentiment
#> [1] "positive"
run(classifier, review = "Not worth the money.", .llm = chat)
#> $sentiment
#> [1] "negative"
run(classifier, review = "Does the job.", .llm = chat)
#> $sentiment
#> [1] "neutral"

if (has_pins) {
  pin_trace(board, "sentiment-traces", classifier)
}
#> Creating new version '20261125T120042Z-28c1c'
#> Writing to pin 'sentiment-traces'
#> ✔ Pinned 4 traces: "sentiment-traces"
#> ℹ Total tokens: 941
```

[`export_traces()`](https://jameshwade.github.io/dsprrr/reference/export_traces.md)
returns the same data as a tibble with one row per call, including the
call made right after compiling:

``` r

export_traces(classifier)
#> # A tibble: 4 × 11
#>     timestamp latency_ms input_tokens cached_input_tokens output_tokens
#>         <dbl>      <dbl>        <int>               <int>         <int>
#> 1 1795608041.       789.          101                   0             7
#> 2 1795608042.       308.          200                   0             7
#> 3 1795608042.       260.          271                   0             7
#> 4 1795608043.       269.          341                   0             7
#> # ℹ 6 more variables: total_tokens <int>, cost <dbl>, model <chr>,
#> #   prompt_length <int>, program_artifact_id <chr>, trace_context <list>
```

The tibble leaves out prompts and responses by default, so you can share
token and latency logs without exposing user text. Pass
`include_prompts = TRUE` or `include_outputs = TRUE` (to
[`export_traces()`](https://jameshwade.github.io/dsprrr/reference/export_traces.md)
or
[`pin_trace()`](https://jameshwade.github.io/dsprrr/reference/pin_trace.md))
when you need them. For a quick rollup of calls, tokens, and spend:

``` r

summarize_traces(classifier)
#> 
#> ── Module Trace Summary ──
#> 
#> ── Usage Metrics
#> • 4 requests
#> • 941 total tokens
#> • Input/Output ratio: 0.03
#> • Total cost: $0.0001053
#> 
#> ── Performance
#> • Average latency: 406.5ms
#> • Total time: 1.63s
#> • Tokens/request: 235.2
#> 
#> ── Models Used
#> gpt-6-luna: 4 requests
```

## Where to go next

Across these six tutorials you have walked the full dsprrr loop: declare
a
[`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md),
wrap it in
[`module()`](https://jameshwade.github.io/dsprrr/reference/module.md),
run it on vectors and data frames, score it with
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md),
improve it with
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
or
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md),
and save the result for reuse. From here:

- **Need reasoning, tools, or multi-step workflows?** Read [Choose a
  module
  type](https://jameshwade.github.io/dsprrr/articles/advanced-modules.md)
  and [Chain modules into
  pipelines](https://jameshwade.github.io/dsprrr/articles/chaining-modules.md).
- **Want stronger optimizers than grid search?** Read [Compile and
  optimize](https://jameshwade.github.io/dsprrr/articles/compilation-optimization.md)
  and [Choose an
  optimizer](https://jameshwade.github.io/dsprrr/articles/advanced-optimization.md).
- **Running dsprrr inside `targets` or Quarto?** Read [Run dsprrr in
  pipelines](https://jameshwade.github.io/dsprrr/articles/orchestration.md).
