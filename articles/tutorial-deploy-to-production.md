# Tutorial 6: Save and reuse a module

Compiling a module, as in Tutorials 4 and 5, produces demos and
instructions that you do not want to recompute in every R session. By
the end of this tutorial you can save a compiled module’s configuration
to a [pins](https://pins.rstudio.com) board, restore it as a working
module, and export the traces the module records as it runs.

``` r

library(dsprrr)
library(ellmer)
library(pins)

chat <- chat_openai(model = "gpt-6-luna")
```

The answers on this page were recorded with `gpt-4.1` and are replayed
when the site is built, so `gpt-6-luna` may word them differently.

## Compile a module

This is the sentiment classifier from Tutorial 5, compiled with
`LabeledFewShot` on nine short reviews:

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

run(classifier, review = "This product is fantastic!", .llm = chat)
#> $sentiment
#> [1] "positive"
```

## Save the configuration

[`pin_module_config()`](https://jameshwade.github.io/dsprrr/reference/pin_module_config.md)
writes the module’s signature, demos, settings and optimization results
to a pins board. The board here is a temporary folder, which R deletes
when the session ends:

``` r

board <- board_temp()

pin_module_config(board, "sentiment-classifier", classifier)
#> Creating new version '20260926T220302Z-59990'
#> Writing to pin 'sentiment-classifier'
#> ✔ Pinned program artifact: "sentiment-classifier"
#> ℹ Root module: <PredictModule>
#> ℹ Graph nodes: 1
#> ℹ Compiled: TRUE
```

For real use, pick a board that outlives the session:

``` r

board <- board_local() # a folder in your user data directory
board <- board_folder("pins") # or a folder in your project
```

Shared boards such as
[`board_connect()`](https://pins.rstudio.com/reference/board_connect.html),
[`board_s3()`](https://pins.rstudio.com/reference/board_s3.html) and
[`board_gcs()`](https://pins.rstudio.com/reference/board_gcs.html) work
the same way and let other people load your module. Each call to
[`pin_module_config()`](https://jameshwade.github.io/dsprrr/reference/pin_module_config.md)
with the same name saves a new version:
`pin_versions(board, "sentiment-classifier")` lists them, and
`pin_read(board, "sentiment-classifier", version = )` reads an older
one.

## Restore it

In a new session, connect to the same board, read the pin and rebuild
the module:

``` r

config <- pin_read(board, "sentiment-classifier")
restored <- restore_module_config(config)
#> ✔ Restored program artifact
#> ℹ Root module: <PredictModule>
#> ℹ Artifact version: 6

identical(restored$demos, classifier$demos)
#> [1] TRUE
```

The chat is not part of the saved configuration, so pass `.llm` when you
run the restored module:

``` r

run(restored, review = "Worst product ever!", .llm = chat)
```

[`restore_module_config()`](https://jameshwade.github.io/dsprrr/reference/restore_module_config.md)
reads only the artifact format of the installed dsprrr (version 6 in the
message above), so keep the code that compiled the module: if a dsprrr
upgrade changes the format, compile and pin again. A module that holds R
functions or tools also needs a `registry` when you save and restore it;
see
[`pin_module_config()`](https://jameshwade.github.io/dsprrr/reference/pin_module_config.md).
To use a saved module in a targets pipeline or a Quarto report, see [Run
dsprrr in
pipelines](https://jameshwade.github.io/dsprrr/articles/orchestration.md).

## Save and inspect traces

A module records a trace for each call it makes, with token counts,
latency, cost and model. Run the module a few times, then pin its traces
next to the configuration:

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

pin_trace(board, "sentiment-traces", classifier)
#> Creating new version '20260926T220304Z-5a2d5'
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
#> 1 1790460183.       860.          101                   0             7
#> 2 1790460184.       423.          200                   0             7
#> 3 1790460184.       376.          271                   0             7
#> 4 1790460185.       411.          341                   0             7
#> # ℹ 6 more variables: total_tokens <int>, cost <dbl>, model <chr>,
#> #   prompt_length <int>, program_artifact_id <chr>, trace_context <list>
```

The tibble leaves out prompts and responses, so you can share it without
exposing the text your module handled. Add them with
`include_prompts = TRUE` and `include_outputs = TRUE`;
[`pin_trace()`](https://jameshwade.github.io/dsprrr/reference/pin_trace.md)
takes the same two arguments. `summarize_traces(classifier)` totals the
tokens, cost and latency.

That completes the tutorials; [Compile and
optimize](https://jameshwade.github.io/dsprrr/articles/compilation-optimization.md)
and [Choose an
optimizer](https://jameshwade.github.io/dsprrr/articles/advanced-optimization.md)
take optimization further.
