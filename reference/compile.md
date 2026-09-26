# Optimize a program with a teleprompter

`compile()` improves a program (its demos, instructions or other
settings) with a teleprompter such as
[`LabeledFewShot()`](https://jameshwade.github.io/dsprrr/reference/LabeledFewShot.md),
[`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md)
or
[`MIPROv2()`](https://jameshwade.github.io/dsprrr/reference/MIPROv2.md),
using a training set. It returns a new program and leaves the input
unchanged. Its argument order suits the native pipe:
`program |> compile(teleprompter, trainset)`.

## Usage

``` r
compile(program, teleprompter, ...)
```

## Arguments

- program:

  The module or pipeline to optimize. Programs made with
  [`module_fn()`](https://jameshwade.github.io/dsprrr/reference/module_fn.md)
  cannot be compiled.

- teleprompter:

  A teleprompter object that sets the optimization strategy. Integer
  settings of teleprompters need integer literals, as in
  `LabeledFewShot(k = 3L)`; `k = 3` is an error.

- ...:

  The training set and optimizer options:

  - `trainset` (required, third argument): a data frame with the
    program's input columns and the expected outputs.

  - `valset`: an optional validation data frame, for teleprompters that
    use one. It may also be given as the fourth argument.

  - `.llm`: an ellmer Chat for the program's calls (see
    [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md)).

  - `.trace_context`: a named, JSON-compatible list copied into the
    metadata and traces of the calls made while compiling.

  Some teleprompters take further arguments; see their help pages.

## Value

A new, compiled program of the same kind as `program`. Check it with
`compiled$is_compiled()`.

## Details

Teleprompters that score candidates call the metric as
`metric(prediction, expected)`, where `expected` is the whole training
row, so give built-in metrics a `field`, as in
`metric_exact_match(field = "sentiment")`. Compiling a program that is
already compiled works but gives a warning.

## See also

Other teleprompters:
[`AutoResearch()`](https://jameshwade.github.io/dsprrr/reference/AutoResearch.md),
[`BetterTogether()`](https://jameshwade.github.io/dsprrr/reference/BetterTogether.md),
[`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md),
[`BootstrapFewShotWithRandomSearch()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShotWithRandomSearch.md),
[`COPRO()`](https://jameshwade.github.io/dsprrr/reference/COPRO.md),
[`GEPA()`](https://jameshwade.github.io/dsprrr/reference/GEPA.md),
[`GridSearchTeleprompter()`](https://jameshwade.github.io/dsprrr/reference/GridSearchTeleprompter.md),
[`KNNFewShot()`](https://jameshwade.github.io/dsprrr/reference/KNNFewShot.md),
[`LabeledFewShot()`](https://jameshwade.github.io/dsprrr/reference/LabeledFewShot.md),
[`MIPROv2()`](https://jameshwade.github.io/dsprrr/reference/MIPROv2.md),
[`MetaHarness()`](https://jameshwade.github.io/dsprrr/reference/MetaHarness.md),
[`Omni()`](https://jameshwade.github.io/dsprrr/reference/Omni.md),
[`ReAnchor()`](https://jameshwade.github.io/dsprrr/reference/ReAnchor.md),
[`SIMBA()`](https://jameshwade.github.io/dsprrr/reference/SIMBA.md),
[`Teleprompter()`](https://jameshwade.github.io/dsprrr/reference/Teleprompter.md)

## Examples

``` r
classifier <- module(
  signature("text -> sentiment: enum('positive', 'negative')")
)
trainset <- data.frame(
  text = c("I love it!", "Terrible experience", "Works great", "Broke in a day"),
  sentiment = c("positive", "negative", "positive", "negative")
)

# LabeledFewShot copies training rows into the prompt as demos, so it
# needs no model calls
compiled <- classifier |> compile(LabeledFewShot(k = 2L), trainset)
compiled$is_compiled()
#> [1] TRUE
classifier$is_compiled()
#> [1] FALSE
compiled$demo_table
#> # A tibble: 2 × 2
#>   text           output  
#>   <chr>          <chr>   
#> 1 Works great    positive
#> 2 Broke in a day negative

if (FALSE) { # \dontrun{
# BootstrapFewShot runs the program and keeps demos that pass the metric
bootstrapped <- classifier |>
  compile(
    BootstrapFewShot(
      metric = metric_exact_match(field = "sentiment"),
      max_bootstrapped_demos = 2L
    ),
    trainset,
    .llm = ellmer::chat_openai(model = "gpt-6-luna")
  )
} # }
```
