# Labeled few-shot: add training rows as demonstrations

`LabeledFewShot()` compiles a Predict module by attaching `k` rows of
the training set as few-shot demonstrations. It makes no model calls and
scores nothing, which makes it a fast, offline baseline for the other
optimizers.

## Usage

``` r
LabeledFewShot(
  metric = NULL,
  metric_threshold = NULL,
  max_errors = 5L,
  k = 4L,
  sample = TRUE,
  seed = 123L
)
```

## Arguments

- metric:

  Optional. It is not used for scoring, but when it has a `field` (as in
  `metric_exact_match(field = "sentiment")`), that column supplies the
  demonstrations' outputs.

- metric_threshold, max_errors:

  Accepted for consistency with the other optimizers (see
  [`Teleprompter()`](https://jameshwade.github.io/dsprrr/reference/Teleprompter.md));
  `LabeledFewShot()` does not use them.

- k:

  Integer number of demonstrations (default `4L`). When the training set
  has fewer rows, all of them are used.

- sample:

  If `TRUE` (the default), draw `k` rows at random. If `FALSE`, take the
  first `k` rows.

- seed:

  Integer seed for the random draw (default `123L`). It is passed to
  [`set.seed()`](https://rdrr.io/r/base/Random.html), which also resets
  the session's random number stream.

## Value

A `LabeledFewShot` object to pass to
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md).

## Details

Rows are drawn at random (or the first `k` rows are taken when
`sample = FALSE`); nothing checks whether they are good examples. A
demonstration's inputs come from the columns named after the signature
inputs. Its output comes from the metric's `field` when the metric has
one, otherwise from the first column named `output`, `label`, `answer`,
`response`, `result` or `y`, otherwise from the first non-input column.

The program must be a Predict module, such as one from
[`module()`](https://jameshwade.github.io/dsprrr/reference/module.md) or
[`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md).
Other programs, including pipelines and RLM modules, are rejected
because training rows do not match the signatures of their inner
predictors.
[`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md)
compiles pipelines.

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
[`MIPROv2()`](https://jameshwade.github.io/dsprrr/reference/MIPROv2.md),
[`MetaHarness()`](https://jameshwade.github.io/dsprrr/reference/MetaHarness.md),
[`Omni()`](https://jameshwade.github.io/dsprrr/reference/Omni.md),
[`ReAnchor()`](https://jameshwade.github.io/dsprrr/reference/ReAnchor.md),
[`SIMBA()`](https://jameshwade.github.io/dsprrr/reference/SIMBA.md),
[`Teleprompter()`](https://jameshwade.github.io/dsprrr/reference/Teleprompter.md),
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)

## Examples

``` r
classifier <- module(signature("text -> sentiment"))
trainset <- data.frame(
  text = c("I love it!", "Terrible experience", "It's okay", "Works well"),
  sentiment = c("positive", "negative", "neutral", "positive")
)

tp <- LabeledFewShot(k = 2L)
tp
#> <dsprrr::LabeledFewShot>
#>  @ metric          : NULL
#>  @ metric_threshold: NULL
#>  @ max_errors      : int 5
#>  @ k               : int 2
#>  @ sample          : logi TRUE
#>  @ seed            : int 123

# Compiling makes no model calls and leaves `classifier` unchanged
compiled <- compile(classifier, tp, trainset)
best_demos(compiled, as_tibble = TRUE)
#> # A tibble: 2 × 2
#>   text       output  
#>   <chr>      <chr>   
#> 1 It's okay  neutral 
#> 2 Works well positive
```
