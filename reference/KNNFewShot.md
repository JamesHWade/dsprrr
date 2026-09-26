# KNN few-shot: choose demonstrations by similarity at run time

`KNNFewShot()` gives every input its own demonstrations: the `k`
training rows whose embeddings are most similar to it. Compiling embeds
the training set once, with no model calls; each run then embeds the
input and picks its nearest rows.
[`LabeledFewShot()`](https://jameshwade.github.io/dsprrr/reference/LabeledFewShot.md),
by contrast, attaches one fixed set.

## Usage

``` r
KNNFewShot(
  metric = NULL,
  metric_threshold = NULL,
  max_errors = 5L,
  k = 3L,
  vectorizer = function() NULL,
  input_text = NULL,
  cache_embeddings = TRUE,
  merge_demos = FALSE
)
```

## Arguments

- metric:

  Optional. It is not used for scoring, but when it has a `field`, that
  column supplies the demonstrations' outputs.

- metric_threshold, max_errors:

  Accepted for consistency with the other optimizers (see
  [`Teleprompter()`](https://jameshwade.github.io/dsprrr/reference/Teleprompter.md));
  `KNNFewShot()` does not use them.

- k:

  Integer number of neighbors used as demonstrations (default `3L`).

- vectorizer:

  Required. A function that takes a character vector and returns a
  numeric matrix of embeddings, one row per string.

- input_text:

  Optional function that turns a one-row data frame of inputs into the
  text to embed. The default pastes the signature's input columns
  together, separated by spaces.

- cache_embeddings:

  Currently has no effect: the training set is always embedded once, at
  compile time.

- merge_demos:

  If `TRUE`, keep the module's existing demonstrations and add the
  selected ones after them. If `FALSE` (the default), the selected rows
  replace them.

## Value

A `KNNFewShot` object to pass to
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md).

## Details

`vectorizer` turns a character vector into a numeric matrix with one row
per string. For real embeddings, wrap a provider, for example
`function(x) ragnar::embed_openai(x, model = "text-embedding-3-small")`.
Similarity is cosine similarity.

[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
returns a wrapper module. Each run records the chosen rows and their
similarity scores in `compiled$state$demo_selections`. Demonstration
outputs come from the metric's `field` when there is one, and otherwise
from an automatically detected label column, as in
[`LabeledFewShot()`](https://jameshwade.github.io/dsprrr/reference/LabeledFewShot.md).

## See also

Other teleprompters:
[`AutoResearch()`](https://jameshwade.github.io/dsprrr/reference/AutoResearch.md),
[`BetterTogether()`](https://jameshwade.github.io/dsprrr/reference/BetterTogether.md),
[`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md),
[`BootstrapFewShotWithRandomSearch()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShotWithRandomSearch.md),
[`COPRO()`](https://jameshwade.github.io/dsprrr/reference/COPRO.md),
[`GEPA()`](https://jameshwade.github.io/dsprrr/reference/GEPA.md),
[`GridSearchTeleprompter()`](https://jameshwade.github.io/dsprrr/reference/GridSearchTeleprompter.md),
[`LabeledFewShot()`](https://jameshwade.github.io/dsprrr/reference/LabeledFewShot.md),
[`MIPROv2()`](https://jameshwade.github.io/dsprrr/reference/MIPROv2.md),
[`MetaHarness()`](https://jameshwade.github.io/dsprrr/reference/MetaHarness.md),
[`Omni()`](https://jameshwade.github.io/dsprrr/reference/Omni.md),
[`ReAnchor()`](https://jameshwade.github.io/dsprrr/reference/ReAnchor.md),
[`SIMBA()`](https://jameshwade.github.io/dsprrr/reference/SIMBA.md),
[`Teleprompter()`](https://jameshwade.github.io/dsprrr/reference/Teleprompter.md),
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)

## Examples

``` r
# A toy vectorizer that counts letters. Use real embeddings in practice.
letter_counts <- function(texts) {
  t(vapply(
    tolower(texts),
    function(x) tabulate(utf8ToInt(x) - 96L, nbins = 26L),
    integer(26)
  ))
}

tp <- KNNFewShot(k = 2L, vectorizer = letter_counts)
tp
#> <dsprrr::KNNFewShot>
#>  @ metric          : NULL
#>  @ metric_threshold: NULL
#>  @ max_errors      : int 5
#>  @ k               : int 2
#>  @ vectorizer      : function (texts)  
#>  @ input_text      : NULL
#>  @ cache_embeddings: logi TRUE
#>  @ merge_demos     : logi FALSE

qa <- module(signature("question -> answer"))
trainset <- data.frame(
  question = c(
    "What is 2 + 2?", "Capital of France?",
    "What is 10 / 5?", "Capital of Peru?"
  ),
  answer = c("4", "Paris", "2", "Lima")
)
# Compiling embeds the training rows; no model is called
compiled <- compile(qa, tp, trainset)
#> Computing embeddings for 4 training examples...

if (FALSE) { # \dontrun{
run(
  compiled,
  question = "Capital of Chile?",
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)
compiled$state$demo_selections
} # }
```
