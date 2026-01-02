# Best by Metric Reducer

Creates a reduce function that scores each output using a metric
function and returns the best-scoring output. Requires expected value to
be set via the `set_expected` attribute before calling.

## Usage

``` r
reduce_best_by_metric(metric, maximize = TRUE)
```

## Arguments

- metric:

  A metric function created with `metric_*()` functions

- maximize:

  If TRUE (default), return highest-scoring output. If FALSE, return
  lowest-scoring output.

## Value

A reduce function for use with
[`ensemble()`](https://jameshwade.github.io/dsprrr/reference/ensemble_module.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Score each output and return best
ens <- ensemble(
  modules,
  reduce_fn = reduce_best_by_metric(
    metric = metric_exact_match(field = "answer")
  )
)
} # }
```
