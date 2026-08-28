# Teleprompter Base Class

Base S7 class for optimization strategies (teleprompters). Teleprompters
are responsible for optimizing modules by adjusting their prompts,
demonstrations, or other parameters based on training data.

## Usage

``` r
Teleprompter(metric = NULL, metric_threshold = NULL, max_errors = 5L)
```

## Arguments

- metric:

  Optional metric function for evaluating predictions. Subclasses that
  evaluate candidates document whether a metric is required.

- metric_threshold:

  Minimum score required to be considered successful. If NULL, uses the
  metric's default threshold.

- max_errors:

  Maximum number of errors allowed during optimization. Default is 5.

## Value

A `Teleprompter` optimization strategy object.

## Examples

``` r
Teleprompter()
#> <dsprrr::Teleprompter>
#>  @ metric          : NULL
#>  @ metric_threshold: NULL
#>  @ max_errors      : int 5
```
