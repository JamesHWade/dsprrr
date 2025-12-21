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

  A metric function for evaluating predictions. If NULL, uses
  exact_match() by default.

- metric_threshold:

  Minimum score required to be considered successful. If NULL, uses the
  metric's default threshold.

- max_errors:

  Maximum number of errors allowed during optimization. Default is 5.
