# BootstrapFewShot Teleprompter

A teleprompter that bootstraps demonstrations by having a teacher model
generate predictions on training examples and selecting successful ones
as demonstrations. This is DSPy's foundational optimization approach.

The optimizer:

1.  Starts with optional labeled demonstrations from the training set

2.  Uses a teacher model to generate predictions on remaining examples

3.  Evaluates predictions using the provided metric

4.  Selects top-scoring predictions as bootstrapped demonstrations

5.  Optionally runs multiple rounds, updating the teacher with new demos

## Usage

``` r
BootstrapFewShot(
  metric = NULL,
  metric_threshold = NULL,
  max_errors = 5L,
  max_bootstrapped_demos = 4L,
  max_labeled_demos = 16L,
  max_rounds = 1L,
  teacher_settings = NULL,
  seed = NULL,
  log_dir = NULL
)
```

## Arguments

- metric:

  A metric function for evaluating predictions (required).

- metric_threshold:

  Minimum score for a demo to be accepted. If NULL, accepts any
  successful prediction. Default is NULL.

- max_errors:

  Maximum number of errors allowed during optimization. Default is 5.

- max_bootstrapped_demos:

  Maximum number of bootstrapped demonstrations to include. Default is
  4.

- max_labeled_demos:

  Maximum number of labeled demonstrations from the training set.
  Default is 16.

- max_rounds:

  Number of bootstrap rounds to perform. Default is 1.

- teacher_settings:

  List of settings for the teacher model, such as `temperature` or
  `model`. If NULL, defaults to `list(temperature = 0.7)`.

- seed:

  Random seed for reproducibility. Default is NULL.

- log_dir:

  Directory for trial logging. Default is NULL.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a BootstrapFewShot teleprompter
tp <- BootstrapFewShot(
  metric = metric_exact_match(field = "answer"),
  max_bootstrapped_demos = 4L,
  max_labeled_demos = 8L
)

# Compile a module
compiled <- compile(tp, qa_module, trainset, .llm = llm)
} # }
```
