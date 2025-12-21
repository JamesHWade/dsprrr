# GridSearchTeleprompter

A teleprompter that performs grid search over different instruction and
template variants to find the best performing configuration.

## Usage

``` r
GridSearchTeleprompter(
  metric = NULL,
  metric_threshold = NULL,
  max_errors = 5L,
  variants = structure(list(id = 1L, instructions = NA_character_, template =
    NA_character_), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -1L)),
  k = 2L,
  eval_sample_size = 50L,
  verbose = TRUE
)
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

- variants:

  A data frame containing variant configurations to test. Must have an
  'id' column. Other columns define parameter values. Default is a
  tibble with one row containing NA values for instructions and
  template.

- k:

  Number of examples to include in few-shot prompts. Default is 2.

- eval_sample_size:

  Number of examples to use for evaluation during grid search. Default
  is 50.

- verbose:

  Whether to print progress messages. Default is TRUE.
