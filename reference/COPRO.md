# COPRO Teleprompter

COPRO (Coordinate Prompt Optimization) automatically refines module
instructions using coordinate ascent. At each iteration, it generates
multiple candidate instruction variants and selects the best performing
one as the baseline for the next iteration.

The optimizer:

1.  Starts with current module instructions as baseline

2.  For each depth iteration:

    - Generates `breadth` candidate instruction variants using the
      prompt_model

    - Evaluates each candidate on the validation set

    - Selects the best performing instruction

    - Uses the best as baseline for the next iteration

3.  Returns module with optimized instructions

## Usage

``` r
COPRO(
  metric = NULL,
  metric_threshold = NULL,
  max_errors = 5L,
  prompt_model = NULL,
  breadth = 10L,
  depth = 3L,
  init_temperature = 1.4,
  track_stats = TRUE,
  seed = 0L,
  log_dir = NULL
)
```

## Arguments

- metric:

  A metric function for evaluating predictions (required).

- metric_threshold:

  Minimum score required to be considered successful. If NULL, uses the
  metric's default threshold.

- max_errors:

  Maximum number of errors allowed during optimization. Default is 5.

- prompt_model:

  Optional ellmer Chat for generating instruction candidates. If `NULL`,
  uses the task Chat supplied through `.llm`.

- breadth:

  Number of instruction candidates to generate per iteration. Default is
  10.

- depth:

  Number of coordinate ascent iterations. Default is 3.

- init_temperature:

  Temperature for instruction generation. Default is 1.4.

- track_stats:

  Whether to track instruction history and scores. Default is TRUE.

- seed:

  Random seed for reproducibility. Default is 0.

- log_dir:

  Directory for trial logging. Default is NULL.

## Examples

``` r
if (FALSE) { # \dontrun{
tp <- COPRO(
  metric = metric_exact_match(field = "answer"),
  prompt_model = ellmer::chat_openai(),
  breadth = 10L,
  depth = 3L
)

compiled <- compile(
  tp, qa_module, trainset,
  valset = valset, .llm = task_llm
)

# Access instruction history
compiled$config$optimizer$history
} # }
```
