# MIPROv2 Teleprompter

MIPROv2 jointly optimizes instructions and few-shot demonstrations using
a discrete Bayesian optimization loop with minibatch evaluation.

## Usage

``` r
MIPROv2(
  metric = NULL,
  metric_threshold = NULL,
  max_errors = 5L,
  prompt_model = NULL,
  task_model = NULL,
  teacher_settings = NULL,
  max_bootstrapped_demos = 4L,
  max_labeled_demos = 4L,
  auto = "light",
  num_candidates = NULL,
  num_threads = 1L,
  seed = 9L,
  init_temperature = 1,
  track_stats = TRUE,
  log_dir = NULL
)
```

## Arguments

- metric:

  A metric function for evaluating predictions (required).

- metric_threshold:

  Minimum score required for acceptance.

- max_errors:

  Maximum number of errors allowed during optimization.

- prompt_model:

  Optional model to propose instructions.

- task_model:

  Optional model to evaluate tasks. Defaults to .llm.

- teacher_settings:

  List of settings for the teacher model.

- max_bootstrapped_demos:

  Maximum number of bootstrapped demonstrations.

- max_labeled_demos:

  Maximum number of labeled demonstrations.

- auto:

  Auto-tuned settings: "light", "medium", "heavy", or NULL.

- num_candidates:

  Optional override for number of instruction candidates.

- num_threads:

  Number of threads to use for evaluation.

- seed:

  Random seed for reproducibility.

- init_temperature:

  Initial temperature for instruction proposals.

- track_stats:

  Whether to track trial history.

- log_dir:

  Directory for trial logging.

## Examples

``` r
if (FALSE) { # \dontrun{
tp <- MIPROv2(
  metric = metric_exact_match(field = "answer"),
  auto = "light",
  max_bootstrapped_demos = 4L
)

compiled <- compile(tp, qa_module, trainset, valset = valset, .llm = llm)
} # }
```
