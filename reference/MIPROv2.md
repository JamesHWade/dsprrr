# MIPROv2 Teleprompter

MIPROv2 jointly optimizes instructions and few-shot demonstrations using
a discrete Bayesian optimization loop with minibatch evaluation for a
root Predict module. For graphs with nested predictors such as RLM, it
optimizes child instructions only and requires
`max_bootstrapped_demos = 0L`; nested demo bootstrapping fails
explicitly until predictor-local evidence is available.

## Usage

``` r
MIPROv2(
  metric = NULL,
  metric_threshold = NULL,
  max_errors = 5L,
  task_model = NULL,
  teacher_settings = NULL,
  max_bootstrapped_demos = 4L,
  max_labeled_demos = 4L,
  auto = "light",
  num_candidates = NULL,
  num_threads = 1L,
  seed = 9L,
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

- task_model:

  Optional ellmer Chat used to evaluate tasks. `NULL` uses the `.llm`
  supplied to
  [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md).

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

- track_stats:

  Whether to track trial history.

- log_dir:

  Directory for trial logging.

## Value

A `MIPROv2` teleprompter object.

## Examples

``` r

if (FALSE) { # \dontrun{
tp <- MIPROv2(
  metric = metric_exact_match(field = "answer"),
  auto = "light",
  max_bootstrapped_demos = 4L
)

qa_module <- module(signature("question -> answer"))
trainset <- data.frame(question = "Capital of France?", answer = "Paris")
valset <- data.frame(question = "Capital of Japan?", answer = "Tokyo")
llm <- ellmer::chat_openai()
compiled <- compile(qa_module, tp, trainset, valset = valset, .llm = llm)
} # }
```
