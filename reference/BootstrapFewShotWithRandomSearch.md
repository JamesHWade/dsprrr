# BootstrapFewShotWithRandomSearch Teleprompter

A teleprompter that extends BootstrapFewShot with random search over
multiple candidate programs. It generates several candidate
configurations and selects the best based on validation set performance.

The optimizer generates candidates including:

1.  Uncompiled baseline program

2.  LabeledFewShot-only program

3.  BootstrapFewShot with unshuffled examples

4.  BootstrapFewShot with various random seeds

Programs containing Flex or RLM modules are rejected before candidate
evaluation because their predictor-local demonstration contracts are not
currently available to this optimizer.

## Usage

``` r
BootstrapFewShotWithRandomSearch(
  metric = NULL,
  metric_threshold = NULL,
  max_errors = 5L,
  num_candidate_programs = 16L,
  num_threads = 1L,
  stop_at_score = NULL,
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

  Maximum number of errors allowed during optimization. Default is 10.

- num_candidate_programs:

  Number of candidate programs to generate. Default is 16.

- num_threads:

  Number of threads for parallel evaluation. If 1, runs sequentially.
  Default is 1.

- stop_at_score:

  Early stopping threshold. If a candidate achieves this score or
  higher, stop searching. Default is NULL (no early stop).

- max_bootstrapped_demos:

  Maximum bootstrapped demos per candidate. Default is 4.

- max_labeled_demos:

  Maximum labeled demos per candidate. Default is 16.

- max_rounds:

  Number of bootstrap rounds per candidate. Default is 1.

- teacher_settings:

  List of settings for the teacher model. If NULL, defaults to
  `list(temperature = 0.7)`.

- seed:

  Random seed for reproducibility. Default is NULL.

- log_dir:

  Directory for trial logging. Default is NULL.

## Value

A `BootstrapFewShotWithRandomSearch` teleprompter object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a BootstrapFewShotWithRandomSearch teleprompter
tp <- BootstrapFewShotWithRandomSearch(
  metric = metric_exact_match(field = "answer"),
  num_candidate_programs = 8L,
  num_threads = 4L,
  stop_at_score = 0.95
)

# Compile with validation set (required)
qa_module <- module(signature("question -> answer"))
trainset <- data.frame(question = "Capital of France?", answer = "Paris")
valset <- data.frame(question = "Capital of Japan?", answer = "Tokyo")
llm <- ellmer::chat_openai()
compiled <- compile(qa_module, tp, trainset, valset = valset, .llm = llm)

# Access ranked candidates
optimization_result(compiled)$extensions$bootstrap_few_shot_with_random_search$candidate_programs
} # }
```
