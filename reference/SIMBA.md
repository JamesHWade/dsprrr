# SIMBA Teleprompter

SIMBA (self-improving via hard example mining) iteratively samples
mini-batches, identifies high-variability examples, and generates
improvement rules or demonstrations to improve performance.

The optimizer:

1.  Evaluates baseline performance on the training set (or validation
    set).

2.  Repeats for up to `max_steps`:

    - Samples a mini-batch

    - Runs multiple candidates to measure variability

    - Identifies hard examples

    - Generates a rule and/or adds demos

    - Evaluates improvement and keeps changes if better

## Usage

``` r
SIMBA(
  metric = NULL,
  metric_threshold = NULL,
  max_errors = 5L,
  bsize = 32L,
  num_candidates = 6L,
  max_steps = 8L,
  max_demos = 4L,
  prompt_model = NULL,
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

- bsize:

  Mini-batch size for hard example mining. Default is 32.

- num_candidates:

  Number of candidate runs per example to measure variability. Default
  is 6.

- max_steps:

  Maximum number of optimization steps. Default is 8.

- max_demos:

  Maximum number of demonstrations to keep. Default is 4.

- prompt_model:

  Optional LLM for rule generation (reflection).

- seed:

  Random seed for reproducibility. Default is 0.

- log_dir:

  Directory for trial logging. Default is NULL.

## Details

### Differences from DSPy's SIMBA

This is an adapted implementation: it mines hard (high-variability)
examples and asks an LLM to generate improvement rules, but it does not
reproduce every detail of DSPy's stochastic introspective mini-batch
ascent (e.g., trajectory-level introspection across candidate programs).
Expect qualitatively similar behavior, not identical results.

## Examples

``` r
if (FALSE) { # \dontrun{
tp <- SIMBA(
  metric = metric_exact_match(field = "answer"),
  bsize = 32L,
  num_candidates = 6L,
  max_steps = 8L,
  max_demos = 4L,
  prompt_model = ellmer::chat_openai(),
  seed = 0L
)

compiled <- compile(tp, qa_module, trainset, .llm = llm)
} # }
```
