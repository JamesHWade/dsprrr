# GEPA Teleprompter

Genetic/evolutionary prompt optimizer that evolves instruction variants
using reflection on failed examples.

## Usage

``` r
GEPA(
  metric = NULL,
  metric_threshold = NULL,
  max_errors = 5L,
  metrics = NULL,
  population_size = 20L,
  generations = 10L,
  mutation_rate = 0.1,
  crossover_rate = 0.7,
  selection = "pareto",
  seed = NULL,
  log_dir = NULL,
  verbose = TRUE,
  track_stats = TRUE
)
```

## Arguments

- metric:

  A single metric function (fallback when `metrics` is NULL).

- metric_threshold:

  Minimum score for an example to be considered successful.

- max_errors:

  Maximum number of errors allowed during optimization.

- metrics:

  Named list of metric functions for evaluation.

- population_size:

  Size of the population. Default is 20.

- generations:

  Number of generations to run. Default is 10.

- mutation_rate:

  Probability of mutation. Default is 0.1.

- crossover_rate:

  Probability of crossover. Default is 0.7.

- selection:

  Selection strategy: "pareto" or "current_best".

- seed:

  Random seed for reproducibility.

- log_dir:

  Optional directory for trial logging.

- verbose:

  Whether to print progress messages.

- track_stats:

  Whether to record generation statistics.

## Details

### Feedback metrics

GEPA works best with feedback-aware metrics created via
[`metric_with_feedback()`](https://jameshwade.github.io/dsprrr/reference/metric_with_feedback.md).
When the metric returns `list(score = , feedback = )`, the textual
feedback for failed examples is included in the reflection prompt,
giving the reflection LLM concrete guidance on *why* an output was wrong
— the key mechanism in the GEPA paper ("GEPA: Reflective Prompt
Evolution Can Outperform RL", Agrawal et al., 2025). Plain numeric
metrics still work; reflection then sees only inputs, expected, and
predicted values.

### Differences from DSPy's GEPA

This is an adapted ("GEPA-lite") implementation. It shares the core
ideas — reflective mutation of instructions guided by failures and
feedback, plus Pareto-frontier selection over multiple metrics — but
uses a fixed population/generations evolutionary loop rather than DSPy's
budget-driven candidate search, and does not yet support per-component
selection in multi-step programs or inference-time search. Expect
qualitatively similar behavior, not identical results.
