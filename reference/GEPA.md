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
