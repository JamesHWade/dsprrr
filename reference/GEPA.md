# GEPA Teleprompter

Reflective optimizer for instructions and complete Flex source
components, using row-level failures and metric feedback to propose
improved candidates.

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
  component_selector = "round_robin",
  use_merge = TRUE,
  max_merge_invocations = 5L,
  seed = NULL,
  log_dir = NULL,
  verbose = TRUE,
  track_stats = TRUE,
  track_best_outputs = FALSE
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

- component_selector:

  Component mutation strategy. Use `"round_robin"` to update one
  component at a time, `"all"` to update all components atomically, or a
  function called with `component_ids`, `candidate`, `failed_examples`,
  and `context`. The function must return one or more unique IDs from
  `component_ids`.

- use_merge:

  Whether to attempt lineage-aware merges of complementary component
  changes.

- max_merge_invocations:

  Maximum merge attempts, or `NULL` for no separate merge-attempt cap.

- seed:

  Random seed for reproducibility.

- log_dir:

  Optional directory for trial logging.

- verbose:

  Whether to print progress messages.

- track_stats:

  Whether to record generation statistics.

- track_best_outputs:

  Whether to retain each validation row's highest-scoring output.
  Requires `track_stats = TRUE`.

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

This is an adapted implementation. It shares reflective mutation guided
by failures and feedback, validation-example winner frontiers, component
selection, lineage-aware merge, and Pareto selection over multiple
metrics, but uses a fixed population/generations loop rather than DSPy's
full budget-driven candidate search.

Every mutable program is represented as a complete component candidate:
ordinary predictor instructions and complete Flex `module_src` values
are proposed, copied, validated, and bound transactionally. A Flex
source is one component; dynamically constructed inner predictors are
not optimized as separate leaves. Invalid Flex sources receive an
auditable failure score and are never selectable. The structured source
proposer receives task and signature context, field schemas, source
runtime, allowed tools and primitives, row-aligned inputs, expected
output, prediction, and metric feedback. Executable source is evaluated
only through Flex's configured interpreter bridge during candidate
evaluation.

Parent selection uses the union of complete candidates that win on at
least one validation row and candidates on the multi-metric objective
Pareto front; component selection is a separate mutation policy. When
`valset` is supplied, `trainset` remains the discovery/reflection
dataset and `valset` is used for aggregate selection,
validation-instance frontiers, and retained outputs. Candidate metadata
includes lineage, aggregate and per-row scores, discovery counts,
winners, and optional retained best outputs. Fine-grained checkpoint
resume, cached subsample merge acceptance, and automatic inference-time
candidate selection are not implemented. Compiled programs record these
distinctions under `config$optimizer$component_semantics`.

## Examples

``` r
# A small GEPA run: 6 candidates evolved over 2 generations
tp <- GEPA(
  metric = metric_exact_match(field = "answer"),
  population_size = 6L,
  generations = 2L,
  seed = 42
)

if (FALSE) { # \dontrun{
# Feedback-aware metrics give the reflection step concrete guidance.
# During evaluation the metric receives the full expected row, so
# extract the target field explicitly:
feedback_metric <- metric_with_feedback(
  function(prediction, expected) {
    if (identical(as.character(prediction), expected$answer)) {
      list(score = 1, feedback = "Correct.")
    } else {
      list(
        score = 0,
        feedback = paste0(
          "Expected '",
          expected$answer,
          "' but got '",
          prediction,
          "'."
        )
      )
    }
  },
  field = "answer"
)
tp <- GEPA(metric = feedback_metric, seed = 42)

qa <- module(signature("question -> answer"), type = "predict")
trainset <- data.frame(
  question = c("What is 2 + 2?", "What is the capital of France?"),
  answer = c("4", "Paris")
)
optimized <- compile(tp, qa, trainset)
} # }
```
