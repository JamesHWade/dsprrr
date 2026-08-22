# Omni Teleprompter

A meta-teleprompter that explores several optimization strategies from
the same seed program, compares their outputs with one shared validation
metric, and seeds a fresh continuation optimizer from the winner.

Inspired by the Omni meta-optimizer from the [GEPA
project](https://github.com/gepa-ai/gepa), this adapts the explore,
pick-best, and continue pattern described in the [GEPA Omni
announcement](https://gepa-ai.github.io/gepa/blog/2026/07/22/optimize-anything-omni/)
to dsprrr modules. The original program remains a candidate throughout,
so a regressing explorer or continuation step cannot replace a better
program.

`Omni()` does not impose a common budget because dsprrr teleprompters
expose different native budget controls. Configure comparable budgets on
the explorer objects before constructing `Omni()`. Common validation
re-scoring of the seed, each explorer result, and the continuation
result is additional evaluation work outside those native optimizer
budgets.

## Usage

``` r
Omni(
  metric,
  explorers,
  continuation,
  metric_threshold = NULL,
  max_errors = 5L,
  valset_ratio = 0.1,
  parallel = FALSE,
  num_workers = NULL,
  seed = NULL,
  verbose = TRUE
)
```

## Arguments

- metric:

  Metric function used to compare every candidate on the same validation
  set.

- explorers:

  Named list of at least two
  [Teleprompter](https://jameshwade.github.io/dsprrr/reference/Teleprompter.md)
  objects. Every explorer starts from an independent copy of the input
  program.

- continuation:

  A
  [Teleprompter](https://jameshwade.github.io/dsprrr/reference/Teleprompter.md)
  object run from the best exploration candidate.

- metric_threshold:

  Minimum score required to be considered successful.

- max_errors:

  Maximum number of errors allowed during evaluation.

- valset_ratio:

  Fraction of `trainset` to hold out for candidate comparison when
  `valset` is not supplied.

- parallel:

  Whether to compile exploration branches concurrently with mirai.
  Parallel exploration requires `.llm = NULL`. Each worker creates its
  own default chat from `OPENAI_API_KEY`, `ANTHROPIC_API_KEY`, or
  `GOOGLE_API_KEY`.

- num_workers:

  Number of mirai workers for parallel exploration. `NULL` uses one
  worker per explorer.

- seed:

  Optional whole-number random seed within R's integer range for
  reproducible splitting, sequential exploration, and mirai worker
  streams.

- verbose:

  Whether to print progress messages.

## Examples

``` r
if (FALSE) { # \dontrun{
metric <- metric_exact_match(field = "answer")

tp <- Omni(
  metric = metric,
  explorers = list(
    bootstrap = BootstrapFewShotWithRandomSearch(metric = metric),
    gepa = GEPA(metric = metric, population_size = 4L, generations = 2L)
  ),
  continuation = GEPA(
    metric = metric,
    population_size = 4L,
    generations = 2L
  )
)

compiled <- compile(tp, qa_module, trainset, valset = valset, .llm = llm)
compiled$config$optimizer$candidate_programs
} # }
```
