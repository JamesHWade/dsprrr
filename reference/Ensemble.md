# Ensemble Teleprompter

Creates an ensemble from multiple compiled modules. The ensemble runs
all modules and combines their outputs using a reduce function (default:
majority voting).

## Usage

``` r
Ensemble(
  metric = NULL,
  metric_threshold = NULL,
  max_errors = 5L,
  reduce_fn = NULL,
  size = NULL,
  weights = NULL
)
```

## Arguments

- metric:

  A metric function for evaluating predictions. If NULL, uses
  exact_match() by default. Used for computing ensemble performance.

- metric_threshold:

  Minimum score required to be considered successful.

- max_errors:

  Maximum number of errors allowed during optimization.

- reduce_fn:

  Function to combine outputs. Default is
  [`reduce_majority()`](https://jameshwade.github.io/dsprrr/reference/reduce_majority.md).
  Other options include
  [`reduce_weighted_vote()`](https://jameshwade.github.io/dsprrr/reference/reduce_weighted_vote.md),
  [`reduce_first()`](https://jameshwade.github.io/dsprrr/reference/reduce_first.md),
  and
  [`reduce_best_by_metric()`](https://jameshwade.github.io/dsprrr/reference/reduce_best_by_metric.md).

- size:

  Optional integer. If provided, limits the number of modules to include
  in the ensemble (takes the first `size` from the programs list).

- weights:

  Optional numeric vector of weights for each module. Commonly set to
  validation scores from optimization.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create ensemble teleprompter
tp <- Ensemble(reduce_fn = reduce_majority())

# Compile with list of modules
compiled_modules <- list(mod1, mod2, mod3)
ens <- compile(tp, programs = compiled_modules)

# Or use top candidates from BootstrapFewShotWithRandomSearch
rs_compiled <- compile(rs_teleprompter, program, trainset)
candidates <- rs_compiled$config$optimizer$candidate_programs[1:3]
weights <- rs_compiled$config$optimizer$candidate_scores[1:3]
tp <- Ensemble(
  reduce_fn = reduce_weighted_vote(),
  weights = weights
)
ens <- compile(tp, programs = candidates)
} # }
```
