# Compile Programs into an Ensemble

Convenience function to create an ensemble from a list of modules
without explicitly creating a teleprompter.

## Usage

``` r
ensemble_from_programs(programs, reduce_fn = NULL, weights = NULL, size = NULL)
```

## Arguments

- programs:

  List of Module objects to combine

- reduce_fn:

  Function to combine outputs. Default is
  [`reduce_majority()`](https://jameshwade.github.io/dsprrr/reference/reduce_majority.md).

- weights:

  Optional numeric vector of weights for each module.

- size:

  Optional integer to limit number of modules included.

## Value

An EnsembleModule

## Examples

``` r
if (FALSE) { # \dontrun{
# Quick ensemble creation
ens <- compile_ensemble(list(mod1, mod2, mod3))

# With weights from optimization
ens <- compile_ensemble(
  programs = candidates,
  reduce_fn = reduce_weighted_vote(),
  weights = validation_scores
)
} # }
```
