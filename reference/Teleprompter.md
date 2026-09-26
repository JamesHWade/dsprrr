# Arguments shared by all optimizers

`Teleprompter()` is the parent class of every optimizer in dsprrr
("teleprompter" is DSPy's original name for an optimizer). You never
compile with it directly: create one of the optimizers below and pass it
to
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md).
This page documents the three arguments that every optimizer accepts.

## Usage

``` r
Teleprompter(metric = NULL, metric_threshold = NULL, max_errors = 5L)
```

## Arguments

- metric:

  A metric function called as `metric(prediction, expected)`, such as
  `metric_exact_match(field = "answer")`, or `NULL`. Optimizers that
  score candidates require one.

- metric_threshold:

  A score between 0 and 1, or `NULL` (the default). Only some optimizers
  use it, and each optimizer's page says how.

- max_errors:

  Integer error budget (default `5L`). The optimizers that run under
  [`optimizer_control()`](https://jameshwade.github.io/dsprrr/reference/optimizer_control.md)
  (the two bootstrap optimizers, COPRO, MIPROv2, SIMBA, GEPA,
  AutoResearch and MetaHarness) stop after this many consecutive failed
  evaluations when
  [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
  is not given a `control` object.

## Value

A `Teleprompter` object.

## Details

Most optimizers store counts as S7 integer properties, so write `5L`
rather than `5`. A double fails with an error such as
`@max_errors must be <integer>, not <double>`.

### Optimizers at a glance

|  |  |
|----|----|
| Optimizer | What it changes |
| [`LabeledFewShot()`](https://jameshwade.github.io/dsprrr/reference/LabeledFewShot.md) | Demos: `k` training rows. No model calls. |
| [`KNNFewShot()`](https://jameshwade.github.io/dsprrr/reference/KNNFewShot.md) | Demos: the training rows most similar to each input, chosen at run time. |
| [`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md) | Demos: training rows plus the program's own outputs that pass the metric. |
| [`BootstrapFewShotWithRandomSearch()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShotWithRandomSearch.md) | Demos: the best of several bootstrap candidates on a validation set. |
| [`GridSearchTeleprompter()`](https://jameshwade.github.io/dsprrr/reference/GridSearchTeleprompter.md) | Instructions, template or settings, from a table of variants. |
| [`COPRO()`](https://jameshwade.github.io/dsprrr/reference/COPRO.md) | Instructions, rewritten by a model over several rounds. |
| [`MIPROv2()`](https://jameshwade.github.io/dsprrr/reference/MIPROv2.md) | Instructions and demos, searched together. |
| [`SIMBA()`](https://jameshwade.github.io/dsprrr/reference/SIMBA.md) | Instruction rules and demos taken from hard examples. |
| [`GEPA()`](https://jameshwade.github.io/dsprrr/reference/GEPA.md) | Instructions (and Flex source) evolved by reflecting on failures. |
| [`ReAnchor()`](https://jameshwade.github.io/dsprrr/reference/ReAnchor.md) | Thresholds, cuts and weights of decision outputs. |
| [`BetterTogether()`](https://jameshwade.github.io/dsprrr/reference/BetterTogether.md), [`Omni()`](https://jameshwade.github.io/dsprrr/reference/Omni.md) | Run other optimizers in sequence or in competition. |
| [`AutoResearch()`](https://jameshwade.github.io/dsprrr/reference/AutoResearch.md), [`MetaHarness()`](https://jameshwade.github.io/dsprrr/reference/MetaHarness.md) | An agent proposes and tests program edits. |

To sweep runtime settings such as `reasoning_effort` on a module in
place, use
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)
instead.

## See also

Other teleprompters:
[`AutoResearch()`](https://jameshwade.github.io/dsprrr/reference/AutoResearch.md),
[`BetterTogether()`](https://jameshwade.github.io/dsprrr/reference/BetterTogether.md),
[`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md),
[`BootstrapFewShotWithRandomSearch()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShotWithRandomSearch.md),
[`COPRO()`](https://jameshwade.github.io/dsprrr/reference/COPRO.md),
[`GEPA()`](https://jameshwade.github.io/dsprrr/reference/GEPA.md),
[`GridSearchTeleprompter()`](https://jameshwade.github.io/dsprrr/reference/GridSearchTeleprompter.md),
[`KNNFewShot()`](https://jameshwade.github.io/dsprrr/reference/KNNFewShot.md),
[`LabeledFewShot()`](https://jameshwade.github.io/dsprrr/reference/LabeledFewShot.md),
[`MIPROv2()`](https://jameshwade.github.io/dsprrr/reference/MIPROv2.md),
[`MetaHarness()`](https://jameshwade.github.io/dsprrr/reference/MetaHarness.md),
[`Omni()`](https://jameshwade.github.io/dsprrr/reference/Omni.md),
[`ReAnchor()`](https://jameshwade.github.io/dsprrr/reference/ReAnchor.md),
[`SIMBA()`](https://jameshwade.github.io/dsprrr/reference/SIMBA.md),
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)

## Examples

``` r
Teleprompter()
#> <dsprrr::Teleprompter>
#>  @ metric          : NULL
#>  @ metric_threshold: NULL
#>  @ max_errors      : int 5

# Counts must be integers
try(Teleprompter(max_errors = 5))
#> Error : <dsprrr::Teleprompter> object properties are invalid:
#> - @max_errors must be <integer>, not <double>
```
