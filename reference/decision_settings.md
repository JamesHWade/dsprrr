# Decision settings of a module

**\[experimental\]**

Lists the effective numeric settings of every decision output configured
with
[`with_decisions()`](https://jameshwade.github.io/dsprrr/reference/with_decisions.md),
including values fitted by
[`ReAnchor()`](https://jameshwade.github.io/dsprrr/reference/ReAnchor.md).

## Usage

``` r
decision_settings(module)
```

## Arguments

- module:

  A module.

## Value

A tibble with one row per decision field and columns `field`, `kind`,
`threshold` (Boolean decisions), `cuts` (Score decisions, a
list-column), and `weights` (Choice decisions, a list-column of named
numeric vectors).

## See also

Other decisions:
[`ReAnchor()`](https://jameshwade.github.io/dsprrr/reference/ReAnchor.md),
[`decision_evidence()`](https://jameshwade.github.io/dsprrr/reference/decision_evidence.md),
[`decision_types`](https://jameshwade.github.io/dsprrr/reference/decision_types.md),
[`with_decisions()`](https://jameshwade.github.io/dsprrr/reference/with_decisions.md)

## Examples

``` r
sig <- signature(
  inputs = list(input("ticket", description = "Customer report")),
  output_type = ellmer::type_object(
    urgent = ellmer::type_boolean("Is the service blocked?")
  )
)
triage <- module(sig) |> with_decisions(urgent = decision_bool())
decision_settings(triage)
#> # A tibble: 1 × 5
#>   field  kind  threshold cuts   weights
#>   <chr>  <chr>     <dbl> <list> <list> 
#> 1 urgent bool        0.5 <NULL> <NULL> 

# A stricter threshold for the same question
stricter <- triage |> with_decisions(urgent = decision_bool(threshold = 0.8))
decision_settings(stricter)
#> # A tibble: 1 × 5
#>   field  kind  threshold cuts   weights
#>   <chr>  <chr>     <dbl> <list> <list> 
#> 1 urgent bool        0.8 <NULL> <NULL> 
```
