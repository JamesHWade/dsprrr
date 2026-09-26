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

[`with_decisions()`](https://jameshwade.github.io/dsprrr/reference/with_decisions.md)
