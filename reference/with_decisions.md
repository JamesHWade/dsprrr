# Attach calibrated decision outputs to a module

**\[experimental\]**

Returns a copy of a Predict module in which the named output fields are
decoded from probability evidence. Each field must be declared in the
module's object-shaped signature output: a `type_boolean()` field for
[`decision_bool()`](https://jameshwade.github.io/dsprrr/reference/decision_types.md),
or a `type_enum()` field for
[`decision_score()`](https://jameshwade.github.io/dsprrr/reference/decision_types.md)
and
[`decision_choice()`](https://jameshwade.github.io/dsprrr/reference/decision_types.md).

A decision field needs a question: either the output type's description
(for example `type_boolean("Is the service blocked?")`) or the spec's
`description`. Without one, `with_decisions()` fails rather than asking
an unspecified question.

Pass `NULL` for a field to remove its decision configuration and return
that field to direct generation.

Decision modules run on the sequential execution path. Concurrent batch
backends are rejected rather than returning undecoded evidence.

## Usage

``` r
with_decisions(module, ...)
```

## Arguments

- module:

  A module created by
  [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)
  or
  [`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md).

- ...:

  Named
  [decision_types](https://jameshwade.github.io/dsprrr/reference/decision_types.md)
  specifications, or `NULL`, keyed by output field name.

## Value

A modified copy of `module`. The original is unchanged.

## See also

Other decisions:
[`ReAnchor()`](https://jameshwade.github.io/dsprrr/reference/ReAnchor.md),
[`decision_evidence()`](https://jameshwade.github.io/dsprrr/reference/decision_evidence.md),
[`decision_settings()`](https://jameshwade.github.io/dsprrr/reference/decision_settings.md),
[`decision_types`](https://jameshwade.github.io/dsprrr/reference/decision_types.md)

## Examples

``` r
sig <- signature(
  inputs = list(input("ticket", description = "Customer report")),
  output_type = ellmer::type_object(
    urgent = ellmer::type_boolean("Is the service blocked?"),
    severity = ellmer::type_enum(
      c("minor", "disruptive", "blocking"),
      "How severe is the impact?"
    ),
    category = ellmer::type_enum(
      c("billing", "technical"),
      "Which team owns the issue?"
    )
  )
)

triage <- module(sig) |>
  with_decisions(
    urgent = decision_bool(threshold = 0.7),
    severity = decision_score(),
    category = decision_choice(
      criteria = c(billing = "Payments", technical = "Product faults")
    )
  )

decision_settings(triage)
#> # A tibble: 3 × 5
#>   field    kind   threshold cuts      weights  
#>   <chr>    <chr>      <dbl> <list>    <list>   
#> 1 urgent   bool         0.7 <NULL>    <NULL>   
#> 2 severity score       NA   <dbl [2]> <NULL>   
#> 3 category choice      NA   <NULL>    <dbl [2]>
```
