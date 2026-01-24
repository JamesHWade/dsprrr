# Pipeline Module for Sequential Module Composition

A module that chains multiple modules together, passing outputs from one
to inputs of the next. Provides DSPy-style piping with the `%>>%`
operator.

Explicitly constructs a pipeline from a sequence of modules. Use this
when you need fine-grained control over input/output mapping between
steps.

## Usage

``` r
pipeline(...)
```

## Arguments

- ...:

  Modules or steps (created with
  [`step()`](https://jameshwade.github.io/dsprrr/reference/step.md)) to
  chain together

## Value

A PipelineModule

## Examples

``` r
if (FALSE) { # \dontrun{
# Simple pipeline
p <- pipeline(mod_a, mod_b, mod_c)

# With explicit mapping
p <- pipeline(
  mod_retrieve,
  step(mod_answer, map = c(documents = "context")),
  mod_summarize
)

# Run it
result <- run(p, query = "...", .llm = llm)
} # }
```
