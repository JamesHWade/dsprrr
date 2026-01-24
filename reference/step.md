# Create a Pipeline Step with Mappings

Wraps a module with input/output mapping configuration for use in
[`pipeline()`](https://jameshwade.github.io/dsprrr/reference/pipeline.md).

## Usage

``` r
step(module, map = list(), select = character(0), ...)
```

## Arguments

- module:

  A Module object

- map:

  Named character vector mapping upstream output fields to this module's
  input fields. Format: `c(output_field = "input_field")`.

- select:

  Character vector of output field names to pass forward. If empty, all
  fields are passed.

- ...:

  Static inputs to inject (name = value pairs)

## Value

A PipelineStep object for use in
[`pipeline()`](https://jameshwade.github.io/dsprrr/reference/pipeline.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Map 'documents' from upstream to 'context' input
pipeline(
  mod_retrieve,
  step(mod_answer, map = c(documents = "context")),
  mod_format
)

# Select only certain output fields
pipeline(
  mod_analyze,
  step(mod_format, select = c("summary", "score")),
  mod_present
)

# Inject static inputs
pipeline(
  mod_retrieve,
  step(mod_answer, system_prompt = "Be concise")
)
} # }
```
