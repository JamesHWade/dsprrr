# Inject Static Inputs for Pipeline Steps

Specifies constant values to inject as inputs to a module, regardless of
what the upstream module outputs.

## Usage

``` r
with_inputs(module, ...)
```

## Arguments

- module:

  A Module object

- ...:

  Named values to inject as inputs

## Value

A PipelineMappedModule object for use with `%>>%`

## Examples

``` r
if (FALSE) { # \dontrun{
# Inject a constant system prompt
mod_retrieve %>>%
  with_inputs(mod_answer, system_prompt = "Be very concise") %>>%
  mod_format
} # }
```
