# Map Inputs for Pipeline Steps

Specifies how output fields from the previous step should be mapped to
input fields of this module. Use with `%>>%` operator.

## Usage

``` r
map_inputs(module, ...)
```

## Arguments

- module:

  A Module object

- ...:

  Named mappings: `output_field = "input_field"`

## Value

A PipelineMappedModule object for use with `%>>%`

## Examples

``` r
if (FALSE) { # \dontrun{
# Map 'documents' output to 'context' input
mod_retrieve %>>%
  map_inputs(mod_answer, documents = "context") %>>%
  mod_format
} # }
```
