# Select Outputs for Pipeline Steps

Filters which output fields from a module should be passed to the next
step. By default, all fields are passed forward.

## Usage

``` r
select_outputs(module, ...)
```

## Arguments

- module:

  A Module object

- ...:

  Field names to select (as character strings)

## Value

A PipelineMappedModule object for use with `%>>%`

## Examples

``` r
if (FALSE) { # \dontrun{
# Only pass 'answer' field, drop 'reasoning'
mod_cot %>>%
  select_outputs(mod_next, "answer") %>>%
  mod_format
} # }
```
