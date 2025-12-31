# Get output from a result

Extract the output from a DSPrrr result object.

## Usage

``` r
get_output(x, ...)
```

## Arguments

- x:

  A DSPrrr result object (e.g., from
  [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) with
  `.return_format = "structured"`)

- ...:

  Additional arguments (unused)

## Value

The output value(s) from the result

## Examples

``` r
if (FALSE) { # \dontrun{
result <- run(mod, text = "hello", .return_format = "structured")
get_output(result)
} # }
```
