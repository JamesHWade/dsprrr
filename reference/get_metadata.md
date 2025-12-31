# Get metadata from a result

Extract metadata from a DSPrrr result object.

## Usage

``` r
get_metadata(x, ...)
```

## Arguments

- x:

  A DSPrrr result object

- ...:

  Additional arguments (unused)

## Value

A list or tibble of metadata

## Examples

``` r
if (FALSE) { # \dontrun{
result <- run(mod, text = "hello", .return_format = "structured")
get_metadata(result)
} # }
```
