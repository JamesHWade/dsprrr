# Get cost from a result

Extract cost information from a DSPrrr result object.

## Usage

``` r
get_cost(x, ...)
```

## Arguments

- x:

  A DSPrrr result object

- ...:

  Additional arguments (unused)

## Value

A numeric value or tibble with cost information

## Examples

``` r
if (FALSE) { # \dontrun{
result <- run(mod, text = "hello", .return_format = "structured")
get_cost(result)
} # }
```
