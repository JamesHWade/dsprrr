# Get token counts from a result

Extract token usage information from a DSPrrr result object.

## Usage

``` r
get_tokens(x, ...)
```

## Arguments

- x:

  A DSPrrr result object

- ...:

  Additional arguments (unused)

## Value

A list or tibble with token counts (input_tokens, output_tokens,
total_tokens)

## Examples

``` r
if (FALSE) { # \dontrun{
result <- run(mod, text = "hello", .return_format = "structured")
get_tokens(result)
} # }
```
