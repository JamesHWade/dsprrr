# Top-p Parameter for dials

Creates a dials parameter object for LLM top-p (nucleus sampling).

## Usage

``` r
top_p(range = c(0, 1), trans = NULL)
```

## Arguments

- range:

  Range of top-p values (default c(0, 1)).

- trans:

  Transformation (default NULL for identity).

## Value

A dials parameter object.

## Examples

``` r
if (FALSE) { # \dontrun{
top_p()
top_p(range = c(0.5, 1))
} # }
```
