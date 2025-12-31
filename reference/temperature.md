# Temperature Parameter for dials

Creates a dials parameter object for LLM temperature.

## Usage

``` r
temperature(range = c(0, 1), trans = NULL)
```

## Arguments

- range:

  Range of temperature values (default c(0, 1)).

- trans:

  Transformation (default NULL for identity).

## Value

A dials parameter object.

## Examples

``` r
if (FALSE) { # \dontrun{
temperature()
temperature(range = c(0.1, 0.9))
} # }
```
