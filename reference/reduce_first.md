# First Successful Output Reducer

Creates a reduce function that simply returns the first successful
output. Useful when you want to try multiple modules but just need one
answer.

## Usage

``` r
reduce_first()
```

## Value

A reduce function for use with
[`ensemble()`](https://jameshwade.github.io/dsprrr/reference/ensemble_module.md)

## Examples

``` r
if (FALSE) { # \dontrun{
ens <- ensemble(modules, reduce_fn = reduce_first())
} # }
```
