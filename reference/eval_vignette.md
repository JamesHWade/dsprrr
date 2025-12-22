# Determine if vignettes should be evaluated

Checks for vcr cassettes or API credentials to determine if vignette
code should be executed. Always returns FALSE during R CMD check to
avoid cassette mismatch errors.

## Usage

``` r
eval_vignette()
```

## Value

Logical indicating whether to evaluate vignette code
