# Split Dataset into Train and Validation Sets

Split a dataset into training and validation portions with optional seed
for reproducibility.

## Usage

``` r
split_dataset(dataset, prop = 0.8, seed = NULL)
```

## Arguments

- dataset:

  A data frame to split.

- prop:

  Proportion of data for training. Default is 0.8.

- seed:

  Random seed for reproducibility.

## Value

A list with `train` and `val` data frames.

## Examples

``` r
df <- tibble::tibble(x = 1:100)
split <- split_dataset(df, prop = 0.8, seed = 42)
nrow(split$train)  # ~80
#> [1] 80
nrow(split$val)    # ~20
#> [1] 20
```
