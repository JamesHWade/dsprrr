# Sample from a Dataset Deterministically

Sample rows from a dataset with optional seed for reproducibility. Used
by optimizers for consistent train/validation splits and demo selection.

## Usage

``` r
sample_dataset(dataset, n = NULL, seed = NULL, replace = FALSE)
```

## Arguments

- dataset:

  A data frame to sample from.

- n:

  Number of rows to sample. If NULL or greater than nrow(dataset),
  returns the full dataset.

- seed:

  Random seed for reproducibility. If NULL, sampling is random.

- replace:

  Whether to sample with replacement. Default is FALSE.

## Value

A data frame containing the sampled rows.

## Examples

``` r
df <- tibble::tibble(x = 1:10, y = letters[1:10])

# Deterministic sampling
sample1 <- sample_dataset(df, n = 5, seed = 42)
sample2 <- sample_dataset(df, n = 5, seed = 42)
identical(sample1, sample2)  # TRUE
#> [1] TRUE

# Random sampling (different each time)
sample3 <- sample_dataset(df, n = 5)
```
