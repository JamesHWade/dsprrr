# Create Training Data for DSPrrr

Helper function to create properly formatted training data for DSPrrr
compilation.

## Usage

``` r
dsp_trainset(..., .data = NULL)
```

## Arguments

- ...:

  Named vectors or lists representing input/output pairs

- .data:

  Optional data frame to use as base

## Value

A data frame suitable for use as trainset

## Examples

``` r
# Create training data from scratch
trainset <- dsp_trainset(
  text = c("Great product!", "Awful service"),
  sentiment = c("positive", "negative")
)

# Add to existing data frame
df <- data.frame(text = c("Hello", "World"))
trainset <- dsp_trainset(.data = df, label = c("greeting", "other"))
```
