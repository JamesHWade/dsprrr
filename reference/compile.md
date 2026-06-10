# Compile S7 Generic and Methods

Generic method for compiling/optimizing a module using a teleprompter.

## Usage

``` r
compile(teleprompter, program, ...)
```

## Arguments

- teleprompter:

  A Teleprompter object

- program:

  A module to optimize

- ...:

  Additional arguments including trainset (training data)

## Value

An optimized module

## Details

This file defines the compile generic and its methods for optimizing
DSPrrr modules using teleprompters. Compile Generic

## See also

[`compile_module()`](https://jameshwade.github.io/dsprrr/reference/compile_module.md)
for the pipe-friendly wrapper with validation and friendlier argument
order

## Examples

``` r
if (FALSE) { # \dontrun{
classifier <- module(signature("text -> sentiment"), type = "predict")
trainset <- dsp_trainset(
  text = c("I love it!", "Terrible experience"),
  sentiment = c("positive", "negative")
)
optimized <- compile(LabeledFewShot(k = 2L), classifier, trainset)
} # }
```
