# Compile a program

Optimize a dsprrr program with a teleprompter. This is the single
user-facing compilation entry point and is ordered for the native pipe:
`program |> compile(teleprompter, trainset)`.

## Usage

``` r
compile(program, teleprompter, ...)
```

## Arguments

- program:

  A dsprrr module or compositional program to optimize.

- teleprompter:

  A Teleprompter defining the optimization strategy.

- ...:

  Additional arguments. The first is normally `trainset`, a data frame.
  Optimizers may also accept `valset`, `.llm`, and `.trace_context`.

## Value

An optimized program.

## Examples

``` r
if (FALSE) { # \dontrun{
classifier <- module(signature("text -> sentiment"))
trainset <- data.frame(
  text = c("I love it!", "Terrible experience"),
  sentiment = c("positive", "negative")
)

optimized <- classifier |>
  compile(LabeledFewShot(k = 2L), trainset)
} # }
```
