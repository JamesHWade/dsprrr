# Run a module asynchronously

Executes a module and returns a promise that resolves to the result.
Useful for running multiple modules in parallel.

## Usage

``` r
run_async(module, ..., .llm = NULL)
```

## Arguments

- module:

  A dsprrr Module object

- ...:

  Named inputs matching the module's signature

- .llm:

  Optional ellmer Chat object

## Value

A promise that resolves to the structured output

## Examples

``` r
if (FALSE) { # \dontrun{
# Run multiple modules in parallel
promises <- list(
  run_async(mod1, question = "Q1"),
  run_async(mod2, question = "Q2"),
  run_async(mod3, question = "Q3")
)

# Wait for all to complete
promises::promise_all(.list = promises) |>
  promises::then(function(results) {
    # Process results
  })
} # }
```
