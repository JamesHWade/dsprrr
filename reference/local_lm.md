# Set Local LM Override

Sets a scoped LLM override that lasts until the calling function exits.
Useful for functions that need to ensure a specific LLM is used for all
internal dsprrr calls.

## Usage

``` r
local_lm(lm, .env = parent.frame())
```

## Arguments

- lm:

  An ellmer Chat object to use as the default LLM, or `NULL` to clear
  any scoped override.

- .env:

  The environment to scope to (defaults to caller's environment).

## Value

Invisibly returns the previous scoped LM (if any).

## Details

This function uses
[`withr::defer()`](https://withr.r-lib.org/reference/defer.html) to
ensure cleanup when the calling function exits, even if an error occurs.
For a block-based alternative, see
[`with_lm()`](https://jameshwade.github.io/dsprrr/reference/with_lm.md).

## Examples

``` r
if (FALSE) { # \dontrun{
my_analysis <- function(data) {
  # All dsprrr calls in this function will use Claude
  local_lm(ellmer::chat_claude())

  # These calls don't need .llm parameter
  summary <- run(module(signature("data -> summary")), data = data)
  insights <- run(
    module(signature("summary -> insights")),
    summary = summary
  )
  insights
}

# The scoped LM is automatically cleared when my_analysis() returns
result <- my_analysis(my_data)
} # }
```
