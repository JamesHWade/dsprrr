# Use a chat for a block of code

`with_lm()` evaluates `code` with `lm` as the chat for every dsprrr call
in it that does not name one, like DSPy's `dspy.context(lm = ...)`.
`local_lm()` does the same until the calling function returns. Either
way, the previous chat is restored afterwards, also after an error.

## Usage

``` r
with_lm(lm, code)

local_lm(lm, .env = parent.frame())
```

## Arguments

- lm:

  An ellmer Chat. For `local_lm()`, `NULL` removes any scoped chat until
  the calling function returns.

- code:

  Code to evaluate with `lm` as the scoped chat.

- .env:

  The environment whose exit ends the scope: by default, the function
  that calls `local_lm()` (cleanup uses
  [`withr::defer()`](https://withr.r-lib.org/reference/defer.html)).

## Value

`with_lm()` returns the value of `code`. `local_lm()` returns the
previous scoped chat (or `NULL`) invisibly.

## Details

The scoped chat ranks below the `.llm` argument and below a chat stored
on the module, and above the default chat; see
[`get_default_chat()`](https://jameshwade.github.io/dsprrr/reference/get_default_chat.md)
for the full order. A module created with a `chat` argument therefore
keeps using its own chat inside `with_lm()`. Blocks can be nested; the
innermost chat wins.

## See also

Other configuration:
[`cache_stats()`](https://jameshwade.github.io/dsprrr/reference/cache_stats.md),
[`clear_cache()`](https://jameshwade.github.io/dsprrr/reference/clear_cache.md),
[`configure_cache()`](https://jameshwade.github.io/dsprrr/reference/configure_cache.md),
[`dsp_configure()`](https://jameshwade.github.io/dsprrr/reference/dsp_configure.md),
[`dsprrr_sitrep()`](https://jameshwade.github.io/dsprrr/reference/dsprrr_sitrep.md),
[`get_default_chat()`](https://jameshwade.github.io/dsprrr/reference/get_default_chat.md),
[`is_reasoning_model()`](https://jameshwade.github.io/dsprrr/reference/is_reasoning_model.md)

## Examples

``` r
# Building chats makes no request, so this runs without an API key
fast <- ellmer::chat_openai(
  model = "gpt-6-luna",
  params = ellmer::params(reasoning_effort = "low")
)
careful <- ellmer::chat_openai(
  model = "gpt-6-luna",
  params = ellmer::params(reasoning_effort = "high")
)

with_lm(fast, identical(get_default_chat(), fast))
#> [1] TRUE
with_lm(fast, with_lm(careful, identical(get_default_chat(), careful)))
#> [1] TRUE

review <- function() {
  local_lm(careful)
  # every dsprrr call from here to the end of the function uses `careful`
  identical(get_default_chat(), careful)
}
review()
#> [1] TRUE

# Outside the block and the function, no scoped chat is left
identical(get_default_chat(create = FALSE), careful)
#> [1] FALSE

if (FALSE) { # \dontrun{
summarize <- module(signature("text -> summary"))
with_lm(fast, run(summarize, text = "A long article ..."))
} # }
```
