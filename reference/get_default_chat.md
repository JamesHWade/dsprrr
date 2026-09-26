# Get, set or clear the default chat

`get_default_chat()` returns the ellmer Chat that dsprrr uses when
neither the call nor the module names one. `set_default_chat()` sets it
for the rest of the session;
[`dsp_configure()`](https://jameshwade.github.io/dsprrr/reference/dsp_configure.md)
does the same from a provider name. `clear_default_chat()` forgets a
chat that was created from an API key, so the next call detects one
again, for example after you change the key.

## Usage

``` r
get_default_chat(create = TRUE)

set_default_chat(chat)

clear_default_chat()
```

## Arguments

- create:

  If `TRUE` (the default), create a chat from an API key when none is
  set (step 5), and error if that fails. If `FALSE`, return `NULL`
  instead of creating one.

- chat:

  An ellmer Chat, or `NULL` to remove the default set earlier.

## Value

`get_default_chat()` returns an ellmer Chat, or `NULL` when
`create = FALSE` and none is set. `set_default_chat()` returns the
previous default (or `NULL`) invisibly, and `clear_default_chat()`
returns `NULL` invisibly.

## Details

### How dsprrr chooses a chat

Each model call uses the first of these that is available:

1.  The `.llm` argument of
    [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md),
    [`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md),
    [`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md),
    [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
    and the other functions that call models.

2.  The chat stored on the module, from the `chat` argument of
    [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)
    and the other constructors.

3.  A chat set for a block of code with
    [`with_lm()`](https://jameshwade.github.io/dsprrr/reference/with_lm.md)
    or
    [`local_lm()`](https://jameshwade.github.io/dsprrr/reference/with_lm.md).

4.  The default chat set with `set_default_chat()` or
    [`dsp_configure()`](https://jameshwade.github.io/dsprrr/reference/dsp_configure.md)
    (stored in `options(dsprrr.default_chat)`).

5.  A chat created from the first API key found in the environment:
    `OPENAI_API_KEY` gives
    [`ellmer::chat_openai()`](https://ellmer.tidyverse.org/reference/chat_openai.html),
    `ANTHROPIC_API_KEY` gives
    [`ellmer::chat_anthropic()`](https://ellmer.tidyverse.org/reference/chat_anthropic.html)
    and `GOOGLE_API_KEY` gives
    [`ellmer::chat_google_gemini()`](https://ellmer.tidyverse.org/reference/chat_google_gemini.html),
    each with the provider's default model. It is created once, with a
    message naming the provider and model (set
    `options(dsprrr.quiet = TRUE)` to silence it), and reused until
    `clear_default_chat()`.

If none is available, the call fails with an error that explains how to
set one up. `get_default_chat()` returns the first of steps 3 to 5.

## See also

Other configuration:
[`cache_stats()`](https://jameshwade.github.io/dsprrr/reference/cache_stats.md),
[`clear_cache()`](https://jameshwade.github.io/dsprrr/reference/clear_cache.md),
[`configure_cache()`](https://jameshwade.github.io/dsprrr/reference/configure_cache.md),
[`dsp_configure()`](https://jameshwade.github.io/dsprrr/reference/dsp_configure.md),
[`dsprrr_sitrep()`](https://jameshwade.github.io/dsprrr/reference/dsprrr_sitrep.md),
[`is_reasoning_model()`](https://jameshwade.github.io/dsprrr/reference/is_reasoning_model.md),
[`with_lm()`](https://jameshwade.github.io/dsprrr/reference/with_lm.md)

## Examples

``` r
# Building a chat makes no request, so this runs without an API key
luna <- ellmer::chat_openai(model = "gpt-6-luna")
previous <- set_default_chat(luna)
get_default_chat()$get_model()
#> [1] "gpt-6-luna"

# Restore the previous default (here: none)
set_default_chat(previous)

# Forget a chat detected from an API key
clear_default_chat()
```
