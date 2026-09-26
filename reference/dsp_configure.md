# Configure the default chat from a provider name

`dsp_configure()` builds an ellmer Chat for a provider and makes it the
default chat, like DSPy's `dspy.configure(lm = ...)`. Modules use it
when neither the call nor the module names a chat; see
[`get_default_chat()`](https://jameshwade.github.io/dsprrr/reference/get_default_chat.md)
for the full order.

## Usage

``` r
dsp_configure(
  provider = NULL,
  model = NULL,
  api_key = NULL,
  temperature = NULL,
  ...
)
```

## Arguments

- provider:

  Character string specifying the provider. One of: `"openai"`,
  `"anthropic"`, `"google"`. If `NULL` (default), uses the first
  provider whose API key is set: `OPENAI_API_KEY`, `ANTHROPIC_API_KEY`,
  then `GOOGLE_API_KEY`.

- model:

  Character string specifying the model name. If `NULL`, uses the
  provider's default model.

- api_key:

  Character string with the API key. If `NULL`, reads from the
  appropriate environment variable.

- temperature:

  Sampling temperature, applied through
  `ellmer::params(temperature = )`. Default `NULL` uses the provider
  default. Reasoning models may ignore or reject it: gpt-6-luna accepts
  it only with `params = ellmer::params(reasoning_effort = "none")`.

- ...:

  Additional arguments passed to the ellmer chat constructor
  ([`ellmer::chat_openai()`](https://ellmer.tidyverse.org/reference/chat_openai.html),
  [`ellmer::chat_anthropic()`](https://ellmer.tidyverse.org/reference/chat_anthropic.html)
  or
  [`ellmer::chat_google_gemini()`](https://ellmer.tidyverse.org/reference/chat_google_gemini.html)),
  such as `params` or `system_prompt`.

## Value

The new default Chat, invisibly.

## See also

Other configuration:
[`cache_stats()`](https://jameshwade.github.io/dsprrr/reference/cache_stats.md),
[`clear_cache()`](https://jameshwade.github.io/dsprrr/reference/clear_cache.md),
[`configure_cache()`](https://jameshwade.github.io/dsprrr/reference/configure_cache.md),
[`dsprrr_sitrep()`](https://jameshwade.github.io/dsprrr/reference/dsprrr_sitrep.md),
[`get_default_chat()`](https://jameshwade.github.io/dsprrr/reference/get_default_chat.md),
[`is_reasoning_model()`](https://jameshwade.github.io/dsprrr/reference/is_reasoning_model.md),
[`with_lm()`](https://jameshwade.github.io/dsprrr/reference/with_lm.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Configure with auto-detection (uses env vars)
dsp_configure()

# Configure with specific provider and model
dsp_configure(provider = "openai", model = "gpt-6-luna")

# Configure with temperature
dsp_configure(provider = "anthropic", temperature = 0.7)

# gpt-6-luna takes a temperature only with reasoning turned off
dsp_configure(
  provider = "openai",
  model = "gpt-6-luna",
  temperature = 0,
  params = ellmer::params(reasoning_effort = "none")
)

# Now run() uses this configuration
run(module(signature("question -> answer")), question = "What is 2+2?")
} # }
```
