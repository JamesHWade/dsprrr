# dsprrr Situation Report

Displays a comprehensive overview of your dsprrr configuration,
including API keys, default chat settings, prompt history, and package
versions. Inspired by `usethis::git_sitrep()`.

## Usage

``` r
dsprrr_sitrep()
```

## Value

Invisibly returns a list with configuration details:

- `has_default_chat`: Logical, whether a default chat is configured

- `provider`: Character, name of the default provider

- `model`: Character, name of the default model

- `api_keys`: Named list of API key availability (logical)

- `n_calls`: Integer, number of LLM calls this session

- `prompt_history_count`: Integer, entries in prompt history

- `prompt_history_max`: Integer, maximum history size

- `ellmer_version`: Character, installed ellmer version

- `dsprrr_version`: Character, installed dsprrr version

## Examples

``` r
if (FALSE) { # \dontrun{
dsprrr_sitrep()
#> dsprrr configuration
#> --------------------------------------------------------
#>
#> -- Packages --
#> [OK] ellmer 0.2.0
#> [OK] dsprrr 0.1.0
#>
#> -- Default Chat --
#> [OK] OpenAI (gpt-4o-mini)
#>   Source: Auto-detected from OPENAI_API_KEY
#>
#> -- API Keys --
#> [OK] OPENAI_API_KEY
#> [OK] ANTHROPIC_API_KEY
#> [missing] GOOGLE_API_KEY
#>
#> -- Session State --
#> • Prompt history: 12 / 100 entries
#> • LLM calls: 15
#> • Tokens: 2,450 in / 890 out
#>
#> -- Options --
#> • dsprrr.verbose: TRUE
#> • dsprrr.quiet: FALSE
} # }
```
