# dsprrr Situation Report

Displays the current dsprrr configuration, available API keys, and
session usage statistics. Inspired by `usethis::git_sitrep()`.

## Usage

``` r
dsprrr_sitrep()
```

## Value

Invisibly returns a list with configuration details.

## Examples

``` r
if (FALSE) { # \dontrun{
dsprrr_sitrep()
#> dsprrr configuration
#> --------------------
#>
#> Default provider: OpenAI (gpt-4o-mini)
#> Source: Auto-detected from OPENAI_API_KEY
#>
#> API keys found:
#>   OPENAI_API_KEY
#>   ANTHROPIC_API_KEY
#>
#> Session usage:
#>   LLM calls: 12
#>   Tokens: 2,450 in / 890 out
#>   Est. cost: $0.02
} # }
```
