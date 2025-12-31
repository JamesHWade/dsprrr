# Get default parameters for a provider

Returns sensible default parameter values and capability flags for
different LLM providers.

## Usage

``` r
provider_defaults(provider)
```

## Arguments

- provider:

  Character string identifying the provider (e.g., "openai",
  "anthropic", "google").

## Value

A named list with default values and capabilities.

## Examples

``` r
provider_defaults("openai")
#> $temperature
#> [1] 0.7
#> 
#> $supports_json_schema
#> [1] TRUE
#> 
#> $supports_reasoning
#> [1] TRUE
#> 
provider_defaults("anthropic")
#> $temperature
#> [1] 1
#> 
#> $supports_json_schema
#> [1] TRUE
#> 
#> $supports_extended_thinking
#> [1] TRUE
#> 
```
