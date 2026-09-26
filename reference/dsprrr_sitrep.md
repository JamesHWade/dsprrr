# Report dsprrr's configuration

`dsprrr_sitrep()` prints what dsprrr will use and what it has done this
session: package versions, the default chat, which API keys are set, the
prompt history, dsprrr options and the response cache. It is modelled on
`usethis::git_sitrep()` and is a good first step when calls do not
behave as expected.

## Usage

``` r
dsprrr_sitrep()
```

## Value

A list, invisibly, with `dsprrr_version`, `ellmer_version`,
`has_default_chat`, `provider` and `model` of the default chat,
`api_keys` (whether `OPENAI_API_KEY`, `ANTHROPIC_API_KEY` and
`GOOGLE_API_KEY` are set), `n_calls` and `prompt_history_count` (entries
in the prompt history), `prompt_history_max`, and the cache settings and
state (`cache_enabled`, `cache_disk_path`, `cache_disk_private`,
`cache_degraded`, `cache_privacy_status` and, when caching is on,
`cache_stats`). Once calls have been recorded, it also has
`total_tokens_in`, `total_tokens_out` and `total_cost`.

## Details

The default chat shown is the one
[`get_default_chat()`](https://jameshwade.github.io/dsprrr/reference/get_default_chat.md)
returns with `create = FALSE`, so a chat that would be created from an
API key on first use is reported as not configured. The report makes no
model calls.

## See also

Other configuration:
[`cache_stats()`](https://jameshwade.github.io/dsprrr/reference/cache_stats.md),
[`clear_cache()`](https://jameshwade.github.io/dsprrr/reference/clear_cache.md),
[`configure_cache()`](https://jameshwade.github.io/dsprrr/reference/configure_cache.md),
[`dsp_configure()`](https://jameshwade.github.io/dsprrr/reference/dsp_configure.md),
[`get_default_chat()`](https://jameshwade.github.io/dsprrr/reference/get_default_chat.md),
[`is_reasoning_model()`](https://jameshwade.github.io/dsprrr/reference/is_reasoning_model.md),
[`with_lm()`](https://jameshwade.github.io/dsprrr/reference/with_lm.md)

## Examples

``` r
status <- dsprrr_sitrep()
#> 
#> ── dsprrr configuration ────────────────────────────────────────────────────────
#> 
#> ── Packages ──
#> 
#> ✔ ellmer 0.5.0 (OK)
#> ✔ dsprrr 0.0.0.9000
#> 
#> 
#> ── Default Chat ──
#> 
#> ✖ Not configured
#> Run `dsp_configure()` or set an API key
#> 
#> 
#> ── API Keys ──
#> 
#> ✖ OPENAI_API_KEY
#> ✖ ANTHROPIC_API_KEY
#> ✖ GOOGLE_API_KEY
#> 
#> 
#> ── Session State ──
#> 
#> • Prompt history: 0 / 100 entries
#> 
#> 
#> ── Options ──
#> 
#> Using defaults (no options set)
#> 
#> 
#> ── Cache ──
#> 
#> ✔ Cache tiers: memory, disk
#> • Disk path: /home/runner/.cache/R/dsprrr
#> ℹ Private disk permissions will be checked on first use
#> ℹ No cache activity yet
#> 
status$has_default_chat
#> [1] FALSE
```
