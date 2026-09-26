# Report response-cache statistics

`cache_stats()` reports whether caching is on, how many requests were
served from the cache (hits) or sent to the model (misses) since the
cache was last cleared, and how many responses each tier holds.

## Usage

``` r
cache_stats()
```

## Value

A list of class `dsprrr_cache_stats` with `enabled`, `hits`, `misses`,
`hit_rate` (hits as a share of all requests), and `memory_entries` and
`disk_entries` once those tiers are in use. If the disk cache failed its
privacy checks, also `degraded = TRUE` and `degraded_reason`. Printing
it gives a short report.

## See also

Other configuration:
[`clear_cache()`](https://jameshwade.github.io/dsprrr/reference/clear_cache.md),
[`configure_cache()`](https://jameshwade.github.io/dsprrr/reference/configure_cache.md),
[`dsp_configure()`](https://jameshwade.github.io/dsprrr/reference/dsp_configure.md),
[`dsprrr_sitrep()`](https://jameshwade.github.io/dsprrr/reference/dsprrr_sitrep.md),
[`get_default_chat()`](https://jameshwade.github.io/dsprrr/reference/get_default_chat.md),
[`is_reasoning_model()`](https://jameshwade.github.io/dsprrr/reference/is_reasoning_model.md),
[`with_lm()`](https://jameshwade.github.io/dsprrr/reference/with_lm.md)

## Examples

``` r
stats <- cache_stats()
stats
#> 
#> ── dsprrr Cache Statistics 
#> • Hit rate: 0%
#> • Hits: 0
#> • Misses: 0
stats$hit_rate
#> [1] 0
```
