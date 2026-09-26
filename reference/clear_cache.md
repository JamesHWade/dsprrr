# Clear the response cache

`clear_cache()` deletes cached model responses from memory, from disk or
both, and resets the hit and miss counts reported by
[`cache_stats()`](https://jameshwade.github.io/dsprrr/reference/cache_stats.md).

## Usage

``` r
clear_cache(which = c("all", "memory", "disk"))
```

## Arguments

- which:

  Which tier to clear: `"all"` (the default), `"memory"` or `"disk"`.

## Value

`TRUE`, invisibly. If a tier cannot be cleaned up, the cache is still
detached and an error lists the tiers that failed.

## Details

The disk tier is cleared only if it has been opened in this session,
which happens at the first cached model call. In a fresh session,
`clear_cache()` leaves the directory on disk alone; delete the directory
shown by
[`dsprrr_sitrep()`](https://jameshwade.github.io/dsprrr/reference/dsprrr_sitrep.md)
to remove it.

## See also

Other configuration:
[`cache_stats()`](https://jameshwade.github.io/dsprrr/reference/cache_stats.md),
[`configure_cache()`](https://jameshwade.github.io/dsprrr/reference/configure_cache.md),
[`dsp_configure()`](https://jameshwade.github.io/dsprrr/reference/dsp_configure.md),
[`dsprrr_sitrep()`](https://jameshwade.github.io/dsprrr/reference/dsprrr_sitrep.md),
[`get_default_chat()`](https://jameshwade.github.io/dsprrr/reference/get_default_chat.md),
[`is_reasoning_model()`](https://jameshwade.github.io/dsprrr/reference/is_reasoning_model.md),
[`with_lm()`](https://jameshwade.github.io/dsprrr/reference/with_lm.md)

## Examples

``` r
clear_cache("memory")
cache_stats()
#> 
#> ── dsprrr Cache Statistics 
#> • Hit rate: 0%
#> • Hits: 0
#> • Misses: 0

if (FALSE) { # \dontrun{
# Delete every cached response, on disk too
clear_cache()
} # }
```
