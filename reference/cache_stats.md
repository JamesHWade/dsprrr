# Get Cache Statistics

Get statistics about cache usage including hit rate, entry counts, and
sizes.

## Usage

``` r
cache_stats()
```

## Value

A list with cache statistics:

- `enabled`: Logical, whether caching is enabled

- `hits`: Integer, number of cache hits

- `misses`: Integer, number of cache misses

- `hit_rate`: Numeric, proportion of requests served from cache

- `memory_entries`: Integer, entries in memory cache (if available)

- `disk_entries`: Integer, entries in disk cache (if available)

## Examples

``` r
if (FALSE) { # \dontrun{
# Check cache performance
stats <- cache_stats()
stats$hit_rate
} # }
```
