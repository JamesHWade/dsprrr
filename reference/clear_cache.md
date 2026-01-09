# Clear dsprrr Cache

Clear cached LLM responses. Can clear memory cache, disk cache, or both.

## Usage

``` r
clear_cache(which = c("all", "memory", "disk"))
```

## Arguments

- which:

  Character. Which cache tier to clear: `"all"` (default), `"memory"`,
  or `"disk"`.

## Value

Invisibly returns `TRUE` on success.

## Examples

``` r
if (FALSE) { # \dontrun{
# Clear all caches
clear_cache()

# Clear only memory cache
clear_cache("memory")

# Clear only disk cache
clear_cache("disk")
} # }
```
