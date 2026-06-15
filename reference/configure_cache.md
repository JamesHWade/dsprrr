# Configure dsprrr Cache

Configure the caching behavior for LLM responses. By default, both
memory and disk caching are enabled.

## Usage

``` r
configure_cache(
  enable = TRUE,
  enable_memory = TRUE,
  enable_disk = TRUE,
  disk_path = default_disk_cache_path(),
  memory_max_entries = 1000L,
  disk_max_size = 500 * 1024^2,
  disk_max_age = Inf
)
```

## Arguments

- enable:

  Logical. Master switch to enable/disable all caching. Default `TRUE`.

- enable_memory:

  Logical. Enable in-memory LRU cache. Default `TRUE`.

- enable_disk:

  Logical. Enable persistent disk cache. Default `TRUE`.

- disk_path:

  Character. Path for disk cache directory. Default `".dsprrr_cache"`.

- memory_max_entries:

  Integer. Maximum entries in memory cache. Default `1000L`.

- disk_max_size:

  Numeric. Maximum disk cache size in bytes. Default `500 * 1024^2`
  (500MB).

- disk_max_age:

  Numeric. Maximum age in seconds for disk cache entries. Default `Inf`
  (no age limit).

## Value

Invisibly returns the previous cache configuration as a list.

## Details

The cache stores parsed LLM responses (lists, tibbles, vectors) keyed by
a hash of the prompt, model, temperature, and output type. This avoids
redundant API calls during development and optimization.

**Environment variable**: Set `DSPRRR_CACHE_ENABLED=false` (or `0`,
`no`, `off`) to globally disable caching, useful for CI/testing
environments.

**Git**: If using disk caching, add `.dsprrr_cache/` to your
`.gitignore`:

    # dsprrr LLM response cache
    .dsprrr_cache/

## Examples

``` r
if (FALSE) { # \dontrun{
# Use defaults (caching enabled)
configure_cache()

# Disable disk cache (memory only)
configure_cache(enable_disk = FALSE)

# Disable all caching
configure_cache(enable = FALSE)

# Custom disk location and size
configure_cache(
  disk_path = "~/.dsprrr_cache",
  disk_max_size = 1024^3  # 1GB
)
} # }
```
