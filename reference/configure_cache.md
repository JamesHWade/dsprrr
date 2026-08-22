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
  disk_private = TRUE,
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

  Character. Path for disk cache directory. Defaults to
  `tools::R_user_dir("dsprrr", "cache")`, unless overridden by
  `DSPRRR_CACHE_PATH`.

- disk_private:

  Logical. Enforce private cache storage. On Unix, require effective
  ownership and exact private POSIX modes for the directory and response
  files, plus root-or-effective ownership for every existing ancestor.
  On Windows, use inherited ACLs and report privacy as unverified. Set
  to `FALSE` only for an explicitly trusted shared cache. Default
  `TRUE`.

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

The cache stores versioned envelopes containing parsed LLM responses
and, when needed, semantic conversation-turn deltas used to restore an
ellmer Chat after a cache hit. Although cache keys hash request
identity, envelope values may contain raw request content and model
outputs. Treat persistent cache files as sensitive data.

**Disk privacy**: By default, the disk cache uses the platform-specific
per-user cache directory. On Unix, dsprrr verifies effective ownership,
canonical path identity, a cache directory with exactly mode `0700`, and
response files with exactly mode `0600` before serialized reads and
writes. Every existing ancestor must be owned by root or the effective
user, including sticky shared parents. Unsafe disk caches fall back to
memory when enabled; otherwise no cache tier remains active. On Windows,
the per-user directory inherits the account's filesystem ACLs; base R
cannot verify that those ACLs are owner-only. Set `disk_private = FALSE`
only for a cache whose writers and readers are all trusted.

Existing Unix caches must already use exactly mode `0700` for the
directory and `0600` for every response file; special mode bits are
rejected. Caches with different modes, untrusted ancestors, symbolic
links, non-regular filesystem entries, or unverifiable ownership are not
changed or read; dsprrr uses memory caching when enabled and otherwise
runs uncached. A shared writable cache could replace an RDS response
envelope and must be treated as untrusted serialized input.

POSIX modes cannot describe every filesystem policy. dsprrr does not
inspect extended ACLs, administrators can still access owner files, and
some network filesystems do not honor local mode changes. A same-account
process can also race path checks and file opens; dsprrr checks identity
before and after I/O but base R does not expose descriptor-level
`openat()`/`fstat()` guarantees. Avoid shared or network cache paths for
sensitive workloads. In CI, disable caching with
`DSPRRR_CACHE_ENABLED=false` or use a job-specific `DSPRRR_CACHE_PATH`.

**Environment variable**: Set `DSPRRR_CACHE_ENABLED=false` (or `0`,
`no`, `off`) to globally disable caching, useful for CI/testing
environments.

**Git**: The default cache is outside the project. If you explicitly use
a project-local path, add it to `.gitignore`, for example:

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

# Trusted shared caches require an explicit privacy opt-out
configure_cache(
  disk_path = "/srv/trusted-team/dsprrr-cache",
  disk_private = FALSE
)
} # }
```
