# Configure the response cache

dsprrr caches model responses, so repeating a request returns the stored
answer without calling the model. That makes re-running code faster and
cheaper. There are two tiers, both on by default: a memory cache for the
session and a disk cache that lasts across sessions. `configure_cache()`
changes the settings for the rest of the session.

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

  Turn all caching on or off.

- enable_memory:

  Use the memory cache.

- enable_disk:

  Use the disk cache.

- disk_path:

  Directory for the disk cache. Defaults to
  `tools::R_user_dir("dsprrr", "cache")`, or `DSPRRR_CACHE_PATH` when
  set.

- disk_private:

  If `TRUE` (the default), enforce the ownership and permission checks
  described under Privacy. `FALSE` is only for a trusted shared cache.

- memory_max_entries:

  Maximum number of responses in the memory cache (least recently used
  ones are dropped first).

- disk_max_size:

  Maximum size of the disk cache in bytes (500 MB by default).

- disk_max_age:

  Maximum age of disk entries in seconds. `Inf` (the default) keeps them
  until they are pruned for size.

## Value

The previous settings, invisibly, as a list that can be passed back with
`do.call(configure_cache, old)`. `NULL` if the settings had not been
read yet in this session.

## Details

### What counts as the same request

A cached response is used only when everything that could change the
answer matches: provider, model, parameters such as temperature, system
prompt, conversation history, prompt and output schema.
[`best_of_n()`](https://jameshwade.github.io/dsprrr/reference/best_of_n.md),
[`refine()`](https://jameshwade.github.io/dsprrr/reference/refine.md)
and
[`with_assertions()`](https://jameshwade.github.io/dsprrr/reference/with_assertions.md)
attempts, and
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
epochs after the first, use separate cache partitions, so they get fresh
responses. Chats with registered tools are never cached.

To skip the cache for one call, pass `.cache = FALSE` to
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md),
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
or
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md).
To turn caching off for a whole environment, for example on CI, set the
environment variable `DSPRRR_CACHE_ENABLED=false` (or `0`, `no`, `off`);
`DSPRRR_CACHE_PATH` moves the default disk location.

### Privacy

Cache files hold the requests and the parsed responses, and, where
needed, the conversation turns used to restore a Chat after a cache hit.
Treat the disk cache as sensitive data.

The default disk location is the per-user cache directory from
[`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html). On Unix,
dsprrr reads and writes it only if the directory has mode `0700`, every
response file has mode `0600`, the effective user owns them, and every
existing parent directory belongs to root or that user. A cache that
fails these checks (other modes, special mode bits, symbolic links,
files that are not regular, or ownership that cannot be verified) is
neither changed nor read: dsprrr falls back to the memory cache if it is
enabled and otherwise runs uncached. On Windows, the directory inherits
the account's access rules, which base R cannot verify.

These checks cannot see extended ACLs, stop administrators, or cover
network file systems that ignore local modes, and a process running as
the same user could swap files between a check and a read. Avoid shared
or network cache paths for sensitive work. Set `disk_private = FALSE`
only for a cache whose readers and writers you all trust: a writable
shared cache could replace a stored response, which dsprrr reads back
with [`readRDS()`](https://rdrr.io/r/base/readRDS.html).

If you point the disk cache inside a project, add the directory (for
example `.dsprrr_cache/`) to `.gitignore`.

## See also

Other configuration:
[`cache_stats()`](https://jameshwade.github.io/dsprrr/reference/cache_stats.md),
[`clear_cache()`](https://jameshwade.github.io/dsprrr/reference/clear_cache.md),
[`dsp_configure()`](https://jameshwade.github.io/dsprrr/reference/dsp_configure.md),
[`dsprrr_sitrep()`](https://jameshwade.github.io/dsprrr/reference/dsprrr_sitrep.md),
[`get_default_chat()`](https://jameshwade.github.io/dsprrr/reference/get_default_chat.md),
[`is_reasoning_model()`](https://jameshwade.github.io/dsprrr/reference/is_reasoning_model.md),
[`with_lm()`](https://jameshwade.github.io/dsprrr/reference/with_lm.md)

## Examples

``` r
cache_stats()
#> 
#> ── dsprrr Cache Statistics 
#> • Hit rate: 0%
#> • Hits: 0
#> • Misses: 0

# Turn caching off for a while, then restore the previous settings
old <- configure_cache(enable = FALSE)
cache_stats()$enabled
#> [1] FALSE
do.call(configure_cache, old)
cache_stats()$enabled
#> [1] TRUE

if (FALSE) { # \dontrun{
# Keep responses in memory only
configure_cache(enable_disk = FALSE)

# A larger cache in another directory
configure_cache(disk_path = "~/.dsprrr_cache", disk_max_size = 1024^3)

# A shared cache that every user of the directory trusts
configure_cache(disk_path = "/srv/team/dsprrr-cache", disk_private = FALSE)
} # }
```
