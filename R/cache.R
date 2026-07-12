#' LLM Response Caching
#'
#' @description
#' dsprrr provides automatic caching of LLM responses to speed up development
#' and reduce costs. The cache uses a two-tier architecture:
#' 1. **Memory cache**: Fast in-session LRU cache
#' 2. **Disk cache**: Persistent cache across R sessions
#'
#' Versioned cache envelopes can contain raw request content, parsed model
#' outputs, and semantic conversation-turn deltas. Persistent cache directories
#' must therefore be treated as sensitive storage; see [configure_cache()].
#'
#' @name cache
#' @keywords internal
NULL

# ── Cache Configuration ──────────────────────────────────────────────────────

#' Default on-disk cache directory
#'
#' Honors the `DSPRRR_CACHE_PATH` environment variable so the test suite (and
#' users with read-only working directories) can redirect the disk cache
#' without writing into the platform-specific per-user cache directory.
#' @noRd
default_disk_cache_path <- function() {
  # Treat an empty-string env var the same as unset, so DSPRRR_CACHE_PATH=""
  # does not resolve the cache to "" (which would write into the working dir).
  path <- Sys.getenv("DSPRRR_CACHE_PATH", unset = "")
  if (nzchar(path)) path.expand(path) else tools::R_user_dir("dsprrr", "cache")
}

#' Configure dsprrr Cache
#'
#' @description
#' Configure the caching behavior for LLM responses. By default, both memory
#' and disk caching are enabled.
#'
#' @details
#' The cache stores versioned envelopes containing parsed LLM responses and,
#' when needed, semantic conversation-turn deltas used to restore an ellmer
#' Chat after a cache hit. Although cache keys hash request identity, envelope
#' values may contain raw request content and model outputs. Treat persistent
#' cache files as sensitive data.
#'
#' **Disk privacy**: By default, the disk cache uses the platform-specific
#' per-user cache directory. On Unix, dsprrr verifies effective ownership,
#' canonical path identity, a `0700` cache directory, and `0600` response files
#' before serialized reads and writes. Unsafe disk caches fall back to memory
#' when enabled; otherwise no cache tier remains active.
#' On Windows, the per-user directory inherits the account's filesystem ACLs;
#' base R cannot verify that those ACLs are owner-only. Set `disk_private =
#' FALSE` only for a cache whose writers and readers are all trusted.
#'
#' Existing Unix caches that were readable but not writable by other accounts
#' are tightened before reuse. Caches that were writable by another account,
#' contain symbolic links or non-regular filesystem entries, or cannot be
#' verified are not read; dsprrr uses memory caching when enabled and otherwise
#' runs uncached. A shared writable cache could replace an RDS response envelope
#' and must be treated as untrusted serialized input.
#'
#' POSIX modes cannot describe every filesystem policy. dsprrr does not inspect
#' extended ACLs, administrators can still access owner files, and some network
#' filesystems do not honor local mode changes. A same-account process can also
#' race path checks and file opens; dsprrr checks identity before and after I/O
#' but base R does not expose descriptor-level `openat()`/`fstat()` guarantees.
#' Avoid shared or network cache paths for sensitive workloads. In CI, disable
#' caching with `DSPRRR_CACHE_ENABLED=false` or use a job-specific
#' `DSPRRR_CACHE_PATH`.
#'
#' **Environment variable**: Set `DSPRRR_CACHE_ENABLED=false` (or `0`, `no`,
#' `off`) to globally disable caching, useful for CI/testing environments.
#'
#' **Git**: The default cache is outside the project. If you explicitly use a
#' project-local path, add it to `.gitignore`, for example:
#' ```
#' # dsprrr LLM response cache
#' .dsprrr_cache/
#' ```
#'
#' @param enable Logical. Master switch to enable/disable all caching.
#'   Default `TRUE`.
#' @param enable_memory Logical. Enable in-memory LRU cache. Default `TRUE`.
#' @param enable_disk Logical. Enable persistent disk cache. Default `TRUE`.
#' @param disk_path Character. Path for disk cache directory.
#'   Defaults to `tools::R_user_dir("dsprrr", "cache")`, unless overridden by
#'   `DSPRRR_CACHE_PATH`.
#' @param disk_private Logical. Enforce private cache storage. On Unix, require
#'   effective ownership and private POSIX modes for the directory and response
#'   files. On Windows, use inherited ACLs and report privacy as unverified. Set
#'   to `FALSE` only for an explicitly trusted shared cache. Default `TRUE`.
#' @param memory_max_entries Integer. Maximum entries in memory cache.
#'   Default `1000L`.
#' @param disk_max_size Numeric. Maximum disk cache size in bytes.
#'   Default `500 * 1024^2` (500MB).
#' @param disk_max_age Numeric. Maximum age in seconds for disk cache entries.
#'   Default `Inf` (no age limit).
#'
#' @return Invisibly returns the previous cache configuration as a list.
#'
#' @export
#' @examples
#' \dontrun{
#' # Use defaults (caching enabled)
#' configure_cache()
#'
#' # Disable disk cache (memory only)
#' configure_cache(enable_disk = FALSE)
#'
#' # Disable all caching
#' configure_cache(enable = FALSE)
#'
#' # Custom disk location and size
#' configure_cache(
#'   disk_path = "~/.dsprrr_cache",
#'   disk_max_size = 1024^3  # 1GB
#' )
#'
#' # Trusted shared caches require an explicit privacy opt-out
#' configure_cache(
#'   disk_path = "/srv/trusted-team/dsprrr-cache",
#'   disk_private = FALSE
#' )
#' }
configure_cache <- function(
  enable = TRUE,
  enable_memory = TRUE,
  enable_disk = TRUE,
  disk_path = default_disk_cache_path(),
  disk_private = TRUE,
  memory_max_entries = 1000L,
  disk_max_size = 500 * 1024^2,
  disk_max_age = Inf
) {
  if (
    !is.logical(disk_private) ||
      length(disk_private) != 1L ||
      is.na(disk_private)
  ) {
    cli::cli_abort(
      "{.arg disk_private} must be a single non-missing logical value",
      class = "dsprrr_cache_config_error"
    )
  }

  # Store previous config for return
  old_config <- .dsprrr_env$cache_config

  # Build new config
  .dsprrr_env$cache_config <- list(
    enable = enable,
    enable_memory = enable_memory,
    enable_disk = enable_disk,
    disk_path = disk_path,
    disk_private = disk_private,
    memory_max_entries = as.integer(memory_max_entries),
    disk_max_size = disk_max_size,
    disk_max_age = disk_max_age
  )

  # Reset cache objects to force re-creation with new config
  .dsprrr_env$cache <- NULL
  .dsprrr_env$cache_memory <- NULL
  .dsprrr_env$cache_disk <- NULL
  .dsprrr_env$cache_disk_guard <- NULL

  # Reset degraded state - reconfiguration might fix previous issues
  .dsprrr_env$cache_degraded <- FALSE
  .dsprrr_env$cache_degraded_reason <- NULL
  .dsprrr_env$cache_privacy_status <- "not_checked"
  .dsprrr_env$cache_privacy_reason <- NULL

  invisible(old_config)
}

#' Clear dsprrr Cache
#'
#' @description
#' Clear cached LLM responses. Can clear memory cache, disk cache, or both.
#'
#' @param which Character. Which cache tier to clear: `"all"` (default),
#'   `"memory"`, or `"disk"`.
#'
#' @return Invisibly returns `TRUE` on success.
#'
#' @export
#' @examples
#' \dontrun{
#' # Clear all caches
#' clear_cache()
#'
#' # Clear only memory cache
#' clear_cache("memory")
#'
#' # Clear only disk cache
#' clear_cache("disk")
#' }
clear_cache <- function(which = c("all", "memory", "disk")) {
  which <- match.arg(which)

  memory_cache <- .dsprrr_env$cache_memory
  disk_cache <- .dsprrr_env$cache_disk
  disk_guard <- .dsprrr_env$cache_disk_guard

  # Detach internal handles before invoking user-space cachem methods. Cleanup
  # failures must never leave a stale layered object reachable from get_cache().
  if (which == "all") {
    .dsprrr_env$cache <- NULL
    .dsprrr_env$cache_memory <- NULL
    .dsprrr_env$cache_disk <- NULL
    .dsprrr_env$cache_disk_guard <- NULL
    .dsprrr_env$cache_degraded <- FALSE
    .dsprrr_env$cache_degraded_reason <- NULL
    .dsprrr_env$cache_privacy_status <- "not_checked"
    .dsprrr_env$cache_privacy_reason <- NULL
  } else if (which == "memory") {
    .dsprrr_env$cache <- disk_cache
    .dsprrr_env$cache_memory <- NULL
  } else {
    .dsprrr_env$cache <- memory_cache
    .dsprrr_env$cache_disk <- NULL
    .dsprrr_env$cache_disk_guard <- NULL
  }

  # Reset observable state before cleanup, so it remains reset even when a tier
  # method throws. Each requested tier is still attempted independently.
  .dsprrr_env$cache_stats <- list(hits = 0L, misses = 0L)
  .dsprrr_env$cache_first_hit_shown <- FALSE

  errors <- list()
  attempt <- function(tier, code) {
    tryCatch(
      {
        result <- force(code)
        if (identical(result, FALSE)) {
          stop("cleanup method reported failure")
        }
        result
      },
      error = function(error) {
        errors[[tier]] <<- error
        invisible(FALSE)
      }
    )
  }

  if (which %in% c("all", "memory") && !is.null(memory_cache)) {
    attempt("memory", memory_cache$reset())
  }
  if (which %in% c("all", "disk") && !is.null(disk_cache)) {
    disk_method <- if (which == "all") "destroy" else "reset"
    attempt("disk", {
      cache_verify_disk_cleanup_guard(disk_guard)
      disk_cache[[disk_method]]()
    })
  }

  # Tier methods may invoke guard callbacks that mutate package state. Restore
  # the detached state again before reporting any collected cleanup error.
  if (which == "all") {
    .dsprrr_env$cache <- NULL
    .dsprrr_env$cache_memory <- NULL
    .dsprrr_env$cache_disk <- NULL
    .dsprrr_env$cache_disk_guard <- NULL
    .dsprrr_env$cache_degraded <- FALSE
    .dsprrr_env$cache_degraded_reason <- NULL
    .dsprrr_env$cache_privacy_status <- "not_checked"
    .dsprrr_env$cache_privacy_reason <- NULL
  }
  .dsprrr_env$cache_stats <- list(hits = 0L, misses = 0L)
  .dsprrr_env$cache_first_hit_shown <- FALSE

  if (length(errors) > 0L) {
    details <- vapply(
      names(errors),
      function(tier) paste0(tier, ": ", conditionMessage(errors[[tier]])),
      character(1)
    )
    cli::cli_abort(
      c(
        "Cache state was cleared, but physical tier cleanup failed",
        "x" = "{details}"
      ),
      class = "dsprrr_cache_clear_error",
      cleanup_errors = errors
    )
  }

  invisible(TRUE)
}

#' Get Cache Statistics
#'
#' @description
#' Get statistics about cache usage including hit rate, entry counts,
#' and sizes.
#'
#' @return A list with cache statistics:
#'   - `enabled`: Logical, whether caching is enabled
#'   - `hits`: Integer, number of cache hits
#'   - `misses`: Integer, number of cache misses
#'   - `hit_rate`: Numeric, proportion of requests served from cache
#'   - `memory_entries`: Integer, entries in memory cache (if available)
#'   - `disk_entries`: Integer, entries in disk cache (if available)
#'
#' @export
#' @examples
#' \dontrun{
#' # Check cache performance
#' stats <- cache_stats()
#' stats$hit_rate
#' }
cache_stats <- function() {
  config <- get_cache_config()
  stats <- .dsprrr_env$cache_stats %||% list(hits = 0L, misses = 0L)

  total <- stats$hits + stats$misses
  hit_rate <- if (total > 0) stats$hits / total else 0

  result <- list(
    enabled = config$enable,
    hits = stats$hits,
    misses = stats$misses,
    hit_rate = hit_rate
  )

  # Add memory cache info if available
  if (!is.null(.dsprrr_env$cache_memory)) {
    result$memory_entries <- .dsprrr_env$cache_memory$size()
  }

  # Add disk cache info if available
  if (!is.null(.dsprrr_env$cache_disk)) {
    result$disk_entries <- .dsprrr_env$cache_disk$size()
  }

  structure(result, class = "dsprrr_cache_stats")
}

#' @export
print.dsprrr_cache_stats <- function(x, ...) {
  cli::cli_h3("dsprrr Cache Statistics")

  if (!x$enabled) {
    cli::cli_text("{.emph Caching is disabled}")
    return(invisible(x))
  }

  cli::cli_bullets(c(
    "*" = "Hit rate: {format(x$hit_rate * 100, digits = 1)}%",
    "*" = "Hits: {x$hits}",
    "*" = "Misses: {x$misses}"
  ))

  if (!is.null(x$memory_entries)) {
    cli::cli_bullets(c("*" = "Memory entries: {x$memory_entries}"))
  }

  if (!is.null(x$disk_entries)) {
    cli::cli_bullets(c("*" = "Disk entries: {x$disk_entries}"))
  }

  invisible(x)
}


# ── Internal Cache Functions ─────────────────────────────────────────────────

#' Get Cache Configuration
#'
#' @description
#' Get the current cache configuration, initializing defaults if needed.
#' Respects `DSPRRR_CACHE_ENABLED` environment variable (set to "false" or "0"
#' to disable caching globally, useful for CI/testing).
#'
#' @return A list with cache configuration.
#'
#' @noRd
get_cache_config <- function() {
  if (is.null(.dsprrr_env$cache_config)) {
    # Check environment variable for global disable
    env_enabled <- Sys.getenv("DSPRRR_CACHE_ENABLED", unset = "")
    env_disabled <- tolower(env_enabled) %in% c("false", "0", "no", "off")

    # Initialize with defaults
    .dsprrr_env$cache_config <- list(
      enable = !env_disabled,
      enable_memory = TRUE,
      enable_disk = TRUE,
      disk_path = default_disk_cache_path(),
      disk_private = TRUE,
      memory_max_entries = 1000L,
      disk_max_size = 500 * 1024^2,
      disk_max_age = Inf
    )
  }
  .dsprrr_env$cache_config
}

#' Get or Create Cache
#'
#' @description
#' Get the layered cache object, creating it if needed.
#'
#' @return A cachem cache object, or NULL if caching is disabled.
#'
#' @noRd
get_cache <- function() {
  config <- get_cache_config()

  if (!config$enable) {
    return(NULL)
  }

  # Return existing cache if available
  if (!is.null(.dsprrr_env$cache)) {
    return(.dsprrr_env$cache)
  }

  # A cleared or manually invalidated layered cache must never inherit stale
  # tier handles from an earlier configuration.
  .dsprrr_env$cache_memory <- NULL
  .dsprrr_env$cache_disk <- NULL
  .dsprrr_env$cache_disk_guard <- NULL

  # Build cache tiers
  caches <- list()

  # Memory tier
  if (config$enable_memory) {
    .dsprrr_env$cache_memory <- cachem::cache_mem(
      max_n = config$memory_max_entries
    )
    caches <- c(caches, list(.dsprrr_env$cache_memory))
  }

  # Disk tier
  if (config$enable_disk && !isTRUE(.dsprrr_env$cache_degraded)) {
    disk_cache <- create_disk_cache(config$disk_path, config)
    if (!is.null(disk_cache)) {
      .dsprrr_env$cache_disk <- disk_cache
      caches <- c(caches, list(disk_cache))
    }
  }

  # Create layered cache or use single tier
  if (length(caches) == 0) {
    .dsprrr_env$cache <- NULL
  } else if (length(caches) == 1) {
    .dsprrr_env$cache <- caches[[1]]
  } else {
    .dsprrr_env$cache <- cachem::cache_layered(caches[[1]], caches[[2]])
  }

  # Initialize stats
  if (is.null(.dsprrr_env$cache_stats)) {
    .dsprrr_env$cache_stats <- list(hits = 0L, misses = 0L)
  }

  .dsprrr_env$cache
}

#' Check if Caching is Enabled
#'
#' @description
#' Check whether caching is currently enabled.
#'
#' @return Logical.
#'
#' @noRd
cache_enabled <- function() {
  config <- get_cache_config()
  isTRUE(config$enable)
}

# Cache keys are an internal persistence format. Increment this whenever the
# fingerprint contract changes so old disk entries become unreachable.
cache_request_schema_version <- function() 3L

cache_envelope_schema_version <- function() 1L

#' Hash a value without retaining its contents in fingerprint material
#' @noRd
cache_opaque_value <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }

  serialized <- tryCatch(
    serialize(x, connection = NULL, version = 2),
    error = function(e) {
      cli::cli_abort(
        "Cannot safely fingerprint a value of class {.cls {class(x)[1]}}",
        class = "dsprrr_cache_fingerprint_error",
        parent = e
      )
    }
  )

  list(
    type = typeof(x),
    class = unname(class(x)),
    length = length(x),
    sha256 = digest::digest(serialized, algo = "sha256", serialize = FALSE)
  )
}

#' Determine whether a field contains authentication material
#' @noRd
cache_is_secret_name <- function(name) {
  compact <- gsub("[^a-z0-9]", "", tolower(name))
  compact %in%
    c(
      "apikey",
      "accesskey",
      "accesstoken",
      "authtoken",
      "token",
      "refreshtoken",
      "session",
      "sessionid",
      "sessiontoken",
      "bearer",
      "bearertoken",
      "authorization",
      "credential",
      "credentials",
      "secret",
      "clientsecret",
      "password",
      "cookie",
      "cookies",
      "privatekey"
    ) ||
    grepl(
      "(apikey|accesstoken|authtoken|refreshtoken|sessiontoken|bearertoken|authorization|credentials|clientsecret|password|privatekey)$",
      compact
    )
}

#' Determine whether a field partitions provider/account identity
#' @noRd
cache_is_account_name <- function(name) {
  compact <- gsub("[^a-z0-9]", "", tolower(name))
  compact %in%
    c(
      "account",
      "accountid",
      "organization",
      "organizationid",
      "org",
      "orgid",
      "project",
      "projectid",
      "tenant",
      "tenantid",
      "subscription",
      "subscriptionid",
      "workspace",
      "workspaceid"
    )
}

#' Resolve and hash credential/account fields without retaining their values
#' @noRd
cache_partition_records <- function(x, path = character()) {
  if (is.null(x)) {
    return(list())
  }

  if (is.atomic(x) && !is.null(names(x))) {
    x <- as.list(x)
  }

  if (is.list(x) && !is.null(names(x))) {
    records <- list()
    for (i in seq_along(x)) {
      name <- names(x)[[i]]
      value <- x[[i]]
      value_path <- c(path, name)
      if (cache_is_secret_name(name) || cache_is_account_name(name)) {
        if (is.function(value)) {
          if (length(formals(value)) != 0) {
            cli::cli_abort(
              "Credential getter {.field {name}} requires arguments",
              class = "dsprrr_cache_fingerprint_error"
            )
          }
          value <- tryCatch(
            value(),
            error = function(e) {
              cli::cli_abort(
                "Failed to resolve credential/account identity",
                class = "dsprrr_cache_fingerprint_error",
                parent = e
              )
            }
          )
        }
        records[[length(records) + 1L]] <- list(
          path = paste(value_path, collapse = "/"),
          value = cache_opaque_value(value)$sha256
        )
      } else {
        records <- c(records, cache_partition_records(value, value_path))
      }
    }
    return(records)
  }

  if (is.list(x)) {
    records <- list()
    for (i in seq_along(x)) {
      records <- c(
        records,
        cache_partition_records(x[[i]], c(path, as.character(i)))
      )
    }
    return(records)
  }

  list()
}

#' Build one opaque provider/account partition digest
#' @noRd
cache_account_partition <- function(x) {
  records <- cache_partition_records(x)
  if (length(records) == 0) {
    return(NULL)
  }
  paths <- vapply(records, `[[`, character(1), "path")
  records <- records[order(paths, seq_along(paths))]
  digest::digest(
    cache_fingerprint_json(records),
    algo = "sha256",
    serialize = FALSE
  )
}

#' Convert configuration to deterministic, content-safe plain R data
#'
#' Atomic values are represented by hashes. Named configuration lists are
#' sorted by name, while request/schema lists can preserve their original order.
#' @noRd
cache_config_fingerprint <- function(
  x,
  sort_named = TRUE,
  omit_secrets = FALSE
) {
  if (is.null(x)) {
    return(list(kind = "null"))
  }

  if (is.atomic(x) && is.null(names(x))) {
    return(c(list(kind = "atomic"), cache_opaque_value(x)))
  }

  if (is.atomic(x)) {
    x <- as.list(x)
  }

  if (is.data.frame(x)) {
    return(list(
      kind = "data_frame",
      rows = nrow(x),
      columns = lapply(seq_along(x), function(i) {
        list(
          name = names(x)[[i]],
          value = cache_config_fingerprint(x[[i]], sort_named = FALSE)
        )
      })
    ))
  }

  if (!is.list(x)) {
    cli::cli_abort(
      "Cannot safely fingerprint a value of class {.cls {class(x)[1]}}",
      class = "dsprrr_cache_fingerprint_error"
    )
  }

  item_names <- names(x)
  if (is.null(item_names)) {
    return(list(
      kind = "list",
      entries = lapply(
        x,
        cache_config_fingerprint,
        sort_named = sort_named,
        omit_secrets = omit_secrets
      )
    ))
  }

  indices <- seq_along(x)
  if (omit_secrets) {
    keep <- !vapply(item_names, cache_is_secret_name, logical(1))
    indices <- indices[keep]
  }
  if (sort_named && length(indices) > 1) {
    indices <- indices[order(item_names[indices], indices)]
  }

  list(
    kind = "named_list",
    entries = lapply(indices, function(i) {
      list(
        name = item_names[[i]],
        value = cache_config_fingerprint(
          x[[i]],
          sort_named = sort_named,
          omit_secrets = omit_secrets
        )
      )
    })
  )
}

#' Convert an ellmer output type to an exact recursive schema
#' @noRd
cache_output_schema <- function(output_type) {
  if (!is_ellmer_type(output_type)) {
    return(list(
      class = unname(class(output_type)),
      value = cache_config_fingerprint(output_type, sort_named = FALSE)
    ))
  }

  common <- list(
    class = class(output_type)[[1]],
    required = tryCatch(output_type@required, error = function(e) NULL),
    description = cache_opaque_value(
      tryCatch(output_type@description, error = function(e) NULL)
    )
  )

  if (inherits(output_type, "ellmer::TypeBasic")) {
    return(c(common, list(type = output_type@type)))
  }

  if (inherits(output_type, "ellmer::TypeEnum")) {
    return(c(common, list(values = cache_opaque_value(output_type@values))))
  }

  if (inherits(output_type, "ellmer::TypeArray")) {
    return(c(common, list(items = cache_output_schema(output_type@items))))
  }

  if (inherits(output_type, "ellmer::TypeObject")) {
    properties <- output_type@properties
    return(c(
      common,
      list(
        properties = lapply(seq_along(properties), function(i) {
          list(
            name = names(properties)[[i]],
            schema = cache_output_schema(properties[[i]])
          )
        }),
        additional_properties = output_type@additional_properties
      )
    ))
  }

  if (inherits(output_type, "ellmer::TypeJsonSchema")) {
    return(c(
      common,
      list(
        json = cache_config_fingerprint(output_type@json, sort_named = FALSE)
      )
    ))
  }

  if (inherits(output_type, "ellmer::TypeIgnore")) {
    return(common)
  }

  cli::cli_abort(
    "Unsupported ellmer output type {.cls {class(output_type)[1]}}",
    class = "dsprrr_cache_fingerprint_error"
  )
}

#' Serialize an output type to deterministic JSON
#' @noRd
serialize_output_type <- function(output_type) {
  cache_fingerprint_json(cache_output_schema(output_type))
}

#' Determine whether a value is an ellmer content object
#' @noRd
is_ellmer_content <- function(x) {
  inherits(x, "ellmer::Content") ||
    any(grepl("(^|::)Content", class(x)))
}

#' Fingerprint one ellmer content object
#' @noRd
cache_content_fingerprint <- function(content) {
  if (is.character(content)) {
    return(list(class = "text", text = cache_opaque_value(content)))
  }

  if (!is_ellmer_content(content)) {
    cli::cli_abort(
      "Unsupported request content {.cls {class(content)[1]}}",
      class = "dsprrr_cache_fingerprint_error"
    )
  }

  content_class <- class(content)[[1]]

  if (inherits(content, "ellmer::ContentText")) {
    return(list(class = content_class, text = cache_opaque_value(content@text)))
  }

  if (inherits(content, "ellmer::ContentThinking")) {
    return(list(
      class = content_class,
      thinking = cache_opaque_value(content@thinking),
      extra = cache_config_fingerprint(content@extra)
    ))
  }

  if (inherits(content, "ellmer::ContentJson")) {
    return(list(
      class = content_class,
      data = cache_config_fingerprint(content@data, sort_named = FALSE),
      string = cache_opaque_value(content@string)
    ))
  }

  if (inherits(content, "ellmer::ContentImageInline")) {
    return(list(
      class = content_class,
      type = content@type,
      data = cache_opaque_value(content@data)
    ))
  }

  if (inherits(content, "ellmer::ContentImageRemote")) {
    return(list(
      class = content_class,
      url = cache_opaque_value(content@url),
      detail = content@detail
    ))
  }

  if (inherits(content, "ellmer::ContentPDF")) {
    return(list(
      class = content_class,
      type = content@type,
      data = cache_opaque_value(content@data),
      filename = cache_opaque_value(content@filename)
    ))
  }

  if (inherits(content, "ellmer::ContentToolRequest")) {
    return(list(
      class = content_class,
      id = cache_opaque_value(content@id),
      name = content@name,
      arguments = cache_config_fingerprint(
        content@arguments,
        sort_named = FALSE
      ),
      extra = cache_config_fingerprint(content@extra, sort_named = FALSE)
    ))
  }

  if (inherits(content, "ellmer::ContentToolResult")) {
    request <- content@request
    request_identity <- if (is.null(request)) {
      NULL
    } else {
      list(
        id = cache_opaque_value(request@id),
        name = request@name,
        arguments = cache_config_fingerprint(
          request@arguments,
          sort_named = FALSE
        )
      )
    }
    return(list(
      class = content_class,
      value = cache_config_fingerprint(content@value, sort_named = FALSE),
      error = cache_opaque_value(content@error),
      extra = cache_config_fingerprint(content@extra, sort_named = FALSE),
      request = request_identity
    ))
  }

  props <- tryCatch(
    S7::props(content),
    error = function(e) {
      cli::cli_abort(
        "Cannot inspect request content {.cls {content_class}}",
        class = "dsprrr_cache_fingerprint_error",
        parent = e
      )
    }
  )
  props$tool <- NULL
  props$request <- NULL

  list(
    class = content_class,
    properties = cache_config_fingerprint(props, sort_named = FALSE)
  )
}

#' Fingerprint a request payload while retaining content boundaries
#' @noRd
cache_payload_fingerprint <- function(payload) {
  if (is.character(payload) || is_ellmer_content(payload)) {
    return(list(cache_content_fingerprint(payload)))
  }

  if (
    is.list(payload) &&
      all(vapply(payload, is_ellmer_content, logical(1)))
  ) {
    return(lapply(payload, cache_content_fingerprint))
  }

  cli::cli_abort(
    "Request payload contains unsupported content",
    class = "dsprrr_cache_fingerprint_error"
  )
}

#' Fingerprint one conversation turn
#' @noRd
cache_turn_fingerprint <- function(turn) {
  contents <- tryCatch(
    turn@contents,
    error = function(e) {
      cli::cli_abort(
        "Cannot inspect conversation turn {.cls {class(turn)[1]}}",
        class = "dsprrr_cache_fingerprint_error",
        parent = e
      )
    }
  )

  result <- list(
    class = class(turn)[[1]],
    role = tryCatch(turn@role, error = function(e) class(turn)[[1]]),
    contents = lapply(contents, cache_content_fingerprint)
  )

  if (inherits(turn, "ellmer::AssistantTurn")) {
    result$json <- cache_config_fingerprint(turn@json, sort_named = FALSE)
    result$finish_reason <- cache_opaque_value(turn@finish_reason)
  }

  result
}

#' Verify the canonical mutable-state contract supplied by ellmer::Chat
#' @noRd
cache_is_trusted_ellmer_chat <- function(llm) {
  if (
    !is.environment(llm) ||
      !R6::is.R6(llm) ||
      !inherits(llm, "Chat")
  ) {
    return(FALSE)
  }

  generator <- tryCatch(
    getFromNamespace("Chat", "ellmer"),
    error = function(e) NULL
  )
  enclosing <- tryCatch(llm$.__enclos_env__, error = function(e) NULL)
  if (
    is.null(generator) ||
      !is.environment(enclosing) ||
      !identical(parent.env(enclosing), asNamespace("ellmer"))
  ) {
    return(FALSE)
  }

  required <- c(
    "get_turns",
    "set_turns",
    "get_provider",
    "get_tools",
    "get_system_prompt",
    "clone",
    "chat_structured"
  )
  methods_match <- vapply(
    required,
    function(name) {
      method <- tryCatch(llm[[name]], error = function(e) NULL)
      canonical <- generator$public_methods[[name]]
      is.function(method) &&
        is.function(canonical) &&
        identical(environment(method), enclosing) &&
        identical(formals(method), formals(canonical)) &&
        identical(body(method), body(canonical))
    },
    logical(1)
  )
  if (!all(methods_match)) {
    return(FALSE)
  }

  provider <- tryCatch(llm$get_provider(), error = function(e) NULL)
  !is.null(provider) &&
    tryCatch(
      S7::S7_inherits(provider, ellmer::Provider),
      error = function(e) FALSE
    )
}

#' Require a Chat whose state can be inspected and replayed safely
#' @noRd
cache_require_trusted_ellmer_chat <- function(llm, error_class) {
  if (!cache_is_trusted_ellmer_chat(llm)) {
    cli::cli_abort(
      c(
        "Caching requires an unmodified {.cls ellmer::Chat}",
        "i" = "Opaque or custom Chat state cannot be fingerprinted or replayed safely."
      ),
      class = c("dsprrr_cache_untrusted_chat", error_class)
    )
  }
  invisible(llm)
}

#' Call a Chat getter, failing closed when required state is unavailable
#' @noRd
cache_chat_get <- function(llm, name, default = NULL, required = FALSE) {
  getter <- tryCatch(llm[[name]], error = function(e) NULL)
  if (is.null(getter)) {
    if (isTRUE(required)) {
      cli::cli_abort(
        "Chat state inspection requires {.code {name}()}",
        class = "dsprrr_cache_fingerprint_error"
      )
    }
    return(default)
  }
  if (!is.function(getter)) {
    cli::cli_abort(
      "Chat member {.field {name}} is not callable",
      class = "dsprrr_cache_fingerprint_error"
    )
  }

  tryCatch(
    getter(),
    error = function(e) {
      cli::cli_abort(
        "Failed to inspect Chat state with {.code {name}()}",
        class = "dsprrr_cache_fingerprint_error",
        parent = e
      )
    }
  )
}

#' Fingerprint provider identity and output-affecting configuration
#' @noRd
cache_provider_fingerprint <- function(llm, llm_id = NULL) {
  provider <- cache_chat_get(
    llm,
    "get_provider",
    default = NULL,
    required = TRUE
  )

  if (!is.null(provider)) {
    props <- tryCatch(
      S7::props(provider),
      error = function(e) {
        cli::cli_abort(
          "Cannot inspect provider {.cls {class(provider)[1]}}",
          class = "dsprrr_cache_fingerprint_error",
          parent = e
        )
      }
    )
    provider_name <- props$name %||% class(provider)[[1]]
    model <- props$model %||% cache_chat_get(llm, "get_model", default = NULL)
    account_partition <- cache_account_partition(props)
    props <- props[!vapply(names(props), cache_is_secret_name, logical(1))]
    props$name <- NULL
    props$model <- NULL

    return(list(
      kind = "ellmer_provider",
      class = class(provider)[[1]],
      name = provider_name,
      model = model,
      account_partition = account_partition,
      config = cache_config_fingerprint(props, omit_secrets = TRUE)
    ))
  }

  cli::cli_abort(
    "Chat provider inspection returned no provider",
    class = "dsprrr_cache_fingerprint_error"
  )
}

#' Build a versioned fingerprint for an actual Chat request
#' @noRd
cache_request_fingerprint <- function(
  llm,
  payload,
  output_type,
  rollout_id = NULL,
  llm_id = NULL
) {
  cache_require_trusted_ellmer_chat(
    llm,
    error_class = "dsprrr_cache_fingerprint_error"
  )
  turns <- cache_chat_get(
    llm,
    "get_turns",
    default = NULL,
    required = TRUE
  )
  if (!is.list(turns)) {
    cli::cli_abort(
      "Chat history inspection must return a list of turns",
      class = "dsprrr_cache_fingerprint_error"
    )
  }
  tools <- cache_chat_get(
    llm,
    "get_tools",
    default = NULL,
    required = TRUE
  )
  if (!is.list(tools)) {
    cli::cli_abort(
      "Chat tool inspection must return a list",
      class = "dsprrr_cache_fingerprint_error"
    )
  }
  if (length(tools) > 0) {
    cli::cli_abort(
      c(
        "Caching is disabled for Chats with registered tools",
        "i" = "Tool implementations and side effects are not safely cacheable."
      ),
      class = c(
        "dsprrr_cache_tools_error",
        "dsprrr_cache_fingerprint_error"
      )
    )
  }
  system_prompt <- cache_chat_get(
    llm,
    "get_system_prompt",
    default = NULL,
    required = TRUE
  )

  list(
    version = cache_request_schema_version(),
    ellmer_version = as.character(utils::packageVersion("ellmer")),
    provider = cache_provider_fingerprint(llm, llm_id = llm_id),
    system_prompt = cache_opaque_value(system_prompt),
    history = lapply(turns, cache_turn_fingerprint),
    tools = list(),
    request = cache_payload_fingerprint(payload),
    output_schema = cache_output_schema(output_type),
    rollout_id = cache_opaque_value(
      if (is.null(rollout_id)) NULL else as.character(rollout_id)
    )
  )
}

#' Serialize a cache fingerprint to deterministic JSON
#' @noRd
cache_fingerprint_json <- function(fingerprint) {
  as.character(jsonlite::toJSON(
    fingerprint,
    auto_unbox = TRUE,
    null = "null",
    na = "string",
    digits = NA,
    pretty = FALSE
  ))
}

#' Compute Cache Key
#'
#' Retains the historical calling convention for internal callers and tests.
#' Runtime requests should provide the complete `fingerprint` built by
#' `cache_request_fingerprint()`.
#' @noRd
cache_key <- function(
  prompt,
  model,
  temperature = NULL,
  output_type,
  rollout_id = NULL,
  llm_id = NULL,
  fingerprint = NULL
) {
  if (is.null(fingerprint)) {
    fingerprint <- list(
      version = cache_request_schema_version(),
      request = cache_payload_fingerprint(prompt),
      provider = list(
        model = model,
        params = cache_config_fingerprint(list(temperature = temperature)),
        llm_id = llm_id
      ),
      output_schema = cache_output_schema(output_type),
      rollout_id = cache_opaque_value(
        if (is.null(rollout_id)) NULL else as.character(rollout_id)
      )
    )
  }

  digest::digest(
    cache_fingerprint_json(fingerprint),
    algo = "sha256",
    serialize = FALSE
  )
}

#' Create Disk Cache
#'
#' @description
#' Attempt to create a disk cache, handling directory creation and errors.
#'
#' @param disk_path Character path for disk cache
#' @param config Cache configuration list
#'
#' @return A disk cache object or NULL if creation failed
#'
#' @noRd
create_disk_cache <- function(disk_path, config) {
  prepared <- prepare_cache_directory(
    disk_path,
    private = isTRUE(config$disk_private)
  )
  if (!prepared$ok) {
    return(cache_disk_degrade(prepared$path, prepared$reason))
  }

  tryCatch(
    {
      private_guard <- if (
        isTRUE(config$disk_private) && cache_private_modes_supported()
      ) {
        new_cache_disk_guard(prepared$trust)
      } else {
        NULL
      }
      write_fn <- if (is.environment(private_guard)) {
        function(value, file) {
          tryCatch(
            write_private_cache_value(value, file, guard = private_guard),
            error = function(error) {
              cache_record_disk_guard_failure(
                private_guard,
                conditionMessage(error)
              )
              stop(error)
            }
          )
        }
      } else {
        NULL
      }
      read_fn <- if (is.environment(private_guard)) {
        function(file) read_private_cache_value(file, private_guard)
      } else {
        NULL
      }
      disk_cache <- cachem::cache_disk(
        dir = prepared$path,
        max_size = config$disk_max_size,
        max_age = config$disk_max_age,
        read_fn = read_fn,
        write_fn = write_fn
      )
      if (is.environment(private_guard)) {
        disk_cache <- guarded_cache_disk(disk_cache, private_guard)
        .dsprrr_env$cache_disk_guard <- private_guard
      } else {
        .dsprrr_env$cache_disk_guard <- NULL
      }
      .dsprrr_env$cache_degraded <- FALSE
      .dsprrr_env$cache_degraded_reason <- NULL
      disk_cache
    },
    error = function(e) {
      cache_disk_degrade(prepared$path, conditionMessage(e))
    }
  )
}

#' Whether POSIX owner-only modes can be enforced and verified
#' @noRd
cache_private_modes_supported <- function() {
  identical(.Platform$OS.type, "unix")
}

#' Resolve the effective POSIX user id without relying on shell commands
#' @noRd
cache_effective_owner_id <- function() {
  if (!cache_private_modes_supported()) {
    return(NA_integer_)
  }
  effective_user <- unname(Sys.info()[["effective_user"]])
  if (
    is.null(effective_user) || is.na(effective_user) || !nzchar(effective_user)
  ) {
    return(NA_integer_)
  }
  users <- tryCatch(fs::user_ids(), error = function(e) NULL)
  if (is.null(users)) {
    return(NA_integer_)
  }
  ids <- unique(users$user_id[users$user_name == effective_user])
  if (length(ids) != 1L || is.na(ids[[1]])) {
    return(NA_integer_)
  }
  as.integer(ids[[1]])
}

#' Read the POSIX owner id for one path
#' @noRd
cache_path_owner_id <- function(path) {
  info <- suppressWarnings(file.info(path, extra_cols = TRUE))
  if (nrow(info) != 1L || is.na(info$uid[[1]])) {
    return(NA_integer_)
  }
  as.integer(info$uid[[1]])
}

#' Verify paths are owned by the effective user
#' @noRd
cache_paths_owned_by_effective_user <- function(paths) {
  if (!cache_private_modes_supported() || length(paths) == 0L) {
    return(TRUE)
  }
  effective_owner <- cache_effective_owner_id()
  if (is.na(effective_owner)) {
    return(FALSE)
  }
  owners <- vapply(paths, cache_path_owner_id, integer(1))
  !anyNA(owners) && all(owners == effective_owner)
}

#' Resolve a cache target through its nearest existing ancestor
#'
#' Future cache operations use the returned canonical path, rather than a
#' caller-supplied alias containing a replaceable symlink component.
#' @noRd
cache_canonical_target_path <- function(path) {
  absolute <- tryCatch(
    as.character(fs::path_abs(path.expand(path))),
    error = function(e) NA_character_
  )
  if (length(absolute) != 1L || is.na(absolute) || !nzchar(absolute)) {
    return(NULL)
  }

  current <- absolute
  missing <- character()
  while (!file.exists(current) && !dir.exists(current)) {
    parent <- dirname(current)
    if (identical(parent, current)) {
      return(NULL)
    }
    missing <- c(basename(current), missing)
    current <- parent
  }
  if (!dir.exists(current)) {
    return(NULL)
  }
  canonical_parent <- tryCatch(
    as.character(fs::path_real(current)),
    error = function(e) NULL
  )
  if (is.null(canonical_parent)) {
    return(NULL)
  }
  as.character(do.call(file.path, c(list(canonical_parent), as.list(missing))))
}

#' Read POSIX permission bits including the sticky bit
#' @noRd
cache_path_permission_bits <- function(path) {
  info <- suppressWarnings(file.info(path, extra_cols = FALSE))
  if (nrow(info) != 1L || is.na(info$mode[[1]])) {
    return(NA_integer_)
  }
  bitwAnd(as.integer(info$mode[[1]]), as.integer(as.octmode("1777")))
}

#' Audit whether an ancestor can replace a descendant cache path
#'
#' Sticky shared directories such as /tmp are safe only when the next path
#' component belongs to the effective user. Writable non-sticky ancestors are
#' rejected because another local account can rename or replace descendants.
#' @noRd
audit_cache_parent_chain <- function(disk_path) {
  if (!cache_private_modes_supported()) {
    return(list(ok = TRUE))
  }
  canonical <- tryCatch(
    as.character(fs::path_real(disk_path)),
    error = function(e) NULL
  )
  if (is.null(canonical)) {
    return(list(ok = FALSE, reason = "the canonical cache path is unavailable"))
  }

  chain <- canonical
  repeat {
    parent <- dirname(chain[[1]])
    if (identical(parent, chain[[1]])) {
      break
    }
    chain <- c(parent, chain)
  }
  if (length(chain) < 2L) {
    return(list(ok = TRUE))
  }

  writable_mask <- as.integer(as.octmode("0022"))
  sticky_mask <- as.integer(as.octmode("1000"))
  for (i in seq_len(length(chain) - 1L)) {
    parent <- chain[[i]]
    child <- chain[[i + 1L]]
    mode <- cache_path_permission_bits(parent)
    if (is.na(mode)) {
      return(list(
        ok = FALSE,
        reason = paste0("ancestor permissions could not be inspected: ", parent)
      ))
    }
    if (bitwAnd(mode, writable_mask) == 0L) {
      next
    }
    if (bitwAnd(mode, sticky_mask) == 0L) {
      return(list(
        ok = FALSE,
        reason = paste0("a non-sticky cache ancestor is writable: ", parent)
      ))
    }
    if (!cache_paths_owned_by_effective_user(child)) {
      return(list(
        ok = FALSE,
        reason = paste0(
          "a sticky writable ancestor has a child not owned by the effective user: ",
          child
        )
      ))
    }
  }
  list(ok = TRUE)
}

#' Reject writable non-sticky ancestors before creating a cache directory
#' @noRd
audit_existing_cache_parent_capability <- function(disk_path) {
  if (!cache_private_modes_supported()) {
    return(list(ok = TRUE))
  }
  current <- dirname(disk_path)
  writable_mask <- as.integer(as.octmode("0022"))
  sticky_mask <- as.integer(as.octmode("1000"))
  repeat {
    if (dir.exists(current)) {
      mode <- cache_path_permission_bits(current)
      if (is.na(mode)) {
        return(list(
          ok = FALSE,
          reason = paste0(
            "ancestor permissions could not be inspected: ",
            current
          )
        ))
      }
      if (
        bitwAnd(mode, writable_mask) != 0L &&
          bitwAnd(mode, sticky_mask) == 0L
      ) {
        return(list(
          ok = FALSE,
          reason = paste0("a non-sticky cache ancestor is writable: ", current)
        ))
      }
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      break
    }
    current <- parent
  }
  list(ok = TRUE)
}

#' Capture the canonical identity of one trusted cache directory
#' @noRd
cache_directory_identity <- function(path) {
  if (cache_path_is_symlink(path) || !dir.exists(path)) {
    return(NULL)
  }
  canonical <- tryCatch(
    as.character(fs::path_real(path)),
    error = function(e) NULL
  )
  info <- tryCatch(
    suppressWarnings(fs::file_info(path, follow = FALSE, fail = FALSE)),
    error = function(e) NULL
  )
  owner <- cache_path_owner_id(path)
  mode <- cache_path_mode(path)
  if (
    is.null(canonical) ||
      is.null(info) ||
      nrow(info) != 1L ||
      is.na(info$device_id[[1]]) ||
      is.na(info$inode[[1]]) ||
      is.na(owner) ||
      is.na(mode)
  ) {
    return(NULL)
  }
  list(
    path = canonical,
    device_id = as.numeric(info$device_id[[1]]),
    inode = as.numeric(info$inode[[1]]),
    owner_id = owner,
    mode = mode
  )
}

#' Verify a directory still has its audited identity and safe ancestors
#' @noRd
cache_directory_trust_error <- function(trust) {
  current <- cache_directory_identity(trust$path)
  if (is.null(current) || !identical(current, trust)) {
    return("the cache directory identity changed after it was audited")
  }
  parent_audit <- audit_cache_parent_chain(trust$path)
  if (!isTRUE(parent_audit$ok)) {
    return(parent_audit$reason)
  }
  NULL
}

#' Create shared state for guarded cachem hooks and their adapter
#' @noRd
new_cache_disk_guard <- function(trust) {
  guard <- new.env(parent = emptyenv())
  guard$trust <- trust
  guard$invalid <- FALSE
  guard$reason <- NULL
  guard$warning_emitted <- FALSE
  guard$destroyed <- FALSE
  guard
}

#' Record a sticky disk trust failure and detach global disk handles
#' @noRd
cache_record_disk_guard_failure <- function(guard, reason) {
  if (!is.environment(guard)) {
    return(invisible(NULL))
  }
  if (!isTRUE(guard$invalid)) {
    guard$invalid <- TRUE
    guard$reason <- as.character(reason)[1]
  }
  if (identical(.dsprrr_env$cache_disk_guard, guard)) {
    .dsprrr_env$cache <- .dsprrr_env$cache_memory
    .dsprrr_env$cache_disk <- NULL
    .dsprrr_env$cache_degraded <- TRUE
    .dsprrr_env$cache_degraded_reason <- guard$reason
    .dsprrr_env$cache_privacy_status <- "degraded"
    .dsprrr_env$cache_privacy_reason <- guard$reason
  }
  invisible(NULL)
}

#' Abort a guarded operation after recording its trust failure
#' @noRd
cache_abort_disk_guard <- function(guard, reason) {
  cache_record_disk_guard_failure(guard, reason)
  cli::cli_abort(
    "Disk cache trust verification failed: {reason}",
    class = "dsprrr_cache_trust_error"
  )
}

#' Verify a guard before touching a cache path
#' @noRd
cache_assert_disk_guard <- function(guard) {
  if (!is.environment(guard)) {
    return(invisible(TRUE))
  }
  if (isTRUE(guard$invalid)) {
    cache_abort_disk_guard(guard, guard$reason %||% "the cache was invalidated")
  }
  reason <- cache_directory_trust_error(guard$trust)
  if (!is.null(reason)) {
    cache_abort_disk_guard(guard, reason)
  }
  invisible(TRUE)
}

#' Whether a path is a symbolic link
#' @noRd
cache_path_is_symlink <- function(path) {
  info <- tryCatch(
    suppressWarnings(fs::file_info(path, follow = FALSE, fail = FALSE)),
    error = function(e) NULL
  )
  !is.null(info) &&
    nrow(info) == 1L &&
    !is.na(info$type[[1]]) &&
    identical(as.character(info$type[[1]]), "symlink")
}

#' Whether a path is a regular file rather than a device, FIFO, or socket
#' @noRd
cache_path_is_regular <- function(path) {
  info <- tryCatch(
    suppressWarnings(fs::file_info(path, follow = FALSE, fail = FALSE)),
    error = function(e) NULL
  )
  !is.null(info) &&
    nrow(info) == 1L &&
    !is.na(info$type[[1]]) &&
    identical(as.character(info$type[[1]]), "file")
}

#' Read the POSIX permission bits for one path
#' @noRd
cache_path_mode <- function(path) {
  info <- suppressWarnings(file.info(path, extra_cols = FALSE))
  if (nrow(info) != 1L || is.na(info$mode[[1]])) {
    return(NA_integer_)
  }
  bitwAnd(as.integer(info$mode[[1]]), as.integer(as.octmode("0777")))
}

#' Verify one path has exactly the requested POSIX mode
#' @noRd
cache_mode_is <- function(path, mode) {
  identical(cache_path_mode(path), as.integer(as.octmode(mode)))
}

#' Set and verify an exact POSIX mode without consulting process umask
#' @noRd
cache_set_private_mode <- function(paths, mode) {
  if (length(paths) == 0L) {
    return(TRUE)
  }
  changed <- tryCatch(
    suppressWarnings(Sys.chmod(paths, mode = mode, use_umask = FALSE)),
    error = function(e) rep(FALSE, length(paths))
  )
  if (length(changed) != length(paths) || anyNA(changed) || !all(changed)) {
    return(FALSE)
  }
  all(vapply(paths, cache_mode_is, logical(1), mode = mode))
}

#' Inspect a cache directory itself before changing permissions or listing it
#' @noRd
audit_private_cache_directory <- function(disk_path) {
  if (cache_path_is_symlink(disk_path)) {
    return(list(
      ok = FALSE,
      reason = "the cache directory is a symbolic link"
    ))
  }
  if (!dir.exists(disk_path)) {
    return(list(ok = FALSE, reason = "the cache path is not a directory"))
  }

  if (!cache_private_modes_supported()) {
    return(list(
      ok = TRUE,
      needs_repair = FALSE,
      was_overexposed = FALSE
    ))
  }

  effective_owner <- cache_effective_owner_id()
  owner <- cache_path_owner_id(disk_path)
  if (is.na(effective_owner) || is.na(owner)) {
    return(list(
      ok = FALSE,
      reason = "the cache directory owner could not be verified"
    ))
  }
  if (owner != effective_owner) {
    return(list(
      ok = FALSE,
      reason = "the cache directory is not owned by the effective user"
    ))
  }

  mode <- cache_path_mode(disk_path)
  if (is.na(mode)) {
    return(list(
      ok = FALSE,
      reason = "existing cache directory permissions could not be inspected"
    ))
  }
  group_or_other_write <- as.integer(as.octmode("0022"))
  if (bitwAnd(mode, group_or_other_write) != 0L) {
    return(list(
      ok = FALSE,
      reason = paste0(
        "the cache was writable by another local account; ",
        "existing RDS responses are untrusted"
      )
    ))
  }

  group_or_other_access <- as.integer(as.octmode("0077"))
  list(
    ok = TRUE,
    needs_repair = mode != as.integer(as.octmode("0700")),
    was_overexposed = bitwAnd(mode, group_or_other_access) != 0L
  )
}

#' Enumerate a cache directory and detect silent permission failures
#' @noRd
list_private_cache_entries <- function(disk_path) {
  marker <- tempfile(
    pattern = ".dsprrr-audit-marker-",
    tmpdir = disk_path
  )
  on.exit(unlink(marker, force = TRUE), add = TRUE)

  created <- tryCatch(
    suppressWarnings(file.create(marker, showWarnings = FALSE)),
    error = function(e) FALSE
  )
  if (!isTRUE(created)) {
    return(list(
      ok = FALSE,
      reason = "the cache directory could not be enumerated safely"
    ))
  }
  if (
    cache_private_modes_supported() &&
      !cache_set_private_mode(marker, "0600")
  ) {
    return(list(
      ok = FALSE,
      reason = "the cache audit marker could not be made owner-only"
    ))
  }
  if (
    cache_private_modes_supported() &&
      !cache_paths_owned_by_effective_user(marker)
  ) {
    return(list(
      ok = FALSE,
      reason = "the cache audit marker owner could not be verified"
    ))
  }

  entries <- tryCatch(
    suppressWarnings(list.files(
      disk_path,
      all.files = TRUE,
      full.names = TRUE,
      no.. = TRUE
    )),
    error = function(e) NULL
  )
  marker_seen <- !is.null(entries) &&
    sum(basename(entries) == basename(marker)) == 1L
  if (!marker_seen) {
    return(list(
      ok = FALSE,
      reason = "the cache directory could not be enumerated completely"
    ))
  }

  entries <- entries[basename(entries) != basename(marker)]
  if (unlink(marker, force = TRUE) != 0L) {
    return(list(
      ok = FALSE,
      reason = "the cache audit marker could not be removed"
    ))
  }
  list(ok = TRUE, entries = entries)
}

#' Inspect every existing cache entry before reading serialized values
#' @noRd
audit_private_cache_entries <- function(disk_path) {
  listed <- list_private_cache_entries(disk_path)
  if (!isTRUE(listed$ok)) {
    return(list(ok = FALSE, reason = listed$reason))
  }
  entries <- listed$entries

  if (any(vapply(entries, cache_path_is_symlink, logical(1)))) {
    return(list(
      ok = FALSE,
      reason = "the cache contains a symbolic link"
    ))
  }
  regular <- vapply(entries, cache_path_is_regular, logical(1))
  if (!all(regular)) {
    return(list(
      ok = FALSE,
      reason = "the cache contains a non-regular filesystem entry"
    ))
  }

  if (!cache_private_modes_supported()) {
    return(list(
      ok = TRUE,
      files = entries,
      needs_repair = FALSE,
      was_overexposed = FALSE
    ))
  }

  if (!cache_paths_owned_by_effective_user(entries)) {
    return(list(
      ok = FALSE,
      reason = "the cache contains files not owned by the effective user"
    ))
  }

  modes <- vapply(entries, cache_path_mode, integer(1))
  if (anyNA(modes)) {
    return(list(
      ok = FALSE,
      reason = "existing cache permissions could not be inspected"
    ))
  }

  group_or_other_write <- as.integer(as.octmode("0022"))
  group_or_other_access <- as.integer(as.octmode("0077"))
  if (any(bitwAnd(modes, group_or_other_write) != 0L)) {
    return(list(
      ok = FALSE,
      reason = paste0(
        "the cache was writable by another local account; ",
        "existing RDS responses are untrusted"
      )
    ))
  }

  expected <- rep(as.integer(as.octmode("0600")), length(entries))
  list(
    ok = TRUE,
    files = entries,
    needs_repair = any(modes != expected),
    was_overexposed = any(bitwAnd(modes, group_or_other_access) != 0L)
  )
}

#' Create one cache directory without changing parent permissions
#' @noRd
create_cache_directory <- function(disk_path, private = TRUE) {
  if (dir.exists(disk_path)) {
    return(TRUE)
  }
  mode <- if (isTRUE(private)) "0700" else "0777"
  tryCatch(
    {
      dir.create(
        disk_path,
        recursive = TRUE,
        showWarnings = FALSE,
        mode = mode
      )
      dir.exists(disk_path)
    },
    error = function(e) FALSE
  )
}

#' Inspect one private cache file without reading serialized content
#' @noRd
cache_private_file_identity <- function(file, trust) {
  parent <- tryCatch(
    as.character(fs::path_real(dirname(file))),
    error = function(e) NULL
  )
  if (is.null(parent) || !identical(parent, trust$path)) {
    return(list(
      ok = FALSE,
      reason = "a cache file escaped its trusted directory"
    ))
  }
  if (!file.exists(file)) {
    return(list(
      ok = FALSE,
      missing = TRUE,
      reason = "the cache file is missing"
    ))
  }
  if (cache_path_is_symlink(file) || !cache_path_is_regular(file)) {
    return(list(ok = FALSE, reason = "a cache entry is not a regular file"))
  }
  owner <- cache_path_owner_id(file)
  effective_owner <- cache_effective_owner_id()
  mode <- cache_path_mode(file)
  info <- tryCatch(
    suppressWarnings(fs::file_info(file, follow = FALSE, fail = FALSE)),
    error = function(e) NULL
  )
  if (
    is.na(owner) ||
      is.na(effective_owner) ||
      owner != effective_owner ||
      is.na(mode) ||
      mode != as.integer(as.octmode("0600")) ||
      is.null(info) ||
      nrow(info) != 1L ||
      is.na(info$device_id[[1]]) ||
      is.na(info$inode[[1]])
  ) {
    return(list(
      ok = FALSE,
      reason = "a cache entry is not an effective-user-owned 0600 file"
    ))
  }
  if (!identical(as.numeric(info$device_id[[1]]), trust$device_id)) {
    return(list(
      ok = FALSE,
      reason = "a cache entry is on an unexpected device"
    ))
  }
  list(
    ok = TRUE,
    identity = list(
      device_id = as.numeric(info$device_id[[1]]),
      inode = as.numeric(info$inode[[1]]),
      owner_id = owner,
      mode = mode,
      size = as.numeric(info$size[[1]]),
      modification_time = as.numeric(info$modification_time[[1]]),
      change_time = as.numeric(info$change_time[[1]])
    )
  )
}

#' Read one serialized value through the private disk trust boundary
#' @noRd
read_private_cache_value <- function(file, guard) {
  cache_assert_disk_guard(guard)
  before <- cache_private_file_identity(file, guard$trust)
  if (isTRUE(before$missing)) {
    stop("Cache entry is missing")
  }
  if (!isTRUE(before$ok)) {
    cache_abort_disk_guard(guard, before$reason)
  }
  value <- tryCatch(
    readRDS(file),
    error = function(error) {
      cache_abort_disk_guard(
        guard,
        paste0(
          "a verified cache entry could not be read: ",
          conditionMessage(error)
        )
      )
    }
  )
  cache_assert_disk_guard(guard)
  after <- cache_private_file_identity(file, guard$trust)
  if (!isTRUE(after$ok) || !identical(after$identity, before$identity)) {
    cache_abort_disk_guard(
      guard,
      "a cache entry changed identity while it was being read"
    )
  }
  value
}

#' Mockable serialization boundary for private cache writes
#' @noRd
cache_save_rds <- function(value, file) {
  saveRDS(value, file)
}

#' Atomically write one owner-only serialized cache value on Unix
#' @noRd
write_private_cache_value <- function(value, file, guard = NULL) {
  if (is.environment(guard)) {
    cache_assert_disk_guard(guard)
    if (file.exists(file)) {
      existing <- cache_private_file_identity(file, guard$trust)
      if (!isTRUE(existing$ok)) {
        cache_abort_disk_guard(guard, existing$reason)
      }
    }
  }
  temp_file <- tempfile(
    pattern = paste0(".", basename(file), "-temp-"),
    tmpdir = dirname(file)
  )
  on.exit(unlink(temp_file, force = TRUE), add = TRUE)

  old_umask <- Sys.umask("0077")
  on.exit(Sys.umask(old_umask), add = TRUE)
  created <- tryCatch(
    suppressWarnings(file.create(temp_file, showWarnings = FALSE)),
    error = function(e) FALSE
  )
  Sys.umask(old_umask)
  if (
    !isTRUE(created) ||
      !cache_set_private_mode(temp_file, "0600") ||
      !cache_paths_owned_by_effective_user(temp_file) ||
      cache_path_is_symlink(temp_file) ||
      !cache_path_is_regular(temp_file)
  ) {
    stop("Could not pre-create an effective-user-owned 0600 cache file")
  }

  # No sensitive bytes are serialized until the staging file is verified.
  staging_before <- if (is.environment(guard)) {
    cache_private_file_identity(temp_file, guard$trust)
  } else {
    NULL
  }
  cache_save_rds(value, temp_file)
  if (
    !cache_mode_is(temp_file, "0600") ||
      !cache_paths_owned_by_effective_user(temp_file) ||
      !cache_path_is_regular(temp_file)
  ) {
    stop("Could not verify the private cache staging file")
  }
  if (is.environment(guard)) {
    staging_after <- cache_private_file_identity(temp_file, guard$trust)
    if (
      !isTRUE(staging_before$ok) ||
        !isTRUE(staging_after$ok) ||
        !identical(
          staging_before$identity$device_id,
          staging_after$identity$device_id
        ) ||
        !identical(
          staging_before$identity$inode,
          staging_after$identity$inode
        ) ||
        !identical(
          staging_before$identity$owner_id,
          staging_after$identity$owner_id
        ) ||
        !identical(staging_before$identity$mode, staging_after$identity$mode)
    ) {
      cache_abort_disk_guard(
        guard,
        "the private cache staging file changed identity during serialization"
      )
    }
  }
  if (is.environment(guard)) {
    cache_assert_disk_guard(guard)
  }
  if (!isTRUE(file.rename(temp_file, file))) {
    stop("Could not atomically install a cache file")
  }
  if (is.environment(guard)) {
    cache_assert_disk_guard(guard)
    installed <- cache_private_file_identity(file, guard$trust)
    installed_ok <- isTRUE(installed$ok)
  } else {
    installed_ok <- cache_mode_is(file, "0600") &&
      cache_paths_owned_by_effective_user(file) &&
      cache_path_is_regular(file)
  }
  if (!installed_ok) {
    unlink(file, force = TRUE)
    stop("Could not verify the installed private cache file")
  }
  invisible(NULL)
}

#' Emit one degradation warning for a sticky disk guard failure
#' @noRd
cache_report_disk_guard_failure <- function(guard) {
  if (!is.environment(guard) || !isTRUE(guard$invalid)) {
    return(FALSE)
  }
  if (!identical(.dsprrr_env$cache_disk_guard, guard)) {
    return(TRUE)
  }
  # Detach before emitting diagnostics: options(warn = 2) may promote the
  # warning below to an error, but must not interrupt global state cleanup.
  .dsprrr_env$cache_disk_guard <- NULL
  if (!isTRUE(guard$warning_emitted)) {
    guard$warning_emitted <- TRUE
    cache_disk_degrade(guard$trust$path, guard$reason)
  }
  TRUE
}

#' Guard a cachem disk object, including the stale handle retained by layers
#' @noRd
guarded_cache_disk <- function(cache, guard) {
  guarded_call <- function(method, ..., missing_on_failure = FALSE) {
    if (isTRUE(guard$destroyed)) {
      stop("Attempted to use a disk cache which has been destroyed")
    }
    if (isTRUE(guard$invalid)) {
      cache_report_disk_guard_failure(guard)
      if (missing_on_failure) {
        return(cachem::key_missing())
      }
      return(invisible(FALSE))
    }
    value <- tryCatch(
      {
        cache_assert_disk_guard(guard)
        cache[[method]](...)
      },
      error = function(error) {
        if (!isTRUE(guard$invalid)) {
          cache_record_disk_guard_failure(guard, conditionMessage(error))
        }
        NULL
      }
    )
    if (isTRUE(guard$invalid)) {
      cache_report_disk_guard_failure(guard)
      if (missing_on_failure) {
        return(cachem::key_missing())
      }
      return(invisible(FALSE))
    }
    if (identical(method, "destroy")) {
      guard$destroyed <- TRUE
    }
    value
  }

  structure(
    list(
      get = function(key, ...) {
        guarded_call("get", key, ..., missing_on_failure = TRUE)
      },
      set = function(key, value) guarded_call("set", key, value),
      exists = function(key) guarded_call("exists", key),
      keys = function() guarded_call("keys"),
      remove = function(key) guarded_call("remove", key),
      reset = function() guarded_call("reset"),
      prune = function() guarded_call("prune"),
      size = function() guarded_call("size"),
      destroy = function() guarded_call("destroy"),
      is_destroyed = cache$is_destroyed,
      info = cache$info
    ),
    class = c("dsprrr_guarded_cache_disk", "cache_disk", "cachem")
  )
}

#' Reject cleanup through a disk handle whose path identity changed
#' @noRd
cache_verify_disk_cleanup_guard <- function(guard) {
  if (is.environment(guard)) {
    cache_assert_disk_guard(guard)
  }
  invisible(TRUE)
}

#' Verify the filesystem supports private response-file creation
#' @noRd
verify_private_cache_write <- function(disk_path) {
  probe <- tempfile(
    pattern = ".dsprrr-permission-probe-",
    tmpdir = disk_path
  )
  on.exit(unlink(probe, force = TRUE), add = TRUE)
  tryCatch(
    {
      write_private_cache_value(NULL, probe)
      cache_mode_is(probe, "0600") && unlink(probe, force = TRUE) == 0L
    },
    error = function(e) FALSE
  )
}

#' Prepare a cache directory and establish its privacy status
#' @noRd
prepare_cache_directory <- function(disk_path, private = TRUE) {
  if (
    !is.character(disk_path) || length(disk_path) != 1L || !nzchar(disk_path)
  ) {
    return(list(
      ok = FALSE,
      path = "<invalid>",
      reason = "the configured disk path is not one non-empty string"
    ))
  }
  disk_path <- path.expand(disk_path)

  if (file.exists(disk_path) && !dir.exists(disk_path)) {
    return(list(
      ok = FALSE,
      path = disk_path,
      reason = "the configured disk path is not a directory"
    ))
  }
  if (cache_path_is_symlink(disk_path)) {
    return(list(
      ok = FALSE,
      path = disk_path,
      reason = "the cache directory is a symbolic link"
    ))
  }
  canonical_target <- cache_canonical_target_path(disk_path)
  if (is.null(canonical_target)) {
    return(list(
      ok = FALSE,
      path = disk_path,
      reason = "the cache path could not be resolved canonically"
    ))
  }
  disk_path <- canonical_target

  if (!isTRUE(private)) {
    if (!create_cache_directory(disk_path, private = FALSE)) {
      return(list(
        ok = FALSE,
        path = disk_path,
        reason = "the cache directory could not be created"
      ))
    }
    .dsprrr_env$cache_privacy_status <- "disabled"
    .dsprrr_env$cache_privacy_reason <- paste0(
      "owner-only enforcement was disabled; all cache users must be trusted"
    )
    return(list(ok = TRUE, path = disk_path))
  }

  directory_audit <- if (dir.exists(disk_path)) {
    audit_private_cache_directory(disk_path)
  } else {
    list(
      ok = TRUE,
      needs_repair = FALSE,
      was_overexposed = FALSE
    )
  }
  if (!isTRUE(directory_audit$ok)) {
    return(list(
      ok = FALSE,
      path = disk_path,
      reason = directory_audit$reason
    ))
  }

  pre_create_parent_audit <- audit_existing_cache_parent_capability(disk_path)
  if (!isTRUE(pre_create_parent_audit$ok)) {
    return(list(
      ok = FALSE,
      path = disk_path,
      reason = pre_create_parent_audit$reason
    ))
  }

  # A directory can be owner-writable but not owner-readable (for example,
  # mode 0300). Close it to other accounts and restore owner access before
  # enumerating; list.files() otherwise reports an indistinguishable empty
  # result on some systems.
  if (
    dir.exists(disk_path) &&
      cache_private_modes_supported() &&
      !cache_set_private_mode(disk_path, "0700")
  ) {
    return(list(
      ok = FALSE,
      path = disk_path,
      reason = "owner-only cache directory permissions could not be enforced"
    ))
  }

  if (!create_cache_directory(disk_path, private = TRUE)) {
    return(list(
      ok = FALSE,
      path = disk_path,
      reason = "the cache directory could not be created"
    ))
  }

  # Re-check directory trust after creation so a concurrent creator cannot
  # populate an externally writable path between the checks above.
  post_create_directory_audit <- audit_private_cache_directory(disk_path)
  if (!isTRUE(post_create_directory_audit$ok)) {
    return(list(
      ok = FALSE,
      path = disk_path,
      reason = post_create_directory_audit$reason
    ))
  }

  if (
    cache_private_modes_supported() &&
      !cache_set_private_mode(disk_path, "0700")
  ) {
    return(list(
      ok = FALSE,
      path = disk_path,
      reason = "owner-only cache directory permissions could not be verified"
    ))
  }

  parent_audit <- audit_cache_parent_chain(disk_path)
  if (!isTRUE(parent_audit$ok)) {
    return(list(
      ok = FALSE,
      path = disk_path,
      reason = parent_audit$reason
    ))
  }

  entry_audit <- audit_private_cache_entries(disk_path)
  if (!isTRUE(entry_audit$ok)) {
    return(list(
      ok = FALSE,
      path = disk_path,
      reason = entry_audit$reason
    ))
  }

  needs_repair <- isTRUE(directory_audit$needs_repair) ||
    isTRUE(post_create_directory_audit$needs_repair) ||
    isTRUE(entry_audit$needs_repair)
  was_overexposed <- isTRUE(directory_audit$was_overexposed) ||
    isTRUE(post_create_directory_audit$was_overexposed) ||
    isTRUE(entry_audit$was_overexposed)

  if (!cache_private_modes_supported()) {
    .dsprrr_env$cache_privacy_status <- "unverified_windows"
    .dsprrr_env$cache_privacy_reason <- paste0(
      "the cache inherits filesystem ACLs that base R cannot verify"
    )
    return(list(ok = TRUE, path = disk_path, trust = NULL))
  }

  if (
    !cache_set_private_mode(entry_audit$files, "0600") ||
      !verify_private_cache_write(disk_path)
  ) {
    return(list(
      ok = FALSE,
      path = disk_path,
      reason = "owner-only cache permissions could not be enforced and verified"
    ))
  }

  # The write probe and repairs mutate the directory. Repeat every audit, then
  # bind future operations to the final canonical identity.
  final_directory_audit <- audit_private_cache_directory(disk_path)
  final_parent_audit <- audit_cache_parent_chain(disk_path)
  final_entry_audit <- audit_private_cache_entries(disk_path)
  trust <- cache_directory_identity(disk_path)
  if (
    !isTRUE(final_directory_audit$ok) ||
      !isTRUE(final_parent_audit$ok) ||
      !isTRUE(final_entry_audit$ok) ||
      is.null(trust) ||
      trust$mode != as.integer(as.octmode("0700")) ||
      trust$owner_id != cache_effective_owner_id()
  ) {
    reason <- final_directory_audit$reason %||%
      final_parent_audit$reason %||%
      final_entry_audit$reason %||%
      "the final cache directory identity could not be verified"
    return(list(ok = FALSE, path = disk_path, reason = reason))
  }

  .dsprrr_env$cache_privacy_status <- "verified_posix_modes"
  .dsprrr_env$cache_privacy_reason <- paste0(
    "effective ownership and POSIX modes were verified; extended ACLs were not checked"
  )
  if (needs_repair && was_overexposed) {
    cli::cli_warn(
      c(
        "!" = "Tightened permissions on an existing disk cache at {.path {disk_path}}",
        "i" = "The directory is now owner-only, but prior disclosure cannot be undone."
      ),
      class = "dsprrr_cache_permissions_repaired",
      .frequency = "once",
      .frequency_id = paste0(
        "cache-permissions-repaired-",
        digest::digest(disk_path, serialize = FALSE)
      )
    )
  }

  list(ok = TRUE, path = disk_path, trust = trust)
}

#' Record a disk-cache failure and fall back to memory-only operation
#' @noRd
cache_disk_degrade <- function(disk_path, reason) {
  memory_available <- isTRUE(.dsprrr_env$cache_config$enable_memory) &&
    !is.null(.dsprrr_env$cache_memory)
  .dsprrr_env$cache <- if (memory_available) {
    .dsprrr_env$cache_memory
  } else {
    NULL
  }
  .dsprrr_env$cache_disk <- NULL
  .dsprrr_env$cache_degraded <- TRUE
  .dsprrr_env$cache_degraded_reason <- reason
  .dsprrr_env$cache_privacy_status <- "degraded"
  .dsprrr_env$cache_privacy_reason <- reason
  fallback <- if (memory_available) {
    "Falling back to memory-only caching for this session."
  } else {
    "No cache tier remains enabled for this session."
  }

  cli::cli_warn(
    c(
      "!" = "Disk caching is unavailable at {.path {disk_path}}",
      "x" = reason,
      "i" = fallback
    ),
    class = c("dsprrr_cache_security_warning", "dsprrr_cache_warning"),
    .frequency = "once",
    .frequency_id = paste0(
      "cache-disk-degraded-",
      digest::digest(c(disk_path, reason), serialize = FALSE)
    )
  )
  NULL
}

#' Increment Cache Statistics
#'
#' @description
#' Increment hit or miss counter.
#'
#' @param type Character. Either "hits" or "misses".
#'
#' @noRd
increment_cache_stats <- function(type) {
  if (is.null(.dsprrr_env$cache_stats)) {
    .dsprrr_env$cache_stats <- list(hits = 0L, misses = 0L)
  }
  .dsprrr_env$cache_stats[[type]] <- .dsprrr_env$cache_stats[[type]] + 1L
  invisible(NULL)
}

#' Validate and copy semantic turn values without retaining executable objects
#' @noRd
cache_replay_value <- function(x) {
  validate <- function(value) {
    if (is.null(value) || is.atomic(value)) {
      return(invisible(NULL))
    }
    if (is.list(value) && !inherits(value, "S7_object")) {
      lapply(value, validate)
      return(invisible(NULL))
    }
    cli::cli_abort(
      "Turn replay contains unsupported {.cls {class(value)[1]}} state",
      class = "dsprrr_cache_replay_error"
    )
  }
  validate(x)
  unserialize(serialize(x, connection = NULL, version = 2))
}

#' Clone one content object for safe semantic replay
#' @noRd
cache_replay_content <- function(content) {
  if (inherits(content, "ellmer::ContentText")) {
    return(ellmer::ContentText(cache_replay_value(content@text)))
  }
  if (inherits(content, "ellmer::ContentJson")) {
    constructor <- getFromNamespace("ContentJson", "ellmer")
    return(constructor(
      data = cache_replay_value(content@data),
      string = cache_replay_value(content@string)
    ))
  }
  if (inherits(content, "ellmer::ContentThinking")) {
    return(ellmer::ContentThinking(
      cache_replay_value(content@thinking),
      extra = cache_replay_value(content@extra)
    ))
  }
  if (inherits(content, "ellmer::ContentImageInline")) {
    return(ellmer::ContentImageInline(
      type = cache_replay_value(content@type),
      data = cache_replay_value(content@data)
    ))
  }
  if (inherits(content, "ellmer::ContentImageRemote")) {
    return(ellmer::ContentImageRemote(
      url = cache_replay_value(content@url),
      detail = cache_replay_value(content@detail)
    ))
  }
  if (inherits(content, "ellmer::ContentPDF")) {
    return(ellmer::ContentPDF(
      type = cache_replay_value(content@type),
      data = cache_replay_value(content@data),
      filename = cache_replay_value(content@filename)
    ))
  }
  if (inherits(content, "ellmer::ContentToolRequest")) {
    return(ellmer::ContentToolRequest(
      id = cache_replay_value(content@id),
      name = cache_replay_value(content@name),
      arguments = cache_replay_value(content@arguments),
      tool = NULL,
      extra = cache_replay_value(content@extra)
    ))
  }
  if (inherits(content, "ellmer::ContentToolResult")) {
    request <- if (is.null(content@request)) {
      NULL
    } else {
      cache_replay_content(content@request)
    }
    return(ellmer::ContentToolResult(
      value = cache_replay_value(content@value),
      error = cache_replay_value(content@error),
      extra = cache_replay_value(content@extra),
      request = request
    ))
  }
  if (inherits(content, "ellmer::ContentUploaded")) {
    constructor <- getFromNamespace("ContentUploaded", "ellmer")
    return(constructor(
      uri = cache_replay_value(content@uri),
      mime_type = cache_replay_value(content@mime_type)
    ))
  }

  cli::cli_abort(
    "Cannot safely replay content {.cls {class(content)[1]}}",
    class = "dsprrr_cache_replay_error"
  )
}

#' Clone a turn while deliberately dropping observed usage and cost metadata
#' @noRd
cache_replay_turn <- function(turn) {
  contents <- lapply(turn@contents, cache_replay_content)
  if (inherits(turn, "ellmer::AssistantTurn")) {
    return(ellmer::AssistantTurn(
      contents = contents,
      json = cache_replay_value(turn@json),
      tokens = c(NA_real_, NA_real_, NA_real_),
      cost = NA_real_,
      duration = NA_real_,
      finish_reason = cache_replay_value(turn@finish_reason)
    ))
  }
  if (inherits(turn, "ellmer::SystemTurn")) {
    return(ellmer::SystemTurn(contents = contents))
  }
  if (inherits(turn, "ellmer::UserTurn")) {
    return(ellmer::UserTurn(contents = contents))
  }

  cli::cli_abort(
    "Cannot safely replay turn {.cls {class(turn)[1]}}",
    class = "dsprrr_cache_replay_error"
  )
}

#' Snapshot Chat turns before a cacheable request
#' @noRd
cache_turn_snapshot <- function(llm) {
  cache_require_trusted_ellmer_chat(
    llm,
    error_class = "dsprrr_cache_replay_error"
  )
  get_turns <- tryCatch(llm$get_turns, error = function(e) NULL)
  set_turns <- tryCatch(llm$set_turns, error = function(e) NULL)
  if (!is.function(get_turns) || !is.function(set_turns)) {
    cli::cli_abort(
      "Chat history cannot be inspected and replayed",
      class = "dsprrr_cache_replay_error"
    )
  }
  turns <- tryCatch(
    get_turns(),
    error = function(e) {
      cli::cli_abort(
        "Failed to read Chat history for cache replay",
        class = "dsprrr_cache_replay_error",
        parent = e
      )
    }
  )
  if (!is.list(turns)) {
    cli::cli_abort(
      "Chat history must be a list of turns",
      class = "dsprrr_cache_replay_error"
    )
  }
  list(mode = "turns", turns = turns)
}

#' Capture the semantic turn delta produced by a request
#' @noRd
cache_turn_delta <- function(llm, before) {
  if (identical(before$mode, "stateless")) {
    return(list(mode = "stateless", turns = list()))
  }
  after <- cache_turn_snapshot(llm)
  before_n <- length(before$turns)
  if (
    !identical(after$mode, "turns") ||
      length(after$turns) < before_n ||
      (before_n > 0 &&
        !identical(after$turns[seq_len(before_n)], before$turns))
  ) {
    cli::cli_abort(
      "Chat history was replaced during the request and cannot be replayed",
      class = "dsprrr_cache_replay_error"
    )
  }
  new_turns <- if (length(after$turns) == before_n) {
    list()
  } else {
    after$turns[seq.int(before_n + 1L, length(after$turns))]
  }
  list(mode = "turns", turns = lapply(new_turns, cache_replay_turn))
}

#' Copy Chat turns so replay attempts cannot corrupt the rollback snapshot
#' @noRd
cache_copy_turn_history <- function(turns) {
  tryCatch(
    rlang::duplicate(turns, shallow = FALSE),
    error = function(e) e
  )
}

#' Verify that a Chat contains exactly the expected turns
#' @noRd
cache_turn_history_matches <- function(get_turns, expected) {
  observed <- tryCatch(get_turns(), error = function(e) e)
  !inherits(observed, "condition") &&
    is.list(observed) &&
    identical(observed, expected)
}

#' Restore and verify the exact history from before a cache replay attempt
#' @noRd
cache_restore_turn_history <- function(get_turns, set_turns, before) {
  restoration <- cache_copy_turn_history(before)
  if (
    inherits(restoration, "condition") ||
      !identical(restoration, before)
  ) {
    return(FALSE)
  }

  tryCatch(set_turns(restoration), error = function(e) NULL)
  cache_turn_history_matches(get_turns, before)
}

#' Replay a cached semantic turn delta without fabricating usage metadata
#' @noRd
cache_replay_turn_delta <- function(llm, delta) {
  if (!cache_is_trusted_ellmer_chat(llm)) {
    return(FALSE)
  }
  if (identical(delta$mode, "stateless")) {
    return(TRUE)
  }
  if (!identical(delta$mode, "turns") || !is.list(delta$turns)) {
    return(FALSE)
  }

  get_turns <- tryCatch(llm$get_turns, error = function(e) NULL)
  set_turns <- tryCatch(llm$set_turns, error = function(e) NULL)
  if (!is.function(get_turns) || !is.function(set_turns)) {
    return(FALSE)
  }
  existing <- tryCatch(get_turns(), error = function(e) e)
  if (inherits(existing, "condition") || !is.list(existing)) {
    return(FALSE)
  }
  before <- cache_copy_turn_history(existing)
  if (inherits(before, "condition") || !identical(before, existing)) {
    return(FALSE)
  }
  replayed <- tryCatch(
    lapply(delta$turns, cache_replay_turn),
    error = function(e) e
  )
  if (inherits(replayed, "condition")) {
    return(FALSE)
  }

  existing_for_update <- cache_copy_turn_history(before)
  if (
    inherits(existing_for_update, "condition") ||
      !identical(existing_for_update, before)
  ) {
    return(FALSE)
  }
  intended <- c(existing_for_update, replayed)
  intended_for_setter <- cache_copy_turn_history(intended)
  if (
    inherits(intended_for_setter, "condition") ||
      !identical(intended_for_setter, intended)
  ) {
    return(FALSE)
  }

  setter_error <- tryCatch(
    {
      set_turns(intended_for_setter)
      NULL
    },
    error = function(e) e
  )
  if (
    is.null(setter_error) &&
      cache_turn_history_matches(get_turns, intended)
  ) {
    return(TRUE)
  }

  if (cache_restore_turn_history(get_turns, set_turns, before)) {
    return(FALSE)
  }

  cli::cli_abort(
    c(
      "Cache replay left the Chat in an unverified state",
      "x" = "The original conversation history could not be restored.",
      "i" = "The provider was not called."
    ),
    class = c(
      "dsprrr_cache_replay_state_error",
      "dsprrr_cache_replay_error"
    )
  )
}

#' Build and validate the versioned cache value envelope
#' @noRd
cache_envelope <- function(result, turn_delta) {
  list(
    version = cache_envelope_schema_version(),
    result = result,
    turn_delta = turn_delta
  )
}

is_cache_envelope <- function(x) {
  is.list(x) &&
    identical(x$version, cache_envelope_schema_version()) &&
    all(c("result", "turn_delta") %in% names(x))
}

#' Cached LLM Call
#'
#' @description
#' Wrapper around LLM calls that checks cache first.
#'
#' @param llm An ellmer Chat object.
#' @param prompt Character or a list of ellmer Content objects.
#' @param output_type An ellmer Type object.
#' @param rollout_id Optional value for cache partitioning (will be converted to character).
#' @param .cache Logical or NULL. Per-call cache control. If NULL (default), uses global config.
#'   If TRUE, attempts to use cache (no effect if caching globally disabled).
#'   If FALSE, bypasses cache for this call only.
#'
#' @return The LLM response (from cache or fresh call).
#'
#' @noRd
cached_chat_structured <- function(
  llm,
  prompt,
  output_type,
  rollout_id = NULL,
  .cache = NULL,
  .observer = NULL
) {
  observed <- FALSE
  observe <- function(status, reason) {
    if (observed) {
      return(invisible(NULL))
    }
    observed <<- TRUE
    if (is.function(.observer)) {
      tryCatch(
        .observer(status = status, reason = reason),
        error = function(e) invisible(NULL)
      )
    }
    invisible(NULL)
  }

  # Per-call override takes precedence over global config
  use_cache <- if (!is.null(.cache)) {
    isTRUE(.cache)
  } else {
    cache_enabled()
  }

  # If caching disabled (globally or per-call), make direct call
  if (!use_cache) {
    observe("bypass", "disabled")
    return(llm$chat_structured(prompt, type = output_type, echo = "none"))
  }

  cache <- get_cache()
  if (is.null(cache)) {
    observe("bypass", "unavailable")
    return(llm$chat_structured(prompt, type = output_type, echo = "none"))
  }
  disk_guard <- .dsprrr_env$cache_disk_guard

  fingerprint <- tryCatch(
    cache_request_fingerprint(
      llm = llm,
      payload = prompt,
      output_type = output_type,
      rollout_id = rollout_id,
      llm_id = format(rlang::obj_address(llm))
    ),
    error = function(e) e
  )

  if (inherits(fingerprint, "condition")) {
    if (inherits(fingerprint, "dsprrr_cache_untrusted_chat")) {
      observe("bypass", "untrusted_chat")
      return(llm$chat_structured(prompt, type = output_type, echo = "none"))
    }
    if (inherits(fingerprint, "dsprrr_cache_tools_error")) {
      cli::cli_warn(
        c(
          "Registered tools disable LLM response caching",
          "i" = "Tool implementations and side effects must execute normally."
        ),
        .frequency = "once",
        .frequency_id = "cache-tools-bypassed"
      )
    } else {
      cli::cli_warn(
        c(
          "Cannot safely identify this LLM request; bypassing the cache",
          "x" = conditionMessage(fingerprint),
          "i" = "The request will still be sent normally."
        ),
        .frequency = "once",
        .frequency_id = "cache-fingerprint-unavailable"
      )
    }
    reason <- if (inherits(fingerprint, "dsprrr_cache_tools_error")) {
      "registered_tools"
    } else {
      "fingerprint_unavailable"
    }
    observe("bypass", reason)
    return(llm$chat_structured(prompt, type = output_type, echo = "none"))
  }

  key <- cache_key(
    prompt = prompt,
    model = "",
    output_type = output_type,
    fingerprint = fingerprint
  )

  # Try to get from cache
  cached_entry <- cache$get(key)
  disk_guard_failed <- is.environment(disk_guard) &&
    isTRUE(disk_guard$invalid)
  if (disk_guard_failed) {
    cache_report_disk_guard_failure(disk_guard)
    observe("bypass", "disk_trust_failed")
    return(llm$chat_structured(prompt, type = output_type, echo = "none"))
  }

  if (
    !disk_guard_failed &&
      !cachem::is.key_missing(cached_entry) &&
      is_cache_envelope(cached_entry) &&
      cache_replay_turn_delta(llm, cached_entry$turn_delta)
  ) {
    increment_cache_stats("hits")
    observe("hit", "cache_hit")

    # Show first-hit message if this is the first cache hit this session
    if (!isTRUE(.dsprrr_env$cache_first_hit_shown)) {
      cli::cli_inform(c(
        "i" = "Using cached LLM responses",
        "i" = "Disable with {.code configure_cache(enable = FALSE)} or {.code .cache = FALSE}"
      ))
      .dsprrr_env$cache_first_hit_shown <- TRUE
    }

    return(cached_entry$result)
  }

  increment_cache_stats("misses")
  observe("miss", "cache_miss")
  before <- tryCatch(cache_turn_snapshot(llm), error = function(e) e)
  if (inherits(before, "condition")) {
    cli::cli_warn(
      c(
        "Cannot safely replay this Chat; bypassing the cache",
        "x" = conditionMessage(before)
      ),
      .frequency = "once",
      .frequency_id = "cache-turn-replay-unavailable"
    )
    return(llm$chat_structured(prompt, type = output_type, echo = "none"))
  }

  result <- llm$chat_structured(prompt, type = output_type, echo = "none")
  turn_delta <- tryCatch(
    cache_turn_delta(llm, before),
    error = function(e) e
  )
  if (inherits(turn_delta, "condition")) {
    cli::cli_warn(
      c(
        "LLM response was not cached because its turn history is not replayable",
        "x" = conditionMessage(turn_delta)
      ),
      .frequency = "once",
      .frequency_id = "cache-turn-delta-unavailable"
    )
    return(result)
  }

  set_error <- tryCatch(
    {
      cache$set(key, cache_envelope(result, turn_delta))
      NULL
    },
    error = function(error) error
  )
  if (is.environment(disk_guard) && isTRUE(disk_guard$invalid)) {
    cache_report_disk_guard_failure(disk_guard)
  } else if (inherits(set_error, "condition")) {
    cli::cli_warn(
      c(
        "The LLM response could not be cached",
        "x" = conditionMessage(set_error),
        "i" = "The provider result is still being returned."
      ),
      class = "dsprrr_cache_write_warning"
    )
  }

  result
}
