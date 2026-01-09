#' LLM Response Caching
#'
#' @description
#' dsprrr provides automatic caching of LLM responses to speed up development
#' and reduce costs. The cache uses a two-tier architecture:
#' 1. **Memory cache**: Fast in-session LRU cache
#' 2. **Disk cache**: Persistent cache across R sessions
#'
#' @name cache
#' @keywords internal
NULL

# ── Cache Configuration ──────────────────────────────────────────────────────

#' Configure dsprrr Cache
#'
#' @description
#' Configure the caching behavior for LLM responses. By default, both memory
#' and disk caching are enabled.
#'
#' @details
#' The cache stores parsed LLM responses (lists, tibbles, vectors) keyed by
#' a hash of the prompt, model, temperature, and output type. This avoids
#' redundant API calls during development and optimization.
#'
#' **Environment variable**: Set `DSPRRR_CACHE_ENABLED=false` (or `0`, `no`,
#' `off`) to globally disable caching, useful for CI/testing environments.
#'
#' **Git**: If using disk caching, add `.dsprrr_cache/` to your `.gitignore`:
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
#'   Default `".dsprrr_cache"`.
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
#' }
configure_cache <- function(
  enable = TRUE,
  enable_memory = TRUE,
  enable_disk = TRUE,
  disk_path = ".dsprrr_cache",
  memory_max_entries = 1000L,
  disk_max_size = 500 * 1024^2,
  disk_max_age = Inf
) {
  # Store previous config for return
  old_config <- .dsprrr_env$cache_config

  # Build new config
  .dsprrr_env$cache_config <- list(
    enable = enable,
    enable_memory = enable_memory,
    enable_disk = enable_disk,
    disk_path = disk_path,
    memory_max_entries = as.integer(memory_max_entries),
    disk_max_size = disk_max_size,
    disk_max_age = disk_max_age
  )

  # Reset cache objects to force re-creation with new config
  .dsprrr_env$cache <- NULL
  .dsprrr_env$cache_memory <- NULL
  .dsprrr_env$cache_disk <- NULL

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

  if (which %in% c("all", "memory") && !is.null(.dsprrr_env$cache_memory)) {
    .dsprrr_env$cache_memory$reset()
  }

  if (which %in% c("all", "disk") && !is.null(.dsprrr_env$cache_disk)) {
    .dsprrr_env$cache_disk$reset()
  }

  # Also reset layered cache reference
  if (which == "all") {
    .dsprrr_env$cache <- NULL
  }

  # Reset stats
  .dsprrr_env$cache_stats <- list(hits = 0L, misses = 0L)

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
      disk_path = ".dsprrr_cache",
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
  if (config$enable_disk) {
    disk_ok <- TRUE

    # Create directory if needed
    if (!dir.exists(config$disk_path)) {
      dir_created <- tryCatch(
        {
          dir.create(config$disk_path, recursive = TRUE, showWarnings = TRUE)
          TRUE
        },
        warning = function(w) {
          cli::cli_warn(c(
            "Could not create cache directory: {.path {config$disk_path}}",
            "i" = "Disk caching will be disabled for this session",
            "x" = w$message
          ))
          FALSE
        },
        error = function(e) {
          cli::cli_warn(c(
            "Error creating cache directory: {.path {config$disk_path}}",
            "i" = "Disk caching will be disabled for this session",
            "x" = e$message
          ))
          FALSE
        }
      )
      disk_ok <- dir_created
    }

    if (disk_ok) {
      disk_cache_result <- tryCatch(
        {
          cachem::cache_disk(
            dir = config$disk_path,
            max_size = config$disk_max_size,
            max_age = config$disk_max_age
          )
        },
        error = function(e) {
          cli::cli_warn(c(
            "Failed to create disk cache at {.path {config$disk_path}}",
            "i" = "Disk caching will be disabled for this session",
            "x" = e$message
          ))
          NULL
        }
      )

      if (!is.null(disk_cache_result)) {
        .dsprrr_env$cache_disk <- disk_cache_result
        caches <- c(caches, list(.dsprrr_env$cache_disk))
      }
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

#' Compute Cache Key
#'
#' @description
#' Compute a unique cache key from request parameters.
#'
#' @param prompt Character. The full prompt text.
#' @param model Character. Model identifier.
#' @param temperature Numeric or NULL. Temperature parameter.
#' @param output_type An ellmer Type object or description.
#' @param rollout_id Optional integer for cache partitioning.
#' @param llm_id Optional character. Unique identifier for the LLM object
#'   (used for mock LLMs to prevent cache collisions).
#'
#' @return Character. A hex digest (SHA256) cache key.
#'
#' @noRd
cache_key <- function(
  prompt,
  model,
  temperature = NULL,
  output_type,
  rollout_id = NULL,
  llm_id = NULL
) {
  # Serialize output_type to stable representation
  output_type_repr <- tryCatch(
    {
      if (
        inherits(output_type, "TypeObject") ||
          inherits(output_type, "TypeString") ||
          inherits(output_type, "TypeEnum")
      ) {
        # Convert ellmer type to JSON representation
        jsonlite::toJSON(output_type, auto_unbox = TRUE, force = TRUE)
      } else {
        as.character(output_type)
      }
    },
    error = function(e) {
      fallback <- as.character(class(output_type)[1])
      cli::cli_warn(c(
        "Failed to serialize output_type for cache key",
        "i" = "Using class name as fallback: {.val {fallback}}",
        "i" = "This may cause cache collisions for different output types",
        "x" = "Original error: {e$message}"
      ))
      fallback
    }
  )

  # Build key components
  key_parts <- list(
    prompt = prompt,
    model = model,
    temperature = temperature %||% "default",
    output_type = output_type_repr
  )

  # Add rollout_id if provided (enables cache partitioning for diversity)
  if (!is.null(rollout_id)) {
    key_parts$rollout_id <- as.character(rollout_id)
  }

  # Add llm_id if provided (used for mock LLMs to prevent cache collisions)
  if (!is.null(llm_id)) {
    key_parts$llm_id <- llm_id
  }

  # Compute SHA256 hash
  key_json <- jsonlite::toJSON(key_parts, auto_unbox = TRUE)
  digest::digest(key_json, algo = "sha256")
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

#' Cached LLM Call
#'
#' @description
#' Wrapper around LLM calls that checks cache first.
#'
#' @param llm An ellmer Chat object.
#' @param prompt Character. The prompt text.
#' @param output_type An ellmer Type object.
#' @param rollout_id Optional value for cache partitioning (will be converted to character).
#'
#' @return The LLM response (from cache or fresh call).
#'
#' @noRd
cached_chat_structured <- function(
  llm,
  prompt,
  output_type,
  rollout_id = NULL
) {
  # If caching disabled, make direct call
  if (!cache_enabled()) {
    return(llm$chat_structured(prompt, type = output_type, echo = "none"))
  }

  cache <- get_cache()
  if (is.null(cache)) {
    return(llm$chat_structured(prompt, type = output_type, echo = "none"))
  }

  # Get model name for cache key
  model <- tryCatch(
    llm$get_model(),
    error = function(e) {
      cli::cli_warn(c(
        "Could not extract model name from LLM for cache key",
        "i" = "Using 'unknown' as fallback - cache may be less effective",
        "x" = "Error: {e$message}"
      ))
      "unknown"
    }
  )

  # Get temperature from LLM config if available
  temperature <- tryCatch(
    {
      # Try to access provider's temperature setting
      llm$.__enclos_env__$private$api_args$temperature
    },
    error = function(e) NULL
  )

  # Get LLM identity - use address for mock LLMs that don't have proper get_model()
  # This ensures different mock LLM objects don't share cache entries
  llm_id <- if (model == "unknown") {
    # For mock/unknown LLMs, include object address to avoid collisions
    format(rlang::obj_address(llm))
  } else {
    NULL
  }

  # Compute cache key
  key <- cache_key(
    prompt = prompt,
    model = model,
    temperature = temperature,
    output_type = output_type,
    rollout_id = rollout_id,
    llm_id = llm_id
  )

  # Try to get from cache
  cached_result <- cache$get(key)

  if (!cachem::is.key_missing(cached_result)) {
    # Cache hit
    increment_cache_stats("hits")
    return(cached_result)
  }

  # Cache miss - make LLM call
  increment_cache_stats("misses")

  result <- llm$chat_structured(prompt, type = output_type, echo = "none")

  # Store in cache
  cache$set(key, result)

  result
}
