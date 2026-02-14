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

  # Reset degraded state - reconfiguration might fix previous issues
  .dsprrr_env$cache_degraded <- FALSE
  .dsprrr_env$cache_degraded_reason <- NULL

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

  # Reset stats and first-hit flag
  .dsprrr_env$cache_stats <- list(hits = 0L, misses = 0L)
  .dsprrr_env$cache_first_hit_shown <- FALSE
  .dsprrr_env$cache_degraded <- FALSE
  .dsprrr_env$cache_degraded_reason <- NULL

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
  output_type_repr <- serialize_output_type(output_type)

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

#' Serialize Output Type to Stable Representation
#'
#' @description
#' Convert an ellmer output_type to a stable JSON representation for cache keys.
#' Handles ellmer Type objects and falls back gracefully on errors.
#'
#' @param output_type An ellmer Type object or other value
#'
#' @return Character representation suitable for cache key generation
#'
#' @noRd
serialize_output_type <- function(output_type) {
  tryCatch(
    {
      if (is_ellmer_type(output_type)) {
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
}

#' Check if Value is an Ellmer Type
#'
#' @description
#' Determine if a value is an ellmer Type object that needs JSON serialization.
#'
#' @param x Value to check
#'
#' @return Logical TRUE if x is an ellmer Type
#'
#' @noRd
is_ellmer_type <- function(x) {
  inherits(x, "TypeObject") ||
    inherits(x, "TypeString") ||
    inherits(x, "TypeEnum")
}

#' Extract Temperature from LLM Configuration
#'
#' @description
#' Safely extract the temperature setting from an ellmer LLM object.
#' Returns NULL if temperature cannot be accessed.
#'
#' @param llm An ellmer Chat object
#'
#' @return Numeric temperature value or NULL
#'
#' @noRd
extract_llm_temperature <- function(llm) {
  tryCatch(
    {
      # Access provider's temperature setting from internal api_args
      llm$.__enclos_env__$private$api_args$temperature
    },
    error = function(e) NULL
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
  # Create directory if needed
  if (!dir.exists(disk_path)) {
    dir_ok <- create_cache_directory(disk_path)
    if (!dir_ok) {
      return(NULL)
    }
  }

  # Create the disk cache object
  tryCatch(
    {
      cachem::cache_disk(
        dir = disk_path,
        max_size = config$disk_max_size,
        max_age = config$disk_max_age
      )
    },
    error = function(e) {
      # Track degraded state for dsprrr_sitrep()
      .dsprrr_env$cache_degraded <- TRUE
      .dsprrr_env$cache_degraded_reason <- e$message

      cli::cli_warn(c(
        "!" = "Failed to create disk cache at {.path {disk_path}}",
        "i" = "Falling back to memory-only cache (will not persist across R sessions)",
        "x" = "Error: {e$message}",
        "i" = "To fix: Check disk space, permissions, and filesystem health"
      ))
      NULL
    }
  )
}

#' Create Cache Directory
#'
#' @description
#' Create cache directory, handling race conditions where another process
#' creates the directory between our check and mkdir.
#'
#' @param disk_path Character path for disk cache directory
#'
#' @return Logical TRUE if directory exists or was created, FALSE if error
#'
#' @noRd
create_cache_directory <- function(disk_path) {
  tryCatch(
    {
      dir.create(disk_path, recursive = TRUE, showWarnings = TRUE)
      TRUE
    },
    warning = function(w) {
      # Race condition: another process created directory between our check and mkdir
      if (dir.exists(disk_path)) {
        # Directory now exists - this is benign, proceed with disk caching
        return(TRUE)
      }
      # Directory still doesn't exist - this is a real problem
      cli::cli_warn(c(
        "Could not create cache directory: {.path {disk_path}}",
        "i" = "Disk caching will be disabled for this session",
        "x" = w$message
      ))
      FALSE
    },
    error = function(e) {
      cli::cli_warn(c(
        "Error creating cache directory: {.path {disk_path}}",
        "i" = "Disk caching will be disabled for this session",
        "x" = e$message
      ))
      FALSE
    }
  )
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
  .cache = NULL
) {
  # Per-call override takes precedence over global config
  use_cache <- if (!is.null(.cache)) {
    isTRUE(.cache)
  } else {
    cache_enabled()
  }

  # If caching disabled (globally or per-call), make direct call
  if (!use_cache) {
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
      # Only warn for real LLMs (not mock LLMs in tests)
      # Mock LLMs typically error with "attempt to apply non-function"
      if (!grepl("attempt to apply non-function", e$message)) {
        cli::cli_warn(c(
          "Could not extract model name from LLM for cache key",
          "i" = "Using 'unknown' as fallback - cache may be less effective",
          "x" = "Error: {e$message}"
        ))
      }
      "unknown"
    }
  )

  # Get temperature from LLM config if available
  temperature <- extract_llm_temperature(llm)

  # Get LLM identity for mock LLMs to prevent cache collisions
  llm_id <- if (model == "unknown") {
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

    # Show first-hit message if this is the first cache hit this session
    if (!isTRUE(.dsprrr_env$cache_first_hit_shown)) {
      cli::cli_inform(c(
        "i" = "Using cached LLM responses",
        "i" = "Disable with {.code configure_cache(enable = FALSE)} or {.code .cache = FALSE}"
      ))
      .dsprrr_env$cache_first_hit_shown <- TRUE
    }

    # Inject synthetic turns so callers see a valid user/assistant exchange
    # even though the LLM was not actually called.
    inject_cache_hit_turns(llm, prompt, cached_result)

    return(cached_result)
  }

  # Cache miss - make LLM call
  increment_cache_stats("misses")

  result <- llm$chat_structured(prompt, type = output_type, echo = "none")

  # Store in cache
  cache$set(key, result)

  result
}

#' Inject synthetic turns into a Chat after a cache hit
#'
#' When `cached_chat_structured()` returns a cached result the underlying Chat
#' object has no record of the exchange. This helper appends a synthetic
#' user/assistant turn pair so that downstream code (trace recording, metadata
#' extraction via `last_turn()`, `inspect_history()`) sees a valid conversation.
#'
#' The function is intentionally defensive: if the Chat lacks `get_turns` or
#' `set_turns` (e.g. a minimal mock), it silently does nothing.
#'
#' @param llm An ellmer Chat object (or mock).
#' @param prompt Character. The full prompt that was sent (already includes
#'   instructions if any).
#' @param result The cached LLM response.
#'
#' @noRd
inject_cache_hit_turns <- function(llm, prompt, result) {
  # Bail out if the Chat doesn't support turn manipulation.
  # Both get_turns and set_turns are required — without get_turns we can't

  # preserve existing history, so calling set_turns would silently wipe it.
  set_turns_fn <- tryCatch(llm$set_turns, error = function(e) NULL)
  if (!is.function(set_turns_fn)) {
    return(invisible(NULL))
  }

  get_turns_fn <- tryCatch(llm$get_turns, error = function(e) NULL)
  if (!is.function(get_turns_fn)) {
    return(invisible(NULL))
  }

  existing_turns <- tryCatch(get_turns_fn(), error = function(e) list())

  # Serialize the cached response for the assistant content

  response_text <- if (is.character(result) && length(result) == 1) {
    result
  } else {
    as.character(jsonlite::toJSON(result, auto_unbox = TRUE))
  }

  user_turn <- ellmer::UserTurn(
    contents = list(ellmer::ContentText(as.character(prompt)))
  )
  assistant_turn <- ellmer::AssistantTurn(
    contents = list(ellmer::ContentText(response_text))
  )

  set_turns_fn(c(existing_turns, list(user_turn, assistant_turn)))
  invisible(NULL)
}
