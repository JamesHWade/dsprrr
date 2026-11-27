#' LLM response caching
#'
#' The user-facing documentation is on the [configure_cache()] page.
#'
#' @name cache
#' @noRd
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

#' Configure the response cache
#'
#' @description
#' dsprrr caches model responses, so repeating a request returns the stored
#' answer without calling the model. That makes re-running code faster and
#' cheaper. There are two tiers, both on by default: a memory cache for the
#' session and a disk cache that lasts across sessions. `configure_cache()`
#' changes the settings for the rest of the session.
#'
#' @details
#' ## What counts as the same request
#'
#' A cached response is used only when everything that could change the
#' answer matches: provider, model, parameters such as temperature, system
#' prompt, conversation history, prompt and output schema. [best_of_n()],
#' [refine()] and [with_assertions()] attempts, and [evaluate()] epochs after
#' the first, use separate cache partitions, so they get fresh responses.
#' Chats with registered tools are never cached.
#'
#' To skip the cache for one call, pass `.cache = FALSE` to [run()],
#' [run_dataset()] or [evaluate()]. To turn caching off for a whole
#' environment, for example on CI, set the environment variable
#' `DSPRRR_CACHE_ENABLED=false` (or `0`, `no`, `off`); `DSPRRR_CACHE_PATH`
#' moves the default disk location.
#'
#' ## Privacy
#'
#' Cache files hold the requests and the parsed responses, and, where
#' needed, the conversation turns used to restore a Chat after a cache hit.
#' Treat the disk cache as sensitive data.
#'
#' The default disk location is the per-user cache directory from
#' [tools::R_user_dir()]. On Unix, dsprrr reads and writes it only if the
#' directory has mode `0700`, every response file has mode `0600`, the
#' effective user owns them, and every existing parent directory belongs to
#' root or that user. A cache that fails these checks (other modes, special
#' mode bits, symbolic links, files that are not regular, or ownership that
#' cannot be verified) is neither changed nor read: dsprrr falls back to the
#' memory cache if it is enabled and otherwise runs uncached. On Windows, the
#' directory inherits the account's access rules, which base R cannot verify.
#'
#' These checks cannot see extended ACLs, stop administrators, or cover
#' network file systems that ignore local modes, and a process running as
#' the same user could swap files between a check and a read. Avoid shared or
#' network cache paths for sensitive work. Set `disk_private = FALSE` only
#' for a cache whose readers and writers you all trust: a writable shared
#' cache could replace a stored response, which dsprrr reads back with
#' `readRDS()`.
#'
#' If you point the disk cache inside a project, add the directory (for
#' example `.dsprrr_cache/`) to `.gitignore`.
#'
#' @param enable Turn all caching on or off.
#' @param enable_memory Use the memory cache.
#' @param enable_disk Use the disk cache.
#' @param disk_path Directory for the disk cache. Defaults to
#'   `tools::R_user_dir("dsprrr", "cache")`, or `DSPRRR_CACHE_PATH` when set.
#' @param disk_private If `TRUE` (the default), enforce the ownership and
#'   permission checks described under Privacy. `FALSE` is only for a trusted
#'   shared cache.
#' @param memory_max_entries Maximum number of responses in the memory cache
#'   (least recently used ones are dropped first).
#' @param disk_max_size Maximum size of the disk cache in bytes (500 MB by
#'   default).
#' @param disk_max_age Maximum age of disk entries in seconds. `Inf` (the
#'   default) keeps them until they are pruned for size.
#'
#' @return The previous settings, invisibly, as a list that can be passed back
#'   with `do.call(configure_cache, old)`. `NULL` if the settings had not been
#'   read yet in this session.
#'
#' @export
#' @family configuration
#' @examples
#' cache_stats()
#'
#' # Turn caching off for a while, then restore the previous settings
#' old <- configure_cache(enable = FALSE)
#' cache_stats()$enabled
#' do.call(configure_cache, old)
#' cache_stats()$enabled
#'
#' \dontrun{
#' # Keep responses in memory only
#' configure_cache(enable_disk = FALSE)
#'
#' # A larger cache in another directory
#' configure_cache(disk_path = "~/.dsprrr_cache", disk_max_size = 1024^3)
#'
#' # A shared cache that every user of the directory trusts
#' configure_cache(disk_path = "/srv/team/dsprrr-cache", disk_private = FALSE)
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

  # Reset cache objects and degraded state to force re-creation with new config
  reset_cache_env_state(reset_stats = FALSE)

  invisible(old_config)
}

reset_cache_env_state <- function(reset_stats = FALSE) {
  .dsprrr_env$cache <- NULL
  .dsprrr_env$cache_memory <- NULL
  .dsprrr_env$cache_disk <- NULL
  .dsprrr_env$cache_disk_guard <- NULL
  .dsprrr_env$cache_degraded <- FALSE
  .dsprrr_env$cache_degraded_reason <- NULL
  .dsprrr_env$cache_privacy_status <- "not_checked"
  .dsprrr_env$cache_privacy_reason <- NULL
  if (isTRUE(reset_stats)) {
    .dsprrr_env$cache_stats <- list(hits = 0L, misses = 0L)
    .dsprrr_env$cache_first_hit_shown <- FALSE
  }
  invisible(NULL)
}

cache_recompose_active_tiers <- function() {
  config <- get_cache_config()
  if (!isTRUE(config$enable)) {
    .dsprrr_env$cache <- NULL
    return(invisible(NULL))
  }

  memory_cache <- if (isTRUE(config$enable_memory)) {
    .dsprrr_env$cache_memory
  } else {
    NULL
  }
  disk_cache <- if (
    isTRUE(config$enable_disk) && !isTRUE(.dsprrr_env$cache_degraded)
  ) {
    .dsprrr_env$cache_disk
  } else {
    NULL
  }

  .dsprrr_env$cache <- if (is.null(memory_cache)) {
    disk_cache
  } else if (is.null(disk_cache)) {
    memory_cache
  } else {
    cachem::cache_layered(memory_cache, disk_cache)
  }
  invisible(.dsprrr_env$cache)
}

#' Clear the response cache
#'
#' @description
#' `clear_cache()` deletes cached model responses from memory, from disk or
#' both, and resets the hit and miss counts reported by [cache_stats()].
#'
#' @details
#' The disk tier is cleared only if it has been opened in this session,
#' which happens at the first cached model call. In a fresh session,
#' `clear_cache()` leaves the directory on disk alone; delete the directory
#' shown by [dsprrr_sitrep()] to remove it.
#'
#' @param which Which tier to clear: `"all"` (the default), `"memory"` or
#'   `"disk"`.
#'
#' @return `TRUE`, invisibly. If a tier cannot be cleaned up, the cache is
#'   still detached and an error lists the tiers that failed.
#'
#' @export
#' @family configuration
#' @examples
#' clear_cache("memory")
#' cache_stats()
#'
#' \dontrun{
#' # Delete every cached response, on disk too
#' clear_cache()
#' }
clear_cache <- function(which = c("all", "memory", "disk")) {
  which <- match.arg(which)

  memory_cache <- .dsprrr_env$cache_memory
  disk_cache <- .dsprrr_env$cache_disk
  disk_guard <- .dsprrr_env$cache_disk_guard

  # Detach internal handles before invoking user-space cachem methods. Cleanup
  # failures must never leave a stale layered object reachable from get_cache().
  if (which == "all") {
    reset_cache_env_state(reset_stats = TRUE)
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
  if (which != "all") {
    .dsprrr_env$cache_stats <- list(hits = 0L, misses = 0L)
    .dsprrr_env$cache_first_hit_shown <- FALSE
  }

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
      result <- disk_cache[[disk_method]]()
      if (which == "disk") {
        cache_verify_disk_cleanup_guard(disk_guard)
      }
      result
    })
  }

  # A targeted clear empties a tier; it does not change the configured cache
  # topology. Reattach only a tier whose cleanup completed successfully, then
  # rebuild the layer from the still-valid handles. Failed tiers stay detached.
  if (which == "memory" && !is.null(memory_cache) && is.null(errors$memory)) {
    config <- get_cache_config()
    if (isTRUE(config$enable) && isTRUE(config$enable_memory)) {
      .dsprrr_env$cache_memory <- memory_cache
    }
  }
  if (which == "disk" && !is.null(disk_cache) && is.null(errors$disk)) {
    config <- get_cache_config()
    if (
      isTRUE(config$enable) &&
        isTRUE(config$enable_disk) &&
        !isTRUE(.dsprrr_env$cache_degraded)
    ) {
      .dsprrr_env$cache_disk <- disk_cache
      .dsprrr_env$cache_disk_guard <- disk_guard
    }
  }
  if (which != "all") {
    cache_recompose_active_tiers()
  }

  # Tier methods may invoke guard callbacks that mutate package state. Restore
  # the detached state again before reporting any collected cleanup error.
  if (which == "all") {
    reset_cache_env_state(reset_stats = TRUE)
  } else {
    .dsprrr_env$cache_stats <- list(hits = 0L, misses = 0L)
    .dsprrr_env$cache_first_hit_shown <- FALSE
  }

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

#' Report response-cache statistics
#'
#' @description
#' `cache_stats()` reports whether caching is on, how many requests were
#' served from the cache (hits) or sent to the model (misses) since the
#' cache was last cleared, and how many responses each tier holds.
#'
#' @return A list of class `dsprrr_cache_stats` with `enabled`, `hits`,
#'   `misses`, `hit_rate` (hits as a share of all requests), and
#'   `memory_entries` and `disk_entries` once those tiers are in use. If the
#'   disk cache failed its privacy checks, also `degraded = TRUE` and
#'   `degraded_reason`. Printing it gives a short report.
#'
#' @export
#' @family configuration
#' @examples
#' stats <- cache_stats()
#' stats
#' stats$hit_rate
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

  # A rejected disk tier leaves `enabled` TRUE, which on its own reads as a
  # healthy cache. Report the degradation so the drop is visible here too.
  if (isTRUE(.dsprrr_env$cache_degraded)) {
    result$degraded <- TRUE
    result$degraded_reason <- .dsprrr_env$cache_degraded_reason
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

  if (isTRUE(x$degraded)) {
    degraded_reason <- x$degraded_reason %||%
      "the disk cache could not be trusted"
    cli::cli_bullets(c(
      "!" = "Disk caching is degraded",
      "x" = "{degraded_reason}"
    ))
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

#' Call `Chat$chat_structured()` with a prompt of one or more parts
#'
#' ellmer takes each prompt part (text or a content object such as an image)
#' as a separate argument, so a multimodal payload list is spliced in.
#' @noRd
chat_structured_parts <- function(llm, prompt, output_type) {
  args <- c(prompt_parts(prompt), list(type = output_type, echo = "none"))
  args$run_context <- chat_run_context(llm)
  do.call(llm$chat_structured, args)
}

#' Split a prompt payload into the parts ellmer expects
#' @noRd
prompt_parts <- function(prompt) {
  if (is.list(prompt) && !S7::S7_inherits(prompt)) {
    unname(prompt)
  } else {
    list(prompt)
  }
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
    return(chat_structured_parts(llm, prompt, output_type))
  }

  cache <- get_cache()
  if (is.null(cache)) {
    observe("bypass", "unavailable")
    return(chat_structured_parts(llm, prompt, output_type))
  }
  disk_guard <- .dsprrr_env$cache_disk_guard
  rollout_id <- scoped_rollout_id(rollout_id)

  fingerprint <- tryCatch(
    cache_request_fingerprint(
      llm = llm,
      payload = prompt,
      output_type = output_type,
      rollout_id = rollout_id
    ),
    error = function(e) e
  )

  if (inherits(fingerprint, "condition")) {
    if (inherits(fingerprint, "dsprrr_cache_untrusted_chat")) {
      observe("bypass", "untrusted_chat")
      return(chat_structured_parts(llm, prompt, output_type))
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
          "x" = "{conditionMessage(fingerprint)}",
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
    return(chat_structured_parts(llm, prompt, output_type))
  }

  key <- cache_key(fingerprint)

  # Try to get from cache
  cached_entry <- cache$get(key)
  disk_guard_failed <- is.environment(disk_guard) &&
    isTRUE(disk_guard$invalid)
  if (disk_guard_failed) {
    cache_report_disk_guard_failure(disk_guard)
    observe("bypass", "disk_trust_failed")
    return(chat_structured_parts(llm, prompt, output_type))
  }

  if (
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
        "x" = "{conditionMessage(before)}"
      ),
      .frequency = "once",
      .frequency_id = "cache-turn-replay-unavailable"
    )
    return(chat_structured_parts(llm, prompt, output_type))
  }

  result <- chat_structured_parts(llm, prompt, output_type)
  turn_delta <- tryCatch(
    cache_turn_delta(llm, before),
    error = function(e) e
  )
  if (inherits(turn_delta, "condition")) {
    cli::cli_warn(
      c(
        "LLM response was not cached because its turn history is not replayable",
        "x" = "{conditionMessage(turn_delta)}"
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
        "x" = "{conditionMessage(set_error)}",
        "i" = "The provider result is still being returned."
      ),
      class = "dsprrr_cache_write_warning"
    )
  }

  result
}
