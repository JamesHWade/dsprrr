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
#' without writing into the current working directory.
#' @noRd
default_disk_cache_path <- function() {
  # Treat an empty-string env var the same as unset, so DSPRRR_CACHE_PATH=""
  # does not resolve the cache to "" (which would write into the working dir).
  path <- Sys.getenv("DSPRRR_CACHE_PATH", unset = "")
  if (nzchar(path)) path else ".dsprrr_cache"
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
#' **Disk permissions**: Private-directory enforcement is tracked separately.
#' Until it is available, choose a cache location with appropriately restricted
#' permissions, or use `enable_disk = FALSE` for sensitive workloads.
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
  disk_path = default_disk_cache_path(),
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
      disk_path = default_disk_cache_path(),
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

  if (
    !cachem::is.key_missing(cached_entry) &&
      is_cache_envelope(cached_entry) &&
      cache_replay_turn_delta(llm, cached_entry$turn_delta)
  ) {
    increment_cache_stats("hits")

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

  cache$set(key, cache_envelope(result, turn_delta))

  result
}
