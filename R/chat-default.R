#' Default Chat Configuration
#'
#' @description
#' Functions for managing the default ellmer Chat object used by dsprrr.
#' When no Chat is explicitly provided to `dsp()` or `module()`, these
#' functions determine which Chat to use.
#'
#' @details
#' The default Chat is resolved in this order:
#' 1. Explicit `options(dsprrr.default_chat = chat_object)`
#' 2. Auto-detection from environment variables:
#'    - `OPENAI_API_KEY` → `ellmer::chat_openai()`
#'    - `ANTHROPIC_API_KEY` → `ellmer::chat_claude()`
#' 3. Error with helpful setup instructions
#'
#' @name default-chat
NULL

# Package environment to store default chat and prompt history
.dsprrr_env <- new.env(parent = emptyenv())
.dsprrr_env$prompt_history <- list()
.dsprrr_env$prompt_history_max <- 100L

#' Get the Default Chat
#'
#' @description
#' Retrieves the default Chat object for dsprrr operations. If no default
#' is set, attempts to auto-detect from environment variables.
#'
#' @param create Logical. If `TRUE` (default), create a Chat from environment
#'   variables if none is explicitly set. If `FALSE`, return `NULL` when no
#'   default is available.
#'
#' @return An ellmer Chat object, or `NULL` if `create = FALSE` and no
#'   default is available.
#'
#' @export
#' @examples
#' \dontrun{
#' # Set a default chat
#' set_default_chat(ellmer::chat_openai())
#'
#' # Get the default chat
#' chat <- get_default_chat()
#'
#' # Check if a default is available without creating one
#' chat <- get_default_chat(create = FALSE)
#' }
get_default_chat <- function(create = TRUE) {
  # Check options first
  chat <- getOption("dsprrr.default_chat")
  if (!is.null(chat)) {
    if (!inherits(chat, "Chat")) {
      cli::cli_abort(c(
        "Invalid default chat in options",
        "x" = "{.code options(dsprrr.default_chat)} must be an ellmer Chat object",
        "i" = "Create one with {.code ellmer::chat_openai()} or similar"
      ))
    }
    return(chat)
  }

  # Check cached chat in package environment
  if (!is.null(.dsprrr_env$default_chat)) {
    return(.dsprrr_env$default_chat)
  }

  if (!create) {
    return(NULL)
  }

  # Auto-detect from environment variables
  chat <- auto_detect_chat()

  if (is.null(chat)) {
    cli::cli_abort(c(
      "No default Chat available",
      "i" = "Set up a default Chat using one of these methods:",
      " " = "1. Set an API key environment variable:",
      " " = "   {.code Sys.setenv(OPENAI_API_KEY = 'your-key')}",
      " " = "   {.code Sys.setenv(ANTHROPIC_API_KEY = 'your-key')}",
      " " = "2. Set a default Chat explicitly:",
      " " = "   {.code options(dsprrr.default_chat = ellmer::chat_openai())}",
      " " = "3. Pass a Chat to the function:",
      " " = "   {.code chat |> dsp('q -> a', q = 'Hello')}"
    ))
  }

  # Cache the auto-detected chat
  .dsprrr_env$default_chat <- chat
  chat
}

#' Set the Default Chat
#'
#' @description
#' Sets the default Chat object for dsprrr operations. This is stored
#' in R options and persists for the session.
#'
#' @param chat An ellmer Chat object, or `NULL` to clear the default.
#'
#' @return Invisibly returns the previous default Chat (if any).
#'
#' @export
#' @examples
#' \dontrun{
#' # Set a default OpenAI chat
#' set_default_chat(ellmer::chat_openai())
#'
#' # Set a default Claude chat
#' set_default_chat(ellmer::chat_claude())
#'
#' # Clear the default
#' set_default_chat(NULL)
#' }
set_default_chat <- function(chat) {
  if (!is.null(chat) && !inherits(chat, "Chat")) {
    cli::cli_abort(c(
      "Invalid chat object",
      "x" = "{.arg chat} must be an ellmer Chat object or NULL",
      "i" = "Create one with {.code ellmer::chat_openai()} or similar"
    ))
  }

  old <- getOption("dsprrr.default_chat")
  options(dsprrr.default_chat = chat)

  # Clear cached chat when explicitly setting
  .dsprrr_env$default_chat <- NULL

  invisible(old)
}

#' Auto-detect Chat from Environment
#'
#' @description
#' Attempts to create an ellmer Chat object by detecting API keys
#' in environment variables.
#'
#' @return An ellmer Chat object, or `NULL` if no API keys are found.
#'
#' @noRd
auto_detect_chat <- function() {
  # Check for OpenAI
  if (nzchar(Sys.getenv("OPENAI_API_KEY"))) {
    chat <- ellmer::chat_openai()
    emit_auto_detection_message("OpenAI", chat)
    return(chat)
  }

  # Check for Anthropic
  if (nzchar(Sys.getenv("ANTHROPIC_API_KEY"))) {
    chat <- ellmer::chat_claude()
    emit_auto_detection_message("Anthropic", chat)
    return(chat)
  }

  # Check for Google
  if (nzchar(Sys.getenv("GOOGLE_API_KEY"))) {
    chat <- ellmer::chat_google_gemini()
    emit_auto_detection_message("Google", chat)
    return(chat)
  }

  NULL
}

#' Emit Auto-Detection Message
#'
#' @description
#' Prints a subtle message when a provider is auto-detected.
#' Only shows once per session unless cleared.
#'
#' @param provider Character string naming the provider
#' @param chat The ellmer Chat object
#'
#' @noRd
emit_auto_detection_message <- function(provider, chat) {
  # Skip if quiet mode is enabled

  if (isTRUE(getOption("dsprrr.quiet", FALSE))) {
    return(invisible(NULL))
  }

  # Skip if message was already shown this session
  if (isTRUE(.dsprrr_env$auto_detect_message_shown)) {
    return(invisible(NULL))
  }

  # Get model name from chat

  model <- tryCatch(
    chat$get_model(),
    error = function(e) "default model"
  )

  # Get env var name based on provider

  env_var <- switch(
    provider,
    "OpenAI" = "OPENAI_API_KEY",
    "Anthropic" = "ANTHROPIC_API_KEY",
    "Google" = "GOOGLE_API_KEY",
    "API_KEY"
  )

  cli::cli_inform(
    c("i" = "Using {provider} ({model}) from {env_var}"),
    class = "dsprrr_auto_detect_message"
  )

  # Mark as shown for this session
  .dsprrr_env$auto_detect_message_shown <- TRUE
  invisible(NULL)
}

#' Clear Cached Default Chat
#'
#' @description
#' Clears any cached default Chat. Useful for testing or when
#' environment variables change.
#'
#' @return Invisibly returns `NULL`.
#'
#' @export
#' @examples
#' \dontrun{
#' # Clear cached chat (will re-detect on next use)
#' clear_default_chat()
#' }
clear_default_chat <- function() {
  .dsprrr_env$default_chat <- NULL
  .dsprrr_env$auto_detect_message_shown <- FALSE
  invisible(NULL)
}

#' Configure dsprrr Default Settings
#'
#' @description
#' Configure the default LLM provider and settings for dsprrr. Similar to
#' DSPy's `dspy.configure(lm=lm)`, this sets up a default Chat that will be
#' used by `dsp()` and modules when no explicit Chat is provided.
#'
#' @param provider Character string specifying the provider. One of:
#'   `"openai"`, `"anthropic"`, `"google"`. If `NULL` (default), auto-detects
#'   from environment variables.
#' @param model Character string specifying the model name. If `NULL`,
#'   uses the provider's default model.
#' @param api_key Character string with the API key. If `NULL`, reads from
#'   the appropriate environment variable.
#' @param temperature Numeric value for temperature (0-2). Default is `NULL`
#'   (use provider default).
#' @param ... Additional arguments passed to the ellmer chat constructor.
#'
#' @return Invisibly returns the configured Chat object.
#'
#' @export
#' @examples
#' \dontrun{
#' # Configure with auto-detection (uses env vars)
#' dsp_configure()
#'
#' # Configure with specific provider and model
#' dsp_configure(provider = "openai", model = "gpt-4o-mini")
#'
#' # Configure with temperature
#' dsp_configure(provider = "anthropic", model = "claude-3-5-sonnet-latest",
#'               temperature = 0.7)
#'
#' # Now dsp() uses this configuration
#' dsp("question -> answer", question = "What is 2+2?")
#' }
dsp_configure <- function(
  provider = NULL,
  model = NULL,
  api_key = NULL,
  temperature = NULL,
  ...
) {
  # Build chat based on provider
  if (is.null(provider)) {
    # Auto-detect from environment
    chat <- auto_detect_chat()
    if (is.null(chat)) {
      cli::cli_abort(c(
        "Could not auto-detect provider",
        "i" = "Set an API key environment variable or specify {.arg provider}"
      ))
    }
  } else {
    # Validate provider
    provider <- tolower(provider)
    valid_providers <- c("openai", "anthropic", "google")
    if (!provider %in% valid_providers) {
      cli::cli_abort(c(
        "Unknown provider: {.val {provider}}",
        "i" = "Valid providers: {.val {valid_providers}}"
      ))
    }

    # Build args for chat constructor
    chat_args <- list(...)

    if (!is.null(model)) {
      chat_args$model <- model
    }

    if (!is.null(api_key)) {
      chat_args$api_key <- api_key
    }

    # Create chat based on provider
    chat <- switch(
      provider,
      "openai" = do.call(ellmer::chat_openai, chat_args),
      "anthropic" = do.call(ellmer::chat_claude, chat_args),
      "google" = do.call(ellmer::chat_google_gemini, chat_args)
    )
  }

  # Store configuration metadata
  .dsprrr_env$config <- list(
    provider = provider,
    model = model,
    temperature = temperature,
    configured_at = Sys.time()
  )

  # Set as default
  set_default_chat(chat)

  # Show confirmation unless quiet
  if (!isTRUE(getOption("dsprrr.quiet", FALSE))) {
    model_name <- tryCatch(
      chat$get_model(),
      error = function(e) model %||% "default"
    )
    provider_name <- provider %||% detect_provider_name(chat)

    cli::cli_inform(c(
      "v" = "Configured dsprrr with {provider_name} ({model_name})"
    ))
  }

  invisible(chat)
}

#' Detect Provider Name from Chat
#'
#' @noRd
detect_provider_name <- function(chat) {
  # Try to use get_provider() method if available (ellmer 0.4+)
  provider_obj <- tryCatch(
    chat$get_provider(),
    error = function(e) NULL
  )

  if (!is.null(provider_obj)) {
    # ellmer returns an S7 Provider object with @name slot
    provider_name <- tryCatch(
      provider_obj@name,
      error = function(e) NULL
    )

    if (!is.null(provider_name) && is.character(provider_name) &&
      nzchar(provider_name)) {
      return(provider_name)
    }

    # Check provider class name as fallback
    provider_class <- class(provider_obj)[1]
    if (grepl("OpenAI", provider_class)) {
      return("OpenAI")
    }
    if (grepl("Claude|Anthropic", provider_class)) {
      return("Anthropic")
    }
    if (grepl("Google|Gemini", provider_class)) {
      return("Google")
    }
  }

  # Final fallback: check chat class name
  class_name <- class(chat)[1]
  if (grepl("openai", class_name, ignore.case = TRUE)) {
    return("OpenAI")
  }
  if (grepl("claude|anthropic", class_name, ignore.case = TRUE)) {
    return("Anthropic")
  }
  if (grepl("google|gemini", class_name, ignore.case = TRUE)) {
    return("Google")
  }

  "Unknown"
}

#' dsprrr Situation Report
#'
#' @description
#' Displays a comprehensive overview of your dsprrr configuration,
#' including API keys, default chat settings, prompt history, and
#' package versions. Inspired by `usethis::git_sitrep()`.
#'
#' @return Invisibly returns a list with configuration details:
#'   - `has_default_chat`: Logical, whether a default chat is configured
#'   - `provider`: Character, name of the default provider
#'   - `model`: Character, name of the default model
#'   - `api_keys`: Named list of API key availability (logical)
#'   - `n_calls`: Integer, number of LLM calls this session
#'   - `prompt_history_count`: Integer, entries in prompt history
#'   - `prompt_history_max`: Integer, maximum history size
#'   - `ellmer_version`: Character, installed ellmer version
#'   - `dsprrr_version`: Character, installed dsprrr version
#'
#' @export
#' @examples
#' \dontrun{
#' dsprrr_sitrep()
#' #> dsprrr configuration
#' #> ────────────────────────────────────────────────────────
#' #>
#' #> ── Packages ──
#' #> ✔ ellmer 0.2.0 (OK)
#' #> ✔ dsprrr 0.1.0
#' #>
#' #> ── Default Chat ──
#' #> ✔ OpenAI (gpt-4o-mini)
#' #>   Source: Auto-detected from OPENAI_API_KEY
#' #>
#' #> ── API Keys ──
#' #> ✔ OPENAI_API_KEY
#' #> ✔ ANTHROPIC_API_KEY
#' #> ✖ GOOGLE_API_KEY
#' #>
#' #> ── Session State ──
#' #> • Prompt history: 12 / 100 entries
#' #> • LLM calls: 15
#' #> • Tokens: 2,450 in / 890 out
#' #>
#' #> ── Options ──
#' #> • dsprrr.verbose: TRUE
#' #> • dsprrr.quiet: FALSE
#' }
dsprrr_sitrep <- function() {
  cli::cli_h1("dsprrr configuration")

  # Collect data for return value
  result <- list()

  # ── Packages section ──
  cli::cli_h2("Packages")

  pkg_version <- tryCatch(
    as.character(utils::packageVersion("dsprrr")),
    error = function(e) "not installed"
  )
  ellmer_version <- tryCatch(
    as.character(utils::packageVersion("ellmer")),
    error = function(e) "not installed"
  )

  result$dsprrr_version <- pkg_version
  result$ellmer_version <- ellmer_version

  # Check ellmer minimum version

  ellmer_ok <- check_ellmer_version(ellmer_version)

  if (ellmer_ok) {
    cli::cli_bullets(c("v" = "ellmer {ellmer_version} (OK)"))
  } else {
    cli::cli_bullets(c(
      "!" = "ellmer {ellmer_version} (update recommended)",
      " " = "  Run: {.code install.packages('ellmer')}"
    ))
  }

  cli::cli_bullets(c("v" = "dsprrr {pkg_version}"))

  cli::cat_line()

  # ── Default Chat section ──
  cli::cli_h2("Default Chat")

  chat <- tryCatch(
    get_default_chat(create = FALSE),
    error = function(e) NULL
  )

  if (!is.null(chat)) {
    model <- tryCatch(chat$get_model(), error = function(e) "unknown")
    provider <- detect_provider_name(chat)

    result$has_default_chat <- TRUE
    result$provider <- provider
    result$model <- model

    cli::cli_bullets(c("v" = "{provider} ({model})"))

    # Check source of configuration
    source_msg <- if (!is.null(getOption("dsprrr.default_chat"))) {
      "Explicitly set via {.code options(dsprrr.default_chat = ...)}"
    } else if (!is.null(.dsprrr_env$config)) {
      "Configured via {.code dsp_configure()}"
    } else {
      "Auto-detected from environment"
    }
    cli::cli_text("  {.emph {source_msg}}")
  } else {
    result$has_default_chat <- FALSE
    result$provider <- NA_character_
    result$model <- NA_character_

    cli::cli_bullets(c("x" = "Not configured"))
    cli::cli_text("  Run {.code dsp_configure()} or set an API key")
  }

  cli::cat_line()

  # ── API Keys section ──
  cli::cli_h2("API Keys")

  api_keys <- list(
    OPENAI_API_KEY = nzchar(Sys.getenv("OPENAI_API_KEY")),
    ANTHROPIC_API_KEY = nzchar(Sys.getenv("ANTHROPIC_API_KEY")),
    GOOGLE_API_KEY = nzchar(Sys.getenv("GOOGLE_API_KEY"))
  )

  result$api_keys <- api_keys

  for (key_name in names(api_keys)) {
    if (api_keys[[key_name]]) {
      cli::cli_bullets(c("v" = "{key_name}"))
    } else {
      cli::cli_bullets(c("x" = "{key_name}"))
    }
  }

  cli::cat_line()

  # ── Session State section ──
  cli::cli_h2("Session State")

  history <- .dsprrr_env$prompt_history %||% list()
  n_calls <- length(history)
  history_max <- .dsprrr_env$prompt_history_max %||% 100L

  result$n_calls <- n_calls
  result$prompt_history_count <- n_calls
  result$prompt_history_max <- history_max

  # Prompt history
  cli::cli_bullets(c(
    "*" = "Prompt history: {n_calls} / {history_max} entries"
  ))

  if (n_calls > 0) {
    # Aggregate stats
    total_tokens_in <- 0L
    total_tokens_out <- 0L
    total_cost <- 0

    for (entry in history) {
      total_tokens_in <- total_tokens_in + (entry$tokens_in %||% 0L)
      total_tokens_out <- total_tokens_out + (entry$tokens_out %||% 0L)
      total_cost <- total_cost + (entry$cost %||% 0)
    }

    result$total_tokens_in <- total_tokens_in
    result$total_tokens_out <- total_tokens_out
    result$total_cost <- total_cost

    cli::cli_bullets(c("*" = "LLM calls: {n_calls}"))

    if (total_tokens_in > 0 || total_tokens_out > 0) {
      cli::cli_bullets(c(
        "*" = "Tokens: {format(total_tokens_in, big.mark = ',')} in / {format(total_tokens_out, big.mark = ',')} out"
      ))
    }

    if (total_cost > 0) {
      cli::cli_bullets(c(
        "*" = "Est. cost: ${format(total_cost, digits = 2, nsmall = 2)}"
      ))
    }
  }

  cli::cat_line()

  # ── Options section ──
  cli::cli_h2("Options")

  # List relevant dsprrr options
  dsprrr_options <- list(
    "dsprrr.verbose" = getOption("dsprrr.verbose"),
    "dsprrr.quiet" = getOption("dsprrr.quiet"),
    "dsprrr.default_return_format" = getOption("dsprrr.default_return_format")
  )

  # Filter to only set options
  set_options <- dsprrr_options[!vapply(dsprrr_options, is.null, logical(1))]

  if (length(set_options) > 0) {
    for (opt_name in names(set_options)) {
      opt_val <- set_options[[opt_name]]
      cli::cli_bullets(c("*" = "{opt_name}: {.val {opt_val}}"))
    }
  } else {
    cli::cli_text("{.emph Using defaults (no options set)}")
  }

  cli::cat_line()

  invisible(result)
}

#' Session Cost Summary
#'
#' @description
#' Get cost and token usage summary for the current dsprrr session.
#' This aggregates data from all LLM calls tracked in the prompt history.
#'
#' @return A list with:
#'   - `n_calls`: Integer, number of LLM calls
#'   - `tokens_in`: Integer, total input tokens
#'   - `tokens_out`: Integer, total output tokens
#'   - `total_tokens`: Integer, sum of input and output tokens
#'   - `cost`: Numeric, total estimated cost in USD
#'   - `by_model`: A tibble with per-model breakdown (if available)
#'
#' @export
#' @examples
#' \dontrun{
#' # After running some dsp() calls
#' dsp("question -> answer", question = "What is 2+2?")
#' dsp("question -> answer", question = "What is the capital of France?")
#'
#' # Get session summary
#' session_cost()
#' #> $n_calls
#' #> [1] 2
#' #> $tokens_in
#' #> [1] 45
#' #> $tokens_out
#' #> [1] 12
#' #> $cost
#' #> [1] 0.0001
#'
#' # Access total cost directly
#' session_cost()$cost
#' }
session_cost <- function() {
  history <- .dsprrr_env$prompt_history %||% list()

  if (length(history) == 0) {
    return(structure(
      list(
        n_calls = 0L,
        tokens_in = 0L,
        tokens_out = 0L,
        total_tokens = 0L,
        cost = 0,
        by_model = tibble::tibble(
          model = character(0),
          n_calls = integer(0),
          tokens_in = integer(0),
          tokens_out = integer(0),
          cost = numeric(0)
        )
      ),
      class = "dsprrr_session_cost"
    ))
  }

  # Aggregate totals
  tokens_in <- sum(vapply(
    history,
    function(e) e$tokens_in %||% 0L,
    integer(1)
  ))
  tokens_out <- sum(vapply(
    history,
    function(e) e$tokens_out %||% 0L,
    integer(1)
  ))
  cost <- sum(vapply(
    history,
    function(e) e$cost %||% 0,
    numeric(1)
  ))

  # Build per-model breakdown
  models <- vapply(
    history,
    function(e) e$model %||% "unknown",
    character(1)
  )
  unique_models <- unique(models)

  by_model <- tibble::tibble(
    model = unique_models,
    n_calls = vapply(unique_models, function(m) sum(models == m), integer(1)),
    tokens_in = vapply(unique_models, function(m) {
      sum(vapply(
        history[models == m],
        function(e) e$tokens_in %||% 0L,
        integer(1)
      ))
    }, integer(1)),
    tokens_out = vapply(unique_models, function(m) {
      sum(vapply(
        history[models == m],
        function(e) e$tokens_out %||% 0L,
        integer(1)
      ))
    }, integer(1)),
    cost = vapply(unique_models, function(m) {
      sum(vapply(
        history[models == m],
        function(e) e$cost %||% 0,
        numeric(1)
      ))
    }, numeric(1))
  )

  structure(
    list(
      n_calls = length(history),
      tokens_in = tokens_in,
      tokens_out = tokens_out,
      total_tokens = tokens_in + tokens_out,
      cost = cost,
      by_model = by_model
    ),
    class = "dsprrr_session_cost"
  )
}

#' @export
print.dsprrr_session_cost <- function(x, ...) {
  cli::cli_h3("dsprrr Session Cost")

  if (x$n_calls == 0) {
    cli::cli_text("{.emph No LLM calls recorded in this session}")
    return(invisible(x))
  }

  cli::cli_bullets(c(
    "*" = "LLM calls: {x$n_calls}",
    "*" = "Tokens: {format(x$tokens_in, big.mark = ',')} in / {format(x$tokens_out, big.mark = ',')} out",
    "*" = "Total: {format(x$total_tokens, big.mark = ',')} tokens"
  ))

  if (x$cost > 0) {
    cli::cli_bullets(c(
      "*" = "Est. cost: ${format(x$cost, digits = 4, nsmall = 4)}"
    ))
  }

  if (nrow(x$by_model) > 1) {
    cli::cli_text("")
    cli::cli_text("{.emph By model:}")
    for (i in seq_len(nrow(x$by_model))) {
      row <- x$by_model[i, ]
      cli::cli_bullets(c(
        " " = "{row$model}: {row$n_calls} call{?s}, {row$tokens_in + row$tokens_out} tokens, ${format(row$cost, digits = 4)}"
      ))
    }
  }

  invisible(x)
}

#' Check ellmer Version Compatibility
#'
#' @description
#' Checks if the installed ellmer version meets minimum requirements.
#'
#' @param version Character string of ellmer version.
#'
#' @return Logical, TRUE if version is compatible.
#'
#' @noRd
check_ellmer_version <- function(version) {
  if (version == "not installed") {
    return(FALSE)
  }


  # Minimum recommended ellmer version
  min_version <- "0.1.0"

  tryCatch(
    {
      utils::compareVersion(version, min_version) >= 0
    },
    error = function(e) TRUE # Assume OK if we can't parse
  )
}
