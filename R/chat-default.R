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
#' Displays the current dsprrr configuration, available API keys, and
#' session usage statistics. Inspired by `usethis::git_sitrep()`.
#'
#' @return Invisibly returns a list with configuration details.
#'
#' @export
#' @examples
#' \dontrun{
#' dsprrr_sitrep()
#' #> dsprrr configuration
#' #> --------------------
#' #>
#' #> Default provider: OpenAI (gpt-4o-mini)
#' #> Source: Auto-detected from OPENAI_API_KEY
#' #>
#' #> API keys found:
#' #>   OPENAI_API_KEY
#' #>   ANTHROPIC_API_KEY
#' #>
#' #> Session usage:
#' #>   LLM calls: 12
#' #>   Tokens: 2,450 in / 890 out
#' #>   Est. cost: $0.02
#' }
dsprrr_sitrep <- function() {
  cli::cli_h1("dsprrr configuration")

  # Check for default chat
  chat <- tryCatch(
    get_default_chat(create = FALSE),
    error = function(e) NULL
  )

  # Default provider section
  cli::cli_h2("Default Provider")

  if (!is.null(chat)) {
    model <- tryCatch(chat$get_model(), error = function(e) "unknown")
    provider <- detect_provider_name(chat)

    cli::cli_text("{.strong {provider}} ({model})")

    # Check source of configuration
    if (!is.null(getOption("dsprrr.default_chat"))) {
      cli::cli_text("{.emph Source: Explicitly set via options}")
    } else if (!is.null(.dsprrr_env$config)) {
      cli::cli_text("{.emph Source: Configured via dsp_configure()}")
    } else {
      cli::cli_text("{.emph Source: Auto-detected}")
    }
  } else {
    cli::cli_text("{.emph Not configured}")
    cli::cli_text("Run {.code dsp_configure()} or set an API key")
  }

  cli::cat_line()

  # API keys section
  cli::cli_h2("API Keys")

  api_keys <- list(
    OPENAI_API_KEY = nzchar(Sys.getenv("OPENAI_API_KEY")),
    ANTHROPIC_API_KEY = nzchar(Sys.getenv("ANTHROPIC_API_KEY")),
    GOOGLE_API_KEY = nzchar(Sys.getenv("GOOGLE_API_KEY"))
  )

  found_keys <- names(api_keys)[unlist(api_keys)]
  missing_keys <- names(api_keys)[!unlist(api_keys)]

  if (length(found_keys) > 0) {
    cli::cli_text("Found:")
    for (key in found_keys) {
      cli::cli_bullets(c("v" = "{key}"))
    }
  }

  if (length(missing_keys) > 0) {
    cli::cli_text("Not set:")
    for (key in missing_keys) {
      cli::cli_bullets(c("x" = "{key}"))
    }
  }

  cli::cat_line()

  # Session usage section
  cli::cli_h2("Session Usage")

  history <- .dsprrr_env$prompt_history %||% list()
  n_calls <- length(history)

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

    cli::cli_text("LLM calls: {.strong {n_calls}}")
    cli::cli_text(
      "Tokens: {.strong {format(total_tokens_in, big.mark = ',')}} in / {.strong {format(total_tokens_out, big.mark = ',')}} out"
    )

    if (total_cost > 0) {
      cli::cli_text(
        "Est. cost: {.strong ${format(total_cost, digits = 2, nsmall = 2)}}"
      )
    }
  } else {
    cli::cli_text("{.emph No LLM calls recorded this session}")
  }

  cli::cat_line()

  # Package info
  cli::cli_h2("Package Info")
  pkg_version <- tryCatch(
    as.character(utils::packageVersion("dsprrr")),
    error = function(e) "unknown"
  )
  ellmer_version <- tryCatch(
    as.character(utils::packageVersion("ellmer")),
    error = function(e) "unknown"
  )

  cli::cli_text("dsprrr: {.strong {pkg_version}}")
  cli::cli_text("ellmer: {.strong {ellmer_version}}")

  # Return invisibly
  invisible(list(
    has_default_chat = !is.null(chat),
    provider = if (!is.null(chat)) detect_provider_name(chat) else NA_character_,
    api_keys = api_keys,
    n_calls = n_calls
  ))
}
