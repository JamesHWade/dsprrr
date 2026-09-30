#' Default chat configuration
#'
#' The user-facing documentation, including the order in which dsprrr picks
#' a chat, is on the [get_default_chat()] page.
#'
#' @name default-chat
#' @noRd
NULL

# Package environment to store default chat, prompt history, scoped LM, and cache state
.dsprrr_env <- new.env(parent = emptyenv())
.dsprrr_env$prompt_history <- list()
.dsprrr_env$prompt_history_max <- 100L
.dsprrr_env$prompt_history_generation <- 0
.dsprrr_env$scoped_lm <- NULL
.dsprrr_env$cache_degraded <- FALSE
.dsprrr_env$cache_degraded_reason <- NULL
.dsprrr_env$cache_privacy_status <- "not_checked"
.dsprrr_env$cache_privacy_reason <- NULL
.dsprrr_env$cache_disk_guard <- NULL
.dsprrr_env$trace_context <- list()
.dsprrr_env$trace_program_artifact_id <- NA_character_
.dsprrr_env$trace_program <- NULL

#' Test whether a value is a current ellmer Chat
#' @noRd
is_ellmer_chat <- function(chat) {
  is.environment(chat) &&
    R6::is.R6(chat) &&
    inherits(chat, "Chat")
}

#' Validate a current ellmer Chat
#' @noRd
assert_ellmer_chat <- function(chat, arg = "chat", allow_null = FALSE) {
  if (isTRUE(allow_null) && is.null(chat)) {
    return(chat)
  }
  if (!is_ellmer_chat(chat)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be an ellmer Chat R6 object",
        "x" = "Got {.cls {class(chat)[1]}}.",
        "i" = "Create one with an {.code ellmer::chat_*()} constructor."
      ),
      class = "dsprrr_chat_type_error"
    )
  }
  chat
}

#' Deep-clone an ellmer Chat, optionally resetting its conversation
#' @noRd
clone_ellmer_chat <- function(chat, arg = "chat", reset_turns = TRUE) {
  chat <- assert_ellmer_chat(chat, arg = arg)
  clone <- tryCatch(chat[["clone"]], error = function(e) NULL)
  if (!is.function(clone)) {
    cli::cli_abort(
      c(
        "Cannot create an independent ellmer Chat",
        "x" = "{.arg {arg}} does not provide {.code clone()}.",
        "i" = "Supply a current ellmer Chat R6 object."
      ),
      class = c("dsprrr_chat_clone_error", "dsprrr_chat_isolation_error")
    )
  }

  cloned <- tryCatch(
    clone(deep = TRUE),
    error = function(e) {
      cli::cli_abort(
        "Failed to deep-clone {.arg {arg}}",
        class = c("dsprrr_chat_clone_error", "dsprrr_chat_isolation_error"),
        parent = e
      )
    }
  )
  if (
    !is_ellmer_chat(cloned) ||
      identical(rlang::obj_address(cloned), rlang::obj_address(chat))
  ) {
    cli::cli_abort(
      c(
        "Cannot create an independent ellmer Chat",
        "x" = "{.code clone(deep = TRUE)} did not return a new Chat R6 object."
      ),
      class = c("dsprrr_chat_clone_error", "dsprrr_chat_isolation_error")
    )
  }

  if (!isTRUE(reset_turns)) {
    return(cloned)
  }

  set_turns <- tryCatch(cloned[["set_turns"]], error = function(e) NULL)
  get_turns <- tryCatch(cloned[["get_turns"]], error = function(e) NULL)
  if (!is.function(set_turns) || !is.function(get_turns)) {
    cli::cli_abort(
      c(
        "Cannot reset the cloned ellmer Chat",
        "x" = "The clone must provide {.code set_turns()} and {.code get_turns()}."
      ),
      class = c("dsprrr_chat_reset_error", "dsprrr_chat_isolation_error")
    )
  }
  tryCatch(
    set_turns(list()),
    error = function(e) {
      cli::cli_abort(
        "Failed to reset the cloned ellmer Chat",
        class = c("dsprrr_chat_reset_error", "dsprrr_chat_isolation_error"),
        parent = e
      )
    }
  )
  turns <- tryCatch(
    get_turns(),
    error = function(e) {
      cli::cli_abort(
        "Failed to verify the cloned ellmer Chat history",
        class = c("dsprrr_chat_reset_error", "dsprrr_chat_isolation_error"),
        parent = e
      )
    }
  )
  if (!is.list(turns) || length(turns) != 0L) {
    cli::cli_abort(
      "The cloned ellmer Chat history could not be reset",
      class = c("dsprrr_chat_reset_error", "dsprrr_chat_isolation_error")
    )
  }
  cloned
}

#' Get, set or clear the default chat
#'
#' @description
#' `get_default_chat()` returns the ellmer Chat that dsprrr uses when neither
#' the call nor the module names one. `set_default_chat()` sets it for the
#' rest of the session; [dsp_configure()] does the same from a provider name.
#' `clear_default_chat()` forgets a chat that was created from an API key, so
#' the next call detects one again, for example after you change the key.
#'
#' @details
#' ## How dsprrr chooses a chat
#'
#' Each model call uses the first of these that is available:
#'
#' 1. The `.llm` argument of [run()], [run_dataset()], [evaluate()],
#'    [compile()] and the other functions that call models.
#' 2. The chat stored on the module, from the `chat` argument of [module()]
#'    and the other constructors.
#' 3. A chat set for a block of code with [with_lm()] or [local_lm()].
#' 4. The default chat set with `set_default_chat()` or [dsp_configure()]
#'    (stored in `options(dsprrr.default_chat)`).
#' 5. A chat created from the first API key found in the environment:
#'    `OPENAI_API_KEY` gives [ellmer::chat_openai()], `ANTHROPIC_API_KEY`
#'    gives [ellmer::chat_anthropic()] and `GOOGLE_API_KEY` gives
#'    [ellmer::chat_google_gemini()], each with the provider's default model.
#'    It is created once, with a message naming the provider and model (set
#'    `options(dsprrr.quiet = TRUE)` to silence it), and reused until
#'    `clear_default_chat()`.
#'
#' If none is available, the call fails with an error that explains how to
#' set one up. `get_default_chat()` returns the first of steps 3 to 5.
#'
#' @param create If `TRUE` (the default), create a chat from an API key when
#'   none is set (step 5), and error if that fails. If `FALSE`, return `NULL`
#'   instead of creating one.
#'
#' @return `get_default_chat()` returns an ellmer Chat, or `NULL` when
#'   `create = FALSE` and none is set. `set_default_chat()` returns the
#'   previous default (or `NULL`) invisibly, and `clear_default_chat()`
#'   returns `NULL` invisibly.
#'
#' @export
#' @family configuration
#' @examples
#' # Building a chat makes no request, so this runs without an API key
#' luna <- ellmer::chat_openai(model = "gpt-6-luna")
#' previous <- set_default_chat(luna)
#' get_default_chat()$get_model()
#'
#' # Restore the previous default (here: none)
#' set_default_chat(previous)
#'
#' # Forget a chat detected from an API key
#' clear_default_chat()
get_default_chat <- function(create = TRUE) {
  # Check for scoped LM first (highest priority after explicit .llm)
  scoped <- get_scoped_lm()
  if (!is.null(scoped)) {
    return(scoped)
  }

  # Check options
  chat <- getOption("dsprrr.default_chat")
  if (!is.null(chat)) {
    assert_ellmer_chat(chat, arg = "options(dsprrr.default_chat)")
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
      " " = "3. Pass a Chat to {.fn run}:",
      " " = paste0(
        "   {.code run(module(signature('q -> a')), q = 'Hello', ",
        ".llm = chat)}"
      )
    ))
  }

  # Cache the auto-detected chat
  .dsprrr_env$default_chat <- chat
  chat
}

#' @rdname get_default_chat
#' @param chat An ellmer Chat, or `NULL` to remove the default set earlier.
#' @export
set_default_chat <- function(chat) {
  assert_ellmer_chat(chat, arg = "chat", allow_null = TRUE)

  old <- getOption("dsprrr.default_chat")
  options(dsprrr.default_chat = chat)

  # Clear cached chat when explicitly setting
  .dsprrr_env$default_chat <- NULL

  invisible(old)
}

#' Provider whose API key is set in the environment
#'
#' Checks OpenAI, Anthropic, then Google, matching `auto_detect_chat()`.
#' @noRd
detect_env_provider <- function() {
  keys <- c(
    openai = "OPENAI_API_KEY",
    anthropic = "ANTHROPIC_API_KEY",
    google = "GOOGLE_API_KEY"
  )
  found <- names(keys)[nzchar(Sys.getenv(keys))]
  if (length(found) == 0) NULL else found[[1]]
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

#' @rdname get_default_chat
#' @export
clear_default_chat <- function() {
  .dsprrr_env$default_chat <- NULL
  .dsprrr_env$auto_detect_message_shown <- FALSE
  invisible(NULL)
}

# ── Scoped LM Functions ──────────────────────────────────────────────────────

#' Get Currently Scoped LM
#'
#' @description
#' Returns the currently active scoped LM set by `with_lm()` or `local_lm()`,
#' or `NULL` if no scoped LM is active.
#'
#' @return An ellmer Chat object, or `NULL`.
#'
#' @noRd
get_scoped_lm <- function() {
  .dsprrr_env$scoped_lm
}

#' Use a chat for a block of code
#'
#' @description
#' `with_lm()` evaluates `code` with `lm` as the chat for every dsprrr call in
#' it that does not name one, like DSPy's `dspy.context(lm = ...)`.
#' `local_lm()` does the same until the calling function returns. Either way,
#' the previous chat is restored afterwards, also after an error.
#'
#' @details
#' The scoped chat ranks below the `.llm` argument and below a chat stored on
#' the module, and above the default chat; see [get_default_chat()] for the
#' full order. A module created with a `chat` argument therefore keeps using
#' its own chat inside `with_lm()`. Blocks can be nested; the innermost chat
#' wins.
#'
#' @param lm An ellmer Chat. For `local_lm()`, `NULL` removes any scoped chat
#'   until the calling function returns.
#' @param code Code to evaluate with `lm` as the scoped chat.
#'
#' @return `with_lm()` returns the value of `code`. `local_lm()` returns the
#'   previous scoped chat (or `NULL`) invisibly.
#'
#' @export
#' @family configuration
#' @examples
#' # Building chats makes no request, so this runs without an API key
#' fast <- ellmer::chat_openai(
#'   model = "gpt-6-luna",
#'   params = ellmer::params(reasoning_effort = "low")
#' )
#' careful <- ellmer::chat_openai(
#'   model = "gpt-6-luna",
#'   params = ellmer::params(reasoning_effort = "high")
#' )
#'
#' with_lm(fast, identical(get_default_chat(), fast))
#' with_lm(fast, with_lm(careful, identical(get_default_chat(), careful)))
#'
#' review <- function() {
#'   local_lm(careful)
#'   # every dsprrr call from here to the end of the function uses `careful`
#'   identical(get_default_chat(), careful)
#' }
#' review()
#'
#' # Outside the block and the function, no scoped chat is left
#' identical(get_default_chat(create = FALSE), careful)
#'
#' \dontrun{
#' summarize <- module(signature("text -> summary"))
#' with_lm(fast, run(summarize, text = "A long article ..."))
#' }
with_lm <- function(lm, code) {
  # Validate input
  assert_ellmer_chat(lm, arg = "lm")

  local_lm(lm)
  code
}

#' @rdname with_lm
#' @param .env The environment whose exit ends the scope: by default, the
#'   function that calls `local_lm()` (cleanup uses [withr::defer()]).
#' @export
local_lm <- function(lm, .env = parent.frame()) {
  # Validate input (NULL is allowed to clear scoped LM)
  assert_ellmer_chat(lm, arg = "lm", allow_null = TRUE)

  # Store the previous value
  old <- .dsprrr_env$scoped_lm

  # Set new value
  .dsprrr_env$scoped_lm <- lm

  # Defer cleanup when .env exits
  withr::defer(
    {
      .dsprrr_env$scoped_lm <- old
    },
    envir = .env
  )

  invisible(old)
}

#' Configure the default chat from a provider name
#'
#' @description
#' `dsp_configure()` builds an ellmer Chat for a provider and makes it the
#' default chat, like DSPy's `dspy.configure(lm = ...)`. Modules use it when
#' neither the call nor the module names a chat; see [get_default_chat()]
#' for the full order.
#'
#' @param provider Character string specifying the provider. One of:
#'   `"openai"`, `"anthropic"`, `"google"`. If `NULL` (default), uses the
#'   first provider whose API key is set: `OPENAI_API_KEY`,
#'   `ANTHROPIC_API_KEY`, then `GOOGLE_API_KEY`.
#' @param model Character string specifying the model name. If `NULL`,
#'   uses the provider's default model.
#' @param api_key Character string with the API key. If `NULL`, reads from
#'   the appropriate environment variable.
#' @param temperature Sampling temperature, applied through
#'   `ellmer::params(temperature = )`. Default `NULL` uses the provider
#'   default. Reasoning models may ignore or reject it: gpt-6-luna accepts it
#'   only with `params = ellmer::params(reasoning_effort = "none")`.
#' @param ... Additional arguments passed to the ellmer chat constructor
#'   ([ellmer::chat_openai()], [ellmer::chat_anthropic()] or
#'   [ellmer::chat_google_gemini()]), such as `params` or `system_prompt`.
#'
#' @return The new default Chat, invisibly.
#'
#' @export
#' @family configuration
#' @examples
#' \dontrun{
#' # Configure with auto-detection (uses env vars)
#' dsp_configure()
#'
#' # Configure with specific provider and model
#' dsp_configure(provider = "openai", model = "gpt-6-luna")
#'
#' # Configure with temperature
#' dsp_configure(provider = "anthropic", temperature = 0.7)
#'
#' # gpt-6-luna takes a temperature only with reasoning turned off
#' dsp_configure(
#'   provider = "openai",
#'   model = "gpt-6-luna",
#'   temperature = 0,
#'   params = ellmer::params(reasoning_effort = "none")
#' )
#'
#' # Now run() uses this configuration
#' run(module(signature("question -> answer")), question = "What is 2+2?")
#' }
dsp_configure <- function(
  provider = NULL,
  model = NULL,
  api_key = NULL,
  temperature = NULL,
  ...
) {
  if (is.null(provider)) {
    provider <- detect_env_provider()
    if (is.null(provider)) {
      cli::cli_abort(c(
        "Could not auto-detect provider",
        "i" = "Set an API key environment variable or specify {.arg provider}"
      ))
    }
  }

  provider <- tolower(provider)
  valid_providers <- c("openai", "anthropic", "google")
  if (!provider %in% valid_providers) {
    cli::cli_abort(c(
      "Unknown provider: {.val {provider}}",
      "i" = "Valid providers: {.val {valid_providers}}"
    ))
  }

  # Build args for the ellmer chat constructor
  chat_args <- list(...)
  if (!is.null(model)) {
    chat_args$model <- model
  }
  if (!is.null(api_key)) {
    chat_args$api_key <- api_key
  }
  if (!is.null(temperature)) {
    chat_args$params <- chat_args$params %||% ellmer::params()
    chat_args$params$temperature <- temperature
  }

  chat <- switch(
    provider,
    "openai" = do.call(ellmer::chat_openai, chat_args),
    "anthropic" = do.call(ellmer::chat_anthropic, chat_args),
    "google" = do.call(ellmer::chat_google_gemini, chat_args)
  )

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
  provider_name <- tryCatch(
    chat$get_provider()@name,
    error = function(e) NULL
  )
  if (
    is.character(provider_name) &&
      length(provider_name) == 1L &&
      !is.na(provider_name) &&
      nzchar(provider_name)
  ) {
    return(provider_name)
  }

  "Unknown"
}

#' Report dsprrr's configuration
#'
#' @description
#' `dsprrr_sitrep()` prints what dsprrr will use and what it has done this
#' session: package versions, the default chat, which API keys are set, the
#' prompt history, dsprrr options and the response cache. It is modelled on
#' `usethis::git_sitrep()` and is a good first step when calls do not behave
#' as expected.
#'
#' @details
#' The default chat shown is the one [get_default_chat()] returns with
#' `create = FALSE`, so a chat that would be created from an API key on first
#' use is reported as not configured. The report makes no model calls.
#'
#' @return A list, invisibly, with `dsprrr_version`, `ellmer_version`,
#'   `has_default_chat`, `provider` and `model` of the default chat,
#'   `api_keys` (whether `OPENAI_API_KEY`, `ANTHROPIC_API_KEY` and
#'   `GOOGLE_API_KEY` are set), `n_calls` and `prompt_history_count` (entries
#'   in the prompt history), `prompt_history_max`, and the cache settings and
#'   state (`cache_enabled`, `cache_disk_path`, `cache_disk_private`,
#'   `cache_degraded`, `cache_privacy_status` and, when caching is on,
#'   `cache_stats`). Once calls have been recorded, it also has
#'   `total_tokens_in`, `total_tokens_out` and `total_cost`.
#'
#' @export
#' @family configuration
#' @examples
#' status <- dsprrr_sitrep()
#' status$has_default_chat
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
    sum_history_field <- function(field) {
      values <- vapply(
        history,
        function(entry) as.numeric(entry[[field]] %||% 0),
        numeric(1)
      )
      if (anyNA(values)) NA_real_ else sum(values)
    }
    total_tokens_in <- sum_history_field("tokens_in")
    total_tokens_out <- sum_history_field("tokens_out")
    total_cost <- sum_history_field("cost")

    result$total_tokens_in <- total_tokens_in
    result$total_tokens_out <- total_tokens_out
    result$total_cost <- total_cost

    cli::cli_bullets(c("*" = "LLM calls: {n_calls}"))

    if (is.na(total_tokens_in) || is.na(total_tokens_out)) {
      cli::cli_bullets(c("*" = "Tokens: unknown"))
    } else if (total_tokens_in > 0 || total_tokens_out > 0) {
      cli::cli_bullets(c(
        "*" = "Tokens: {format(total_tokens_in, big.mark = ',')} in / {format(total_tokens_out, big.mark = ',')} out"
      ))
    }

    if (is.na(total_cost)) {
      cli::cli_bullets(c("*" = "Est. cost: unknown"))
    } else if (total_cost > 0) {
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

  # ── Cache section ──
  cli::cli_h2("Cache")

  cache_config <- get_cache_config()
  result$cache_enabled <- cache_config$enable
  result$cache_disk_path <- cache_config$disk_path
  result$cache_disk_private <- cache_config$disk_private
  result$cache_degraded <- isTRUE(.dsprrr_env$cache_degraded)
  result$cache_degraded_reason <- .dsprrr_env$cache_degraded_reason
  result$cache_privacy_status <- .dsprrr_env$cache_privacy_status %||%
    "not_checked"
  result$cache_privacy_reason <- .dsprrr_env$cache_privacy_reason

  if (!cache_config$enable) {
    cli::cli_bullets(c("x" = "Caching disabled"))
    env_var <- Sys.getenv("DSPRRR_CACHE_ENABLED", unset = "")
    if (nzchar(env_var)) {
      cli::cli_text(
        "  {.emph Disabled via {.envvar DSPRRR_CACHE_ENABLED={env_var}}}"
      )
    }
  } else {
    # Show enabled tiers
    tiers <- character()
    if (cache_config$enable_memory) {
      tiers <- c(tiers, "memory")
    }
    if (cache_config$enable_disk) {
      tiers <- c(tiers, "disk")
    }

    cli::cli_bullets(c("v" = "Cache tiers: {paste(tiers, collapse = ', ')}"))

    if (cache_config$enable_disk) {
      cli::cli_bullets(c("*" = "Disk path: {.path {cache_config$disk_path}}"))
      privacy_message <- switch(
        result$cache_privacy_status,
        "verified_posix_modes" = paste0(
          "Effective owner and POSIX modes verified (extended ACLs not checked)"
        ),
        "unverified_windows" = "Disk privacy relies on unverified inherited ACLs",
        "disabled" = "Owner-only disk enforcement disabled (trusted cache only)",
        "degraded" = if (cache_config$enable_memory) {
          "Disk cache unavailable; using memory only"
        } else {
          "Disk cache unavailable; no cache tier remains enabled"
        },
        "Private disk permissions will be checked on first use"
      )
      privacy_bullet <- if (
        result$cache_privacy_status == "verified_posix_modes"
      ) {
        "v"
      } else if (result$cache_privacy_status == "degraded") {
        "x"
      } else {
        "i"
      }
      cli::cli_bullets(stats::setNames(privacy_message, privacy_bullet))
    }

    if (result$cache_degraded && !is.null(result$cache_degraded_reason)) {
      cli::cli_bullets(c("!" = "Disk cache: {result$cache_degraded_reason}"))
    }

    # Cache statistics
    stats <- cache_stats()
    result$cache_stats <- stats

    if (stats$hits > 0 || stats$misses > 0) {
      cli::cli_bullets(c(
        "*" = "Hit rate: {format(stats$hit_rate * 100, digits = 1)}%",
        "*" = "Requests: {stats$hits + stats$misses} ({stats$hits} hits, {stats$misses} misses)"
      ))

      if (!is.null(stats$memory_entries) && stats$memory_entries > 0) {
        cli::cli_bullets(c("*" = "Memory entries: {stats$memory_entries}"))
      }

      if (!is.null(stats$disk_entries) && stats$disk_entries > 0) {
        cli::cli_bullets(c("*" = "Disk entries: {stats$disk_entries}"))
      }
    } else {
      cli::cli_bullets(c("i" = "No cache activity yet"))
    }
  }

  cli::cat_line()

  invisible(result)
}

#' Summarize this session's token use and cost
#'
#' @description
#' `session_cost()` adds up the tokens and estimated cost of the model calls
#' in the prompt history (see [inspect_history()]), overall and per model.
#'
#' @details
#' The prompt history keeps the most recent 100 calls by default
#' (`options(dsprrr.prompt_history_max = )`), so older calls drop out of the
#' totals, and [clear_prompt_history()] resets them. Only calls recorded in
#' the history count: see [inspect_history()] for which ones are. Costs are
#' ellmer's estimates.
#'
#' @return A list of class `dsprrr_session_cost` with `n_calls`, `tokens_in`,
#'   `tokens_out`, `total_tokens`, `cost` (in US dollars; `NA` if any call's
#'   cost is unknown) and `by_model`, a tibble with the same totals per model.
#'
#' @export
#' @family inspection
#' @examples
#' session_cost()
#' session_cost()$total_tokens
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
    tokens_in = vapply(
      unique_models,
      function(m) {
        sum(vapply(
          history[models == m],
          function(e) e$tokens_in %||% 0L,
          integer(1)
        ))
      },
      integer(1)
    ),
    tokens_out = vapply(
      unique_models,
      function(m) {
        sum(vapply(
          history[models == m],
          function(e) e$tokens_out %||% 0L,
          integer(1)
        ))
      },
      integer(1)
    ),
    cost = vapply(
      unique_models,
      function(m) {
        sum(vapply(
          history[models == m],
          function(e) e$cost %||% 0,
          numeric(1)
        ))
      },
      numeric(1)
    )
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

  if (isTRUE(x$cost > 0)) {
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

  # Keep this aligned with the minimum declared in DESCRIPTION.
  min_version <- "0.5.0"

  tryCatch(
    {
      comparison <- suppressWarnings(
        utils::compareVersion(version, min_version)
      )
      isTRUE(comparison >= 0)
    },
    error = function(e) FALSE
  )
}
