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

# Package environment to store default chat
.dsprrr_env <- new.env(parent = emptyenv())

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
    return(ellmer::chat_openai())
  }

  # Check for Anthropic
  if (nzchar(Sys.getenv("ANTHROPIC_API_KEY"))) {
    return(ellmer::chat_claude())
  }

  # Check for Google
  if (nzchar(Sys.getenv("GOOGLE_API_KEY"))) {
    return(ellmer::chat_google_gemini())
  }

  NULL
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
  invisible(NULL)
}
