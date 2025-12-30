# Setup vcr for tests
if (requireNamespace("vcr", quietly = TRUE)) {
  library(vcr)

  vcr::vcr_configure(
    dir = vcr::vcr_test_path("_vcr"),
    filter_sensitive_data = list(
      "<OPENAI_KEY>" = Sys.getenv("OPENAI_API_KEY"),
      "<ANTHROPIC_KEY>" = Sys.getenv("ANTHROPIC_API_KEY"),
      "<GEMINI_KEY>" = Sys.getenv("GOOGLE_GEMINI_API_KEY")
    ),
    # Don't record these headers
    filter_request_headers = c("Authorization", "x-api-key"),
    # Match requests on method + uri
    match_requests_on = c("method", "uri")
  )
}

# Helper for mocked chat objects (when not using vcr)
MockedChat <- R6::R6Class(
  "MockedChat",
  inherit = ellmer::Chat,
  public = list(
    i = 0,
    saved_chats = character(),

    initialize = function(saved_chats) {
      self$saved_chats <- saved_chats
    },

    chat = function(...) {
      self$i <- self$i + 1
      if (self$i > length(self$saved_chats)) {
        stop("No more mocked responses available")
      }
      self$saved_chats[self$i]
    },

    chat_structured = function(prompt, type, ...) {
      self$i <- self$i + 1
      if (self$i > length(self$saved_chats)) {
        stop("No more mocked responses available")
      }
      # Return pre-defined structured responses
      self$saved_chats[self$i]
    }
  )
)

mocked_chat <- function(chats) {
  MockedChat$new(saved_chats = chats)
}

# Check if we should use real API calls
use_real_api <- function() {
  dsprrr:::has_ellmer_credentials() && !is_ci()
}

# Check if running on CI
is_ci <- function() {
  isTRUE(as.logical(Sys.getenv("CI")))
}
