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
