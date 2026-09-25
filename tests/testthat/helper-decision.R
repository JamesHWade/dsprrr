# Build a mock Chat that answers decision evidence requests.
#
# `evidence` is a function(input, field, field_type) returning the raw
# structured value for one output field. `input` is the value of the module's
# first input, recovered from the prompt. The mock records every request type
# in `calls`.
new_decision_chat <- function(evidence, input_name = "text") {
  calls <- new.env(parent = emptyenv())
  calls$types <- list()
  chat <- new_test_chat(
    chat_structured = function(prompt, type, ...) {
      calls$types[[length(calls$types) + 1L]] <- type
      text <- if (is.character(prompt)) prompt else prompt[[1]]@text
      pattern <- paste0(input_name, ": ([^\n]*)")
      input <- sub(pattern, "\\1", regmatches(text, regexpr(pattern, text)))
      out <- lapply(names(type@properties), function(field) {
        evidence(input, field, type@properties[[field]])
      })
      stats::setNames(out, names(type@properties))
    }
  )
  list(chat = chat, calls = calls)
}

is_evidence_type <- function(type) {
  inherits(type, "ellmer::TypeObject") &&
    any(c("probability", "probabilities") %in% names(type@properties))
}
