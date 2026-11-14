gepa_test_metadata <- function(program) {
  result <- optimization_result(program)
  utils::modifyList(
    result$extensions$gepa,
    list(
      budget_summary = result$budget,
      stop_reason = result$budget$stop_reason,
      error_count = result$budget$total_errors,
      partial = identical(result$status, "partial")
    )
  )
}

gepa_test_reflection_chat <- function(
  instructions = "Test reflection proposal"
) {
  new_test_chat(
    model = "gepa-test-reflection",
    chat_structured = function(prompt, type, ...) {
      if (!identical(names(type@properties), "instructions")) {
        stop("unexpected GEPA test reflection schema")
      }
      list(instructions = instructions)
    }
  )
}
