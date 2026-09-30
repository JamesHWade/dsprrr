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
