#' Commit an async trace and its global history record transactionally
#' @noRd
commit_async_trace_transaction <- function(
  module,
  trace,
  keep_local_on_error = FALSE
) {
  module_traces_before <- module$state$traces
  history_before <- .dsprrr_env$prompt_history
  history_generation_before <- .dsprrr_env$prompt_history_generation
  commit_error <- tryCatch(
    {
      commit_run_traces(
        module,
        list(trace),
        source = class(module)[[1L]]
      )
      NULL
    },
    error = function(error) error
  )
  if (inherits(commit_error, "condition")) {
    module$state$traces <- module_traces_before
    .dsprrr_env$prompt_history <- history_before
    .dsprrr_env$prompt_history_generation <- history_generation_before
    if (isTRUE(keep_local_on_error)) {
      module$state$traces <- append(module$state$traces, list(trace))
      return(list(committed = FALSE, local_only = TRUE, error = commit_error))
    }
    stop(commit_error)
  }
  list(committed = TRUE, local_only = FALSE, error = NULL)
}
