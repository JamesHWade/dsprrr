react_history_validate <- function(history) {
  history <- history_validate_envelope(history)
  if (!is.null(history$kind) && !identical(history$kind, "react")) {
    history_abort("Explicit history for ReAct must contain ReAct turns")
  }
  structure(history, class = c("dsprrr_history", "list"))
}

assert_explicit_react_history <- function(
  module,
  history,
  entrypoint = "run()"
) {
  if (is.null(history)) {
    return(invisible(FALSE))
  }
  if (
    !inherits(module, "Module") ||
      !identical(class(module)[[1L]], "ReactModule")
  ) {
    cli::cli_abort(
      c(
        "Explicit conversation history is supported only for a root ReAct module",
        "x" = "{.fn {entrypoint}} received {.cls {class(module)[[1L]]}}.",
        "i" = "Use a scalar {.fn react} call, or omit {.arg .history}."
      ),
      class = c(
        "dsprrr_history_unsupported_module",
        "dsprrr_history_error"
      )
    )
  }
  react_history_validate(history)
  invisible(TRUE)
}

assert_explicit_history_structured <- function(history, return_format) {
  if (
    !is.null(history) &&
      !identical(return_format, "structured")
  ) {
    cli::cli_abort(
      c(
        "Explicit conversation history requires structured results",
        "x" = "The simple result format would discard continuation state.",
        "i" = "Set {.arg .return_format} to {.val structured}."
      ),
      class = c(
        "dsprrr_history_result_format_error",
        "dsprrr_history_error"
      )
    )
  }
  invisible(NULL)
}

assert_explicit_history_scalar <- function(
  history,
  input_contract,
  entrypoint
) {
  if (
    !is.null(history) &&
      !identical(input_contract$kind, "scalar")
  ) {
    cli::cli_abort(
      c(
        "Explicit conversation history requires one scalar input",
        "x" = "{.fn {entrypoint}} received {.val {input_contract$kind}} inputs.",
        "i" = "Use {.fn run_dataset} only when no history is supplied."
      ),
      class = c(
        "dsprrr_history_batch_error",
        "dsprrr_history_error"
      )
    )
  }
  invisible(NULL)
}

reject_explicit_history_entrypoint <- function(history, entrypoint) {
  if (!is.null(history)) {
    cli::cli_abort(
      c(
        "Explicit conversation history is not supported by {.fn {entrypoint}}",
        "i" = "Use synchronous {.fn run} with a scalar root {.fn react} module and {.arg .return_format = \"structured\"}."
      ),
      class = c(
        "dsprrr_history_unsupported_entrypoint",
        "dsprrr_history_error"
      )
    )
  }
  invisible(NULL)
}

# Prepare an independent current Chat for one explicit-history ReAct call.
# The codec reconstructs only data; setting those turns never dispatches tools.
react_history_prepare <- function(chat, history) {
  history <- react_history_validate(history)
  cloned <- clone_ellmer_chat(chat, arg = "chat", reset_turns = TRUE)
  turns <- history_turns(history)
  if (length(turns) > 0L) {
    cloned$set_turns(turns)
  }

  seeded <- conversation_history(cloned$get_turns())
  if (!identical(seeded, history)) {
    history_abort("The cloned Chat did not retain the supplied ReAct history")
  }
  cloned
}

# Capture a completed prepared Chat as new portable history. The supplied
# history must still be the exact prefix, so callers cannot accidentally save
# a different Chat's conversation under this explicit-history request.
react_history_finish <- function(chat, history) {
  history <- react_history_validate(history)
  turns <- chat$get_turns()
  turn_count <- length(history$turns)
  if (length(turns) < turn_count) {
    history_abort("The completed Chat is missing supplied ReAct history")
  }
  prefix <- if (turn_count == 0L) {
    list()
  } else {
    turns[seq_len(turn_count)]
  }
  if (!identical(conversation_history(prefix), history)) {
    history_abort(
      "The completed Chat does not begin with supplied ReAct history"
    )
  }
  conversation_history(turns)
}
