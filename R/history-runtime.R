react_history_validate <- function(history) {
  history <- history_validate_envelope(history)
  if (!is.null(history$kind) && !identical(history$kind, "react")) {
    history_abort("Explicit history for ReAct must contain ReAct turns")
  }
  structure(history, class = c("dsprrr_history", "list"))
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
