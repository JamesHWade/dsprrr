offline_chat_class <- R6::R6Class(
  "Chat",
  public = list(
    turns = NULL,
    session_id = NULL,
    initialize = function(model, responder, audit, session_id) {
      self$turns <- list()
      self$session_id <- session_id
      private$model <- ellmer::Model(name = model, extra_args = list())
      private$provider <- ellmer::Provider(
        name = "offline-fixture",
        base_url = ""
      )
      private$responder <- responder
      private$audit <- audit
    },
    chat = function(prompt, ...) {
      private$call(prompt, type = NULL, ...)
    },
    chat_structured = function(prompt, type, ...) {
      private$call(prompt, type = type, ...)
    },
    stream = function(...) character(),
    get_turns = function() self$turns,
    set_turns = function(turns) {
      self$turns <- turns
      invisible(self)
    },
    last_turn = function(role = NULL) {
      if (length(self$turns) == 0L) NULL else self$turns[[length(self$turns)]]
    },
    get_model = function() private$model@name,
    get_provider = function() private$provider,
    get_model_object = function() private$model
  ),
  private = list(
    model = NULL,
    provider = NULL,
    responder = NULL,
    audit = NULL,
    call = function(prompt, type, ...) {
      prompt <- paste(as.character(prompt), collapse = "\n")
      prior_turns <- length(self$turns)
      private$audit$calls <- append(
        private$audit$calls,
        list(list(
          session_id = self$session_id,
          prior_turns = prior_turns,
          prompt = prompt
        ))
      )
      response <- private$responder(
        prompt,
        type,
        session_id = self$session_id,
        prior_turns = prior_turns,
        ...
      )
      response
    }
  ),
  lock_objects = FALSE,
  parent_env = globalenv()
)

new_offline_audit <- function() {
  audit <- new.env(parent = emptyenv())
  audit$calls <- list()
  audit$clone_count <- 0L
  audit$clone_sessions <- character()
  audit
}

new_offline_chat <- function(model, responder, audit, session_id = model) {
  chat <- offline_chat_class$new(model, responder, audit, session_id)
  clone_chat <- function(deep = TRUE) {
    audit$clone_count <- audit$clone_count + 1L
    new_session <- paste0(model, "-fresh-", audit$clone_count)
    audit$clone_sessions <- c(audit$clone_sessions, new_session)
    cloned <- new_offline_chat(model, responder, audit, new_session)
    cloned$set_turns(chat$get_turns())
    cloned
  }
  unlockBinding("clone", chat)
  assign("clone", clone_chat, envir = chat)
  lockBinding("clone", chat)
  chat
}

read_prompt_line <- function(prompt, prefix) {
  lines <- strsplit(prompt, "\n", fixed = TRUE)[[1L]]
  matches <- grep(paste0("^", prefix), lines, value = TRUE)
  if (length(matches) != 1L) {
    stop("Offline task fixture expected one prompt line beginning: ", prefix)
  }
  sub(paste0("^", prefix), "", matches[[1L]])
}

extract_requested_value <- function(context, question) {
  if (grepl("year", question, ignore.case = TRUE)) {
    value <- regmatches(context, regexpr("\\b20[0-9]{2}\\b", context))
    return(if (length(value) == 0L) "unknown" else value)
  }
  if (grepl("business days", context, ignore.case = TRUE)) {
    value <- regmatches(
      context,
      regexpr("\\b[0-9]+ business days\\b", context, ignore.case = TRUE)
    )
    return(if (length(value) == 0L) "unknown" else value)
  }
  if (grepl("weekday", question, ignore.case = TRUE)) {
    weekdays <- c(
      "Monday",
      "Tuesday",
      "Wednesday",
      "Thursday",
      "Friday",
      "Saturday",
      "Sunday"
    )
    matches <- weekdays[vapply(
      weekdays,
      grepl,
      logical(1),
      x = context,
      ignore.case = TRUE
    )]
    return(if (length(matches) == 0L) "unknown" else matches[[1L]])
  }
  if (grepl("how many", question, ignore.case = TRUE)) {
    value <- regmatches(context, regexpr("\\b[0-9]+\\b", context))
    return(if (length(value) == 0L) "unknown" else value)
  }
  "unknown"
}

task_audit <- new_offline_audit()
task_responder <- function(prompt, type, ...) {
  lines <- strsplit(prompt, "\n", fixed = TRUE)[[1L]]
  evidence_prompt <- any(grepl("^Question: ", lines)) &&
    any(grepl("^Context: ", lines))
  if (evidence_prompt) {
    context <- read_prompt_line(prompt, "Context: ")
    question <- read_prompt_line(prompt, "Question: ")
    if (
      grepl("write only that value", prompt, fixed = TRUE) &&
        any(grepl("^Extracted value:$", lines))
    ) {
      extract_requested_value(context, question)
    } else {
      context
    }
  } else {
    evidence <- if (any(grepl("^Evidence block:$", lines))) {
      index <- match("Evidence block:", lines) + 1L
      lines[[index]]
    } else {
      read_prompt_line(prompt, "Evidence: ")
    }
    if (
      grepl("complete evidence value unchanged", prompt, fixed = TRUE) &&
        any(grepl("^Evidence block:$", lines))
    ) {
      evidence
    } else {
      strsplit(evidence, " ", fixed = TRUE)[[1L]][[1L]]
    }
  }
}

task_chat <- new_offline_chat(
  "offline-task",
  task_responder,
  task_audit
)

evidence_instructions <- paste(
  "Extract the exact fact requested by the question from the context;",
  "write only that value."
)
evidence_template <- paste(
  "Question: {question}",
  "Context: {context}",
  "Extracted value:",
  sep = "\n"
)
answer_instructions <- paste(
  "Return the complete evidence value unchanged, preserving multiple words."
)
answer_template <- paste(
  "Evidence block:",
  "{evidence}",
  "Copy the complete value:",
  sep = "\n"
)

edit_evidence_only <- list(
  path = "$/steps/1",
  instructions = evidence_instructions,
  template = evidence_template
)
edit_answer <- list(
  path = "$/steps/2",
  instructions = answer_instructions,
  template = answer_template
)

candidate <- function(name, rationale, edits, parent_id = NULL) {
  proposal <- list(name = name, rationale = rationale, edits = edits)
  if (!is.null(parent_id)) {
    proposal$parent_id <- parent_id
  }
  proposal
}

action_propose <- function(rationale, candidates) {
  list(action = "propose", rationale = rationale, candidates = candidates)
}

result_summary <- function(result) {
  data.frame(
    optimizer = result$optimizer,
    status = result$status,
    baseline_score = result$baseline_score,
    best_score = result$best_score,
    stop_reason = result$stop_reason,
    row.names = NULL
  )
}

budget_summary <- function(result) {
  budget <- result$budget
  limits <- budget$limits
  data.frame(
    trials = budget$trials,
    metric_calls = budget$metric_calls,
    provider_calls = budget$provider_calls,
    errors = budget$total_errors,
    elapsed_seconds = budget$elapsed_seconds,
    stopped = budget$stopped,
    max_trials = limits$max_trials,
    max_metric_calls = limits$max_metric_calls,
    max_provider_calls = limits$max_provider_calls,
    row.names = NULL
  )
}
