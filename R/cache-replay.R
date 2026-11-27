# Safe cache envelope serialization and turn-history replay

#' Validate and copy semantic turn values without retaining executable objects
#' @noRd
cache_replay_value <- function(x) {
  validate <- function(value) {
    if (is.null(value) || is.atomic(value)) {
      return(invisible(NULL))
    }
    if (is.list(value) && !inherits(value, "S7_object")) {
      lapply(value, validate)
      return(invisible(NULL))
    }
    cli::cli_abort(
      "Turn replay contains unsupported {.cls {class(value)[1]}} state",
      class = "dsprrr_cache_replay_error"
    )
  }
  validate(x)
  unserialize(serialize(x, connection = NULL, version = 2))
}

#' Clone one content object for safe semantic replay
#' @noRd
cache_replay_content <- function(content) {
  if (inherits(content, "ellmer::ContentText")) {
    return(ellmer::ContentText(cache_replay_value(content@text)))
  }
  if (inherits(content, "ellmer::ContentJson")) {
    return(ellmer_content_json(
      data = cache_replay_value(content@data),
      string = cache_replay_value(content@string)
    ))
  }
  if (inherits(content, "ellmer::ContentThinking")) {
    return(ellmer::ContentThinking(
      cache_replay_value(content@thinking),
      extra = cache_replay_value(content@extra)
    ))
  }
  if (inherits(content, "ellmer::ContentImageInline")) {
    return(ellmer::ContentImageInline(
      type = cache_replay_value(content@type),
      data = cache_replay_value(content@data)
    ))
  }
  if (inherits(content, "ellmer::ContentImageRemote")) {
    return(ellmer::ContentImageRemote(
      url = cache_replay_value(content@url),
      detail = cache_replay_value(content@detail)
    ))
  }
  if (inherits(content, "ellmer::ContentPDF")) {
    if (S7::prop_exists(content, "url")) {
      return(ellmer::ContentPDF(
        type = cache_replay_value(content@type),
        data = cache_replay_value(content@data),
        filename = cache_replay_value(content@filename),
        url = cache_replay_value(content@url)
      ))
    }
    return(ellmer::ContentPDF(
      type = cache_replay_value(content@type),
      data = cache_replay_value(content@data),
      filename = cache_replay_value(content@filename)
    ))
  }
  if (inherits(content, "ellmer::ContentToolRequest")) {
    return(ellmer::ContentToolRequest(
      id = cache_replay_value(content@id),
      name = cache_replay_value(content@name),
      arguments = cache_replay_value(content@arguments),
      tool = NULL,
      extra = cache_replay_value(content@extra)
    ))
  }
  if (inherits(content, "ellmer::ContentToolResult")) {
    request <- if (is.null(content@request)) {
      NULL
    } else {
      cache_replay_content(content@request)
    }
    return(ellmer::ContentToolResult(
      value = cache_replay_value(content@value),
      error = cache_replay_value(content@error),
      extra = cache_replay_value(content@extra),
      request = request
    ))
  }
  if (inherits(content, "ellmer::ContentUploaded")) {
    return(ellmer::ContentUploaded(
      uri = cache_replay_value(content@uri),
      mime_type = cache_replay_value(content@mime_type)
    ))
  }

  cli::cli_abort(
    "Cannot safely replay content {.cls {class(content)[1]}}",
    class = "dsprrr_cache_replay_error"
  )
}

#' Clone a turn while deliberately dropping observed usage and cost metadata
#' @noRd
cache_replay_turn <- function(turn) {
  contents <- lapply(turn@contents, cache_replay_content)
  if (inherits(turn, "ellmer::AssistantTurn")) {
    arguments <- list(
      contents = contents,
      json = cache_replay_value(turn@json),
      tokens = c(NA_real_, NA_real_, NA_real_),
      cost = NA_real_,
      duration = NA_real_
    )
    if (S7::prop_exists(turn, "finish_reason")) {
      arguments$finish_reason <- cache_replay_value(
        S7::prop(turn, "finish_reason")
      )
    }
    return(do.call(ellmer::AssistantTurn, arguments))
  }
  if (inherits(turn, "ellmer::SystemTurn")) {
    return(ellmer::SystemTurn(contents = contents))
  }
  if (inherits(turn, "ellmer::UserTurn")) {
    return(ellmer::UserTurn(contents = contents))
  }

  cli::cli_abort(
    "Cannot safely replay turn {.cls {class(turn)[1]}}",
    class = "dsprrr_cache_replay_error"
  )
}

#' Snapshot Chat turns before a cacheable request
#' @noRd
cache_turn_snapshot <- function(llm) {
  cache_require_trusted_ellmer_chat(
    llm,
    error_class = "dsprrr_cache_replay_error"
  )
  get_turns <- tryCatch(llm$get_turns, error = function(e) NULL)
  set_turns <- tryCatch(llm$set_turns, error = function(e) NULL)
  if (!is.function(get_turns) || !is.function(set_turns)) {
    cli::cli_abort(
      "Chat history cannot be inspected and replayed",
      class = "dsprrr_cache_replay_error"
    )
  }
  turns <- tryCatch(
    get_turns(),
    error = function(e) {
      cli::cli_abort(
        "Failed to read Chat history for cache replay",
        class = "dsprrr_cache_replay_error",
        parent = e
      )
    }
  )
  if (!is.list(turns)) {
    cli::cli_abort(
      "Chat history must be a list of turns",
      class = "dsprrr_cache_replay_error"
    )
  }
  list(mode = "turns", turns = turns)
}

#' Capture the semantic turn delta produced by a request
#' @noRd
cache_turn_delta <- function(llm, before) {
  if (identical(before$mode, "stateless")) {
    return(list(mode = "stateless", turns = list()))
  }
  after <- cache_turn_snapshot(llm)
  before_n <- length(before$turns)
  if (
    !identical(after$mode, "turns") ||
      length(after$turns) < before_n ||
      (before_n > 0 &&
        !identical(after$turns[seq_len(before_n)], before$turns))
  ) {
    cli::cli_abort(
      "Chat history was replaced during the request and cannot be replayed",
      class = "dsprrr_cache_replay_error"
    )
  }
  new_turns <- if (length(after$turns) == before_n) {
    list()
  } else {
    after$turns[seq.int(before_n + 1L, length(after$turns))]
  }
  list(mode = "turns", turns = lapply(new_turns, cache_replay_turn))
}

#' Copy Chat turns so replay attempts cannot corrupt the rollback snapshot
#' @noRd
cache_copy_turn_history <- function(turns) {
  tryCatch(
    rlang::duplicate(turns, shallow = FALSE),
    error = function(e) e
  )
}

#' Verify that a Chat contains exactly the expected turns
#' @noRd
cache_turn_history_matches <- function(get_turns, expected) {
  observed <- tryCatch(get_turns(), error = function(e) e)
  !inherits(observed, "condition") &&
    is.list(observed) &&
    identical(observed, expected)
}

#' Restore and verify the exact history from before a cache replay attempt
#' @noRd
cache_restore_turn_history <- function(get_turns, set_turns, before) {
  restoration <- cache_copy_turn_history(before)
  if (
    inherits(restoration, "condition") ||
      !identical(restoration, before)
  ) {
    return(FALSE)
  }

  tryCatch(set_turns(restoration), error = function(e) NULL)
  cache_turn_history_matches(get_turns, before)
}

#' Replay a cached semantic turn delta without fabricating usage metadata
#' @noRd
cache_replay_turn_delta <- function(llm, delta) {
  if (!cache_is_trusted_ellmer_chat(llm)) {
    return(FALSE)
  }
  if (identical(delta$mode, "stateless")) {
    return(TRUE)
  }
  if (!identical(delta$mode, "turns") || !is.list(delta$turns)) {
    return(FALSE)
  }

  get_turns <- tryCatch(llm$get_turns, error = function(e) NULL)
  set_turns <- tryCatch(llm$set_turns, error = function(e) NULL)
  if (!is.function(get_turns) || !is.function(set_turns)) {
    return(FALSE)
  }
  existing <- tryCatch(get_turns(), error = function(e) e)
  if (inherits(existing, "condition") || !is.list(existing)) {
    return(FALSE)
  }
  before <- cache_copy_turn_history(existing)
  if (inherits(before, "condition") || !identical(before, existing)) {
    return(FALSE)
  }
  replayed <- tryCatch(
    lapply(delta$turns, cache_replay_turn),
    error = function(e) e
  )
  if (inherits(replayed, "condition")) {
    return(FALSE)
  }

  existing_for_update <- cache_copy_turn_history(before)
  if (
    inherits(existing_for_update, "condition") ||
      !identical(existing_for_update, before)
  ) {
    return(FALSE)
  }
  intended <- c(existing_for_update, replayed)
  intended_for_setter <- cache_copy_turn_history(intended)
  if (
    inherits(intended_for_setter, "condition") ||
      !identical(intended_for_setter, intended)
  ) {
    return(FALSE)
  }

  setter_error <- tryCatch(
    {
      set_turns(intended_for_setter)
      NULL
    },
    error = function(e) e
  )
  if (
    is.null(setter_error) &&
      cache_turn_history_matches(get_turns, intended)
  ) {
    return(TRUE)
  }

  if (cache_restore_turn_history(get_turns, set_turns, before)) {
    return(FALSE)
  }

  cli::cli_abort(
    c(
      "Cache replay left the Chat in an unverified state",
      "x" = "The original conversation history could not be restored.",
      "i" = "The provider was not called."
    ),
    class = c(
      "dsprrr_cache_replay_state_error",
      "dsprrr_cache_replay_error"
    )
  )
}

#' Build and validate the versioned cache value envelope
#' @noRd
cache_envelope <- function(result, turn_delta) {
  list(
    version = cache_envelope_schema_version(),
    result = result,
    turn_delta = turn_delta
  )
}

is_cache_envelope <- function(x) {
  is.list(x) &&
    identical(x$version, cache_envelope_schema_version()) &&
    all(c("result", "turn_delta") %in% names(x))
}
