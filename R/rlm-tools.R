#' RLM Tools - Prelude Generator
#'
#' @description
#' Generates R code that defines RLM tools in the execution environment.
#' This code is run before user-generated code by the configured code runner.
#'
#' @details
#' The prelude defines these functions in the execution environment:
#'
#' - `SUBMIT(...)`: Terminate and return final output values
#' - `peek(var, start, end)`: View a slice of a variable
#' - `search(var, pattern)`: Regex search in variable
#' - `llm_query(query, context_slice)`: Request a recursive LLM call
#' - `llm_query_batched(queries, slices)`: Request batched LLM calls
#' - `rlm_query()` / `rlm_query_batch()`: Backward-compatible aliases
#'
#' Recursive-query helpers suspend execution with a nonce-bound, schema-checked
#' request. The main RLM process handles the request and replays the code with
#' an immutable response, so the helpers behave like ordinary value-returning R
#' functions.
#' The actual LLM calls happen in the parent R process, not in the sandboxed code
#' execution environment.
#'
#' @keywords internal
#' @name rlm-tools
NULL


rlm_host_tool_replay_field <- function() {
  ".dsprrr_rlm_host_tool_replay"
}


rlm_query_replay_field <- function() {
  ".dsprrr_rlm_query_replay"
}


rlm_control_replay_field <- function() {
  ".dsprrr_rlm_control_replay"
}


#' Create RLM Prelude Code
#'
#' @description
#' Generates R code that defines RLM tools in the execution environment.
#'
#' @param max_llm_calls Maximum allowed recursive LLM calls
#' @param has_sub_lm Logical indicating if recursive queries are enabled
#' @param custom_tools Named list of user-defined R functions or ellmer ToolDef
#'   objects. Bridge arguments use a lossless JSON-compatible domain: unclassed
#'   logical, integer, double, character, NULL, and recursively nested lists;
#'   missing and non-finite values are rejected.
#' @param output_fields Character vector of output field names for SUBMIT()
#' @param required_output_fields Character vector of required output fields
#' @param control_nonce Per-invocation nonce used to correlate control frames
#' @param control_frame_limit Maximum encoded control-frame size in bytes
#'
#' @return Character string of R code defining RLM tools
#'
#' @keywords internal
#' @noRd
create_rlm_prelude <- function(
  max_llm_calls = 50L,
  has_sub_lm = FALSE,
  custom_tools = list(),
  output_fields = "answer",
  required_output_fields = output_fields,
  control_nonce = rlm_control_nonce(),
  control_frame_limit = Inf
) {
  if (!is.character(output_fields)) {
    output_fields <- "answer"
  }

  output_fields <- trimws(output_fields)
  output_fields <- output_fields[nzchar(output_fields)]
  if (
    !is.character(required_output_fields) ||
      anyNA(required_output_fields) ||
      !all(nzchar(required_output_fields)) ||
      anyDuplicated(required_output_fields) ||
      !all(required_output_fields %in% output_fields)
  ) {
    cli::cli_abort(
      "Internal required RLM output fields must be a unique subset of output fields",
      class = "dsprrr_rlm_control_error"
    )
  }

  quoted_fields <- paste(sprintf("\"%s\"", output_fields), collapse = ", ")
  quoted_required_fields <- paste(
    sprintf("\"%s\"", required_output_fields),
    collapse = ", "
  )

  if (
    !is.character(control_nonce) ||
      length(control_nonce) != 1L ||
      is.na(control_nonce) ||
      !nzchar(control_nonce)
  ) {
    cli::cli_abort(
      "Internal RLM control nonce must be one non-empty string",
      class = "dsprrr_rlm_control_error"
    )
  }
  if (
    !is.numeric(control_frame_limit) ||
      length(control_frame_limit) != 1L ||
      is.na(control_frame_limit) ||
      control_frame_limit <= 0
  ) {
    cli::cli_abort(
      "Internal RLM control-frame limit must be one positive number",
      class = "dsprrr_rlm_control_error"
    )
  }

  control_nonce_literal <- encodeString(control_nonce, quote = "\"")
  control_frame_limit_literal <- if (is.infinite(control_frame_limit)) {
    "Inf"
  } else {
    format(control_frame_limit, scientific = FALSE, trim = TRUE)
  }

  control_prelude <- sprintf(
    '
# Versioned text envelope survives both in-process and MCP runner boundaries.
.rlm_control_encode <- function(kind, payload) {
  envelope <- base::list(
    version = 1L,
    nonce = %s,
    kind = kind,
    payload = payload
  )
  json <- jsonlite::toJSON(
    envelope,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    dataframe = "rows",
    digits = NA
  )
  frame <- base::paste0(
    "%s",
    base::gsub(
      "[[:space:]]",
      "",
      jsonlite::base64_enc(base::charToRaw(base::as.character(json)))
    )
  )
  if (base::nchar(frame, type = "bytes") > %s) {
    base::stop(
      "RLM control frame exceeds the runner transport limit"
    )
  }
  frame
}

# One ordered ledger covers recursive queries and host tools. A shared sequence
# prevents a replay from changing the next privileged operation kind.
.rlm_control_replay <- .context[[%s]]
if (base::is.null(.rlm_control_replay)) {
  .rlm_control_replay <- base::list()
}
if (!base::is.list(.rlm_control_replay)) {
  base::stop("Malformed RLM control replay state")
}
.rlm_control_call <- base::local({
  .replay <- .rlm_control_replay
  .index <- 0L
  .encode <- .rlm_control_encode
  .validate <- function(value, path = "request") {
    if (base::is.null(value)) {
      return(base::invisible(TRUE))
    }
    if (base::inherits(value, "AsIs")) {
      value <- base::unclass(value)
    }
    if (base::is.object(value)) {
      base::stop(
        path,
        " must use unclassed JSON-compatible values",
        call. = FALSE
      )
    }
    if (base::is.list(value)) {
      value_names <- base::names(value)
      if (
        !base::is.null(value_names) &&
          (
            base::anyNA(value_names) ||
              base::any(!base::nzchar(value_names)) ||
              base::anyDuplicated(value_names)
          )
      ) {
        base::stop(
          path,
          " must have unique non-empty names or no names",
          call. = FALSE
        )
      }
      for (i in base::seq_along(value)) {
        child <- if (base::is.null(value_names)) {
          base::paste0(path, "[[", i, "]]" )
        } else {
          base::paste0(path, "$", value_names[[i]])
        }
        .validate(value[[i]], child)
      }
      return(base::invisible(TRUE))
    }
    supported <- base::is.atomic(value) &&
      base::typeof(value) %%in%% base::c(
        "logical",
        "integer",
        "double",
        "character"
      ) &&
      base::is.null(base::attributes(value))
    if (!supported) {
      base::stop(
        path,
        " must use unclassed JSON-compatible values",
        call. = FALSE
      )
    }
    if (base::anyNA(value)) {
      base::stop(path, " cannot contain missing values", call. = FALSE)
    }
    if (base::is.numeric(value) && base::any(!base::is.finite(value))) {
      base::stop(path, " cannot contain NaN or infinite values", call. = FALSE)
    }
    base::invisible(TRUE)
  }
  .canonicalize <- function(value) {
    if (base::is.object(value)) {
      value <- base::unclass(value)
    }
    if (base::is.list(value)) {
      value <- base::lapply(value, .canonicalize)
      value_names <- base::names(value)
      if (!base::is.null(value_names) && base::length(value_names) > 0L) {
        value <- value[base::order(value_names)]
      }
    }
    value
  }
  .canonical <- function(value) {
    base::as.character(jsonlite::toJSON(
      .canonicalize(value),
      auto_unbox = TRUE,
      null = "null",
      na = "null",
      dataframe = "rows",
      digits = NA
    ))
  }
  .suspend <- function(frame) {
    if (!base::is.null(base::findRestart(".dsprrr_rlm_control"))) {
      base::invokeRestart(".dsprrr_rlm_control", frame)
    }
    base::stop(frame, call. = FALSE)
  }

  function(kind, request) {
    .index <<- .index + 1L
    request <- base::c(base::list(index = .index), request)
    .validate(request)
    if (.index <= base::length(.replay)) {
      response <- .replay[[.index]]
      valid <- base::is.list(response) &&
        base::identical(response$kind, kind) &&
        base::is.list(response$request) &&
        base::is.logical(response$success) &&
        base::length(response$success) == 1L &&
        !base::is.na(response$success)
      if (!valid) {
        base::stop("Malformed RLM control replay state", call. = FALSE)
      }
      if (!base::identical(.canonical(response$request), .canonical(request))) {
        base::stop(
          "RLM control replay diverged from its recorded request",
          call. = FALSE
        )
      }
      if (base::isTRUE(response$success)) {
        if (!"value" %%in%% base::names(response)) {
          base::stop("Malformed RLM control replay state", call. = FALSE)
        }
        return(response$value)
      }
      if (
        !base::is.character(response$error) ||
          base::length(response$error) != 1L ||
          base::is.na(response$error) ||
          !base::nzchar(response$error)
      ) {
        base::stop("Malformed RLM control replay state", call. = FALSE)
      }
      base::stop(response$error, call. = FALSE)
    }
    .suspend(.encode(kind, request))
  }
})
',
    control_nonce_literal,
    rlm_control_prefix(),
    control_frame_limit_literal,
    encodeString(rlm_control_replay_field(), quote = "\"")
  )

  submit_prelude <- sprintf(
    '
# SUBMIT: Terminate and return final answer
# Supports positional args (SUBMIT(v1, v2)) or named args
# (SUBMIT(field1 = v1, field2 = v2)).
SUBMIT <- base::local({
  .call <- .rlm_control_call
  .output_fields <- base::c(%s)
  .required_output_fields <- base::c(%s)

  function(...) {
    args <- base::list(...)
    if (
      base::length(args) == 0 &&
        base::length(.required_output_fields) > 0
    ) {
      base::stop("SUBMIT() requires at least one output value")
    }

    arg_names <- base::names(args)
    if (base::is.null(arg_names)) {
      arg_names <- base::rep("", base::length(args))
    }
    has_any_names <- base::any(base::nzchar(arg_names))

    if (base::length(args) == 0) {
      args <- base::list()
    } else if (!has_any_names) {
      if (base::length(args) != base::length(.output_fields)) {
        base::stop(
          "SUBMIT() expected ",
          base::length(.output_fields),
          " output(s): ",
          base::paste(.output_fields, collapse = ", ")
        )
      }
      args <- stats::setNames(args, .output_fields)
    } else {
      if (base::any(!base::nzchar(arg_names))) {
        base::stop("SUBMIT() cannot mix named and unnamed outputs")
      }
      if (base::anyDuplicated(arg_names)) {
        base::stop("SUBMIT() output names must be unique")
      }

      missing <- base::setdiff(.required_output_fields, arg_names)
      extra <- base::setdiff(arg_names, .output_fields)

      if (base::length(missing) > 0) {
        base::stop(
          "SUBMIT() missing outputs: ",
          base::paste(missing, collapse = ", ")
        )
      }
      if (base::length(extra) > 0) {
        base::stop(
          "SUBMIT() unknown outputs: ",
          base::paste(extra, collapse = ", ")
        )
      }

      args <- args[base::intersect(.output_fields, arg_names)]
    }

    .call("final", base::list(output = args))
  }
})
',
    quoted_fields,
    quoted_required_fields
  )

  # Base tools (always available)
  base_prelude <- '
# ============================================
# RLM Tools - Injected by dsprrr
# ============================================

# peek: View a slice of a character variable
# Useful for exploring large text contexts
peek <- base::local({
  .index <- function(value, name) {
    valid <- base::is.numeric(value) &&
      base::length(value) == 1L &&
      !base::is.na(value) &&
      base::is.finite(value) &&
      value == base::floor(value)
    if (!valid) {
      base::stop(name, " must be one finite whole number", call. = FALSE)
    }
    value
  }

  function(var, start = 1L, end = 1000L) {
    if (!base::is.character(var)) {
      var <- base::as.character(var)
    }
    start <- .index(start, "start")
    end <- .index(end, "end")

    if (base::length(var) == 0L) {
      return(base::character())
    }

    if (base::length(var) > 1L) {
      # For character vectors, show elements in range.
      n <- base::length(var)
      start <- base::max(1, start)
      end <- base::min(n, end)
      if (start > n || end < start) {
        return(base::character())
      }
      return(var[base::seq.int(start, end)])
    }

    # For single strings, show character range.
    if (base::is.na(var)) {
      return(NA_character_)
    }
    total_chars <- base::nchar(var)
    start <- base::max(1, start)
    end <- base::min(total_chars, end)
    if (start > total_chars || end < start) {
      return("")
    }

    base::substr(var, start, end)
  }
})

# search: Regex search in a variable
# Returns all matches as a character vector
search <- function(var, pattern, ignore_case = FALSE) {
  if (!base::is.character(var)) {
    var <- base::as.character(var)
  }

  # Collapse to single string if vector
  if (base::length(var) > 1) {
    var <- base::paste(var, collapse = "\\n")
  }

  # Find all matches
  matches <- base::regmatches(
    var,
    base::gregexpr(pattern, var, ignore.case = ignore_case, perl = TRUE)
  )

  base::unlist(matches)
}
'

  # Recursive query tools (only if sub_lm is available)
  if (has_sub_lm) {
    recursive_prelude <- sprintf(
      '
# Non-local recursive-query replay bridge

# llm_query: Recursive LLM query
llm_query <- base::local({
  .call <- .rlm_control_call
  function(query, context_slice = NULL) {
    if (
      !base::is.character(query) ||
        base::length(query) != 1L ||
        base::is.na(query) ||
        !base::nzchar(query)
    ) {
      base::stop("query must be one non-empty character string")
    }
    if (
      !base::is.null(context_slice) &&
        (
          !base::is.character(context_slice) ||
            base::length(context_slice) != 1L ||
            base::is.na(context_slice)
        )
    ) {
      base::stop(
        "context_slice must be NULL or one non-missing character string"
      )
    }
    .call(
      "query",
      base::list(query = query, context = context_slice, batch = FALSE)
    )
  }
})

# llm_query_batched: Batched recursive queries
llm_query_batched <- base::local({
  .call <- .rlm_control_call
  function(queries, slices = NULL) {
    if (
      !base::is.character(queries) ||
        base::anyNA(queries) ||
        base::any(!base::nzchar(queries))
    ) {
      base::stop("queries must be a non-empty character vector")
    }

    if (!base::is.null(slices)) {
      if (base::length(slices) != base::length(queries)) {
        base::stop("slices must have same length as queries")
      }
      valid_slices <- base::vapply(
        slices,
        function(slice) {
          base::is.character(slice) &&
            base::length(slice) == 1L &&
            !base::is.na(slice)
        },
        base::logical(1)
      )
      if (!base::all(valid_slices)) {
        base::stop(
          "slices must contain one non-missing character string per query"
        )
      }
    }

    .call(
      "query",
      base::list(
        queries = base::I(queries),
        slices = if (base::is.null(slices)) NULL else base::I(slices),
        batch = TRUE
      )
    )
  }
})

# Backward-compatible aliases
rlm_query <- llm_query
rlm_query_batch <- llm_query_batched

# Note: Maximum LLM calls allowed: %d
# Exceeding this limit will result in an error
',
      max_llm_calls
    )
  } else {
    recursive_prelude <- '
# llm_query: Disabled (no sub_lm provided)
llm_query <- function(query, context_slice = NULL) {
  base::stop("Recursive LLM queries are disabled. Provide sub_lm to enable.")
}

llm_query_batched <- function(queries, slices = NULL) {
  base::stop("Recursive LLM queries are disabled. Provide sub_lm to enable.")
}

# Backward-compatible aliases
rlm_query <- llm_query
rlm_query_batch <- llm_query_batched
'
  }

  # Custom tools remain in the host process. The guest emits one nonce-bound,
  # schema-checked request, then the host replays the program with an immutable response. This
  # preserves closure identity and works across both local and remote runners
  # without serializing or deparsing privileged functions into generated code.
  custom_prelude <- ""
  if (length(custom_tools) > 0) {
    tool_wrappers <- vapply(
      names(custom_tools),
      function(name) {
        encoded_name <- encodeString(name, quote = "\"")
        sprintf(
          paste0(
            "%1$s <- base::local({\n",
            "  .name <- %2$s\n",
            "  function(...) .rlm_host_tool_call(.name, base::list(...))\n",
            "})\n"
          ),
          name,
          encoded_name
        )
      },
      character(1)
    )

    custom_prelude <- sprintf(
      '
# Non-local host-tool replay bridge
.rlm_host_tool_call <- base::local({
  .call <- .rlm_control_call
  function(name, arguments) {
    .call(
      "host_tool",
      base::list(name = name, arguments = arguments)
    )
  }
})

%s
',
      paste(tool_wrappers, collapse = "\n")
    )
  }

  # Combine all parts
  paste0(
    control_prelude,
    "\n",
    submit_prelude,
    "\n",
    base_prelude,
    recursive_prelude,
    custom_prelude,
    "\nbase::rm(.rlm_control_encode, .rlm_control_call, .rlm_control_replay)\n",
    "\n# ============================================\n"
  )
}


rlm_control_prefix <- function() {
  "__DSPR_RLM_CONTROL_V1__:"
}


rlm_control_nonce <- function() {
  entropy <- tryCatch(
    openssl::rand_bytes(32L),
    error = function(error) {
      cli::cli_abort(
        "Could not create a secure RLM control nonce",
        class = "dsprrr_rlm_nonce_error",
        parent = error
      )
    }
  )
  digest::digest(entropy, algo = "sha256", serialize = FALSE)
}


abort_rlm_control <- function(message) {
  cli::cli_abort(message, class = "dsprrr_rlm_control_error")
}


rlm_control_attestation_attribute <- function() {
  ".dsprrr_rlm_control_attestation"
}


rlm_control_attestation_token <- local({
  token <- new.env(parent = emptyenv())
  function() token
})


attest_rlm_control <- function(x) {
  attr(
    x,
    rlm_control_attestation_attribute()
  ) <- rlm_control_attestation_token()
  x
}


consume_attested_rlm_control <- function(x) {
  attestation <- attr(
    x,
    rlm_control_attestation_attribute(),
    exact = TRUE
  )
  if (!identical(attestation, rlm_control_attestation_token())) {
    return(NULL)
  }
  attr(x, rlm_control_attestation_attribute()) <- NULL
  x
}


decode_rlm_control <- function(
  x,
  control_nonce = NULL,
  .attest = FALSE
) {
  if (
    inherits(x, "rlm_final") ||
      inherits(x, "rlm_query_request") ||
      inherits(x, "rlm_host_tool_request")
  ) {
    return(consume_attested_rlm_control(x))
  }
  if (
    !is.character(x) ||
      length(x) == 0L ||
      all(is.na(x)) ||
      !is.character(control_nonce) ||
      length(control_nonce) != 1L ||
      is.na(control_nonce) ||
      !nzchar(control_nonce)
  ) {
    return(NULL)
  }

  text <- paste(x[!is.na(x)], collapse = "\n")
  prefix <- rlm_control_prefix()
  prefix_locations <- gregexpr(prefix, text, fixed = TRUE)[[1L]]
  prefix_count <- if (identical(prefix_locations[[1L]], -1L)) {
    0L
  } else {
    length(prefix_locations)
  }
  if (prefix_count == 0L) {
    return(NULL)
  }

  pattern <- paste0(prefix, "[A-Za-z0-9+/=]+")
  matches <- regmatches(text, gregexpr(pattern, text, perl = TRUE))[[1L]]
  if (
    length(matches) != prefix_count ||
      !all(nzchar(matches))
  ) {
    abort_rlm_control("Malformed RLM control frame")
  }
  envelopes <- lapply(matches, function(match) {
    token <- sub(prefix, "", match, fixed = TRUE)
    envelope <- tryCatch(
      {
        json <- rawToChar(jsonlite::base64_dec(token))
        jsonlite::fromJSON(json, simplifyVector = FALSE)
      },
      error = function(e) NULL
    )
    valid_version <- is.list(envelope) &&
      is.numeric(envelope$version) &&
      length(envelope$version) == 1L &&
      !is.na(envelope$version) &&
      envelope$version == 1
    if (
      !is.list(envelope) ||
        !valid_version ||
        !is.character(envelope$nonce) ||
        length(envelope$nonce) != 1L ||
        is.na(envelope$nonce) ||
        !is.character(envelope$kind) ||
        length(envelope$kind) != 1L ||
        is.na(envelope$kind) ||
        !is.list(envelope$payload)
    ) {
      abort_rlm_control("Malformed RLM control frame")
    }
    envelope
  })

  # A syntactically valid frame from model-visible data or an earlier
  # invocation is ordinary output, never a command for this invocation.
  current <- vapply(
    envelopes,
    function(envelope) identical(envelope$nonce, control_nonce),
    logical(1)
  )
  if (!any(current)) {
    return(NULL)
  }
  if (sum(current) != 1L) {
    abort_rlm_control("Multiple RLM control frames were returned")
  }
  envelope <- envelopes[[which(current)]]

  if (identical(envelope$kind, "final")) {
    index <- envelope$payload$index
    output <- envelope$payload$output
    valid_index <- is.numeric(index) &&
      length(index) == 1L &&
      !is.na(index) &&
      is.finite(index) &&
      index == floor(index) &&
      index >= 1L &&
      index <= .Machine$integer.max
    if (
      !valid_index ||
        !is.list(output) ||
        !identical(names(envelope$payload), c("index", "output"))
    ) {
      abort_rlm_control("Malformed RLM final control frame")
    }
    control <- structure(
      output,
      class = c("rlm_final", class(output)),
      rlm_final = TRUE,
      rlm_control_index = as.integer(index)
    )
    return(if (isTRUE(.attest)) attest_rlm_control(control) else control)
  }
  if (identical(envelope$kind, "query")) {
    index <- envelope$payload$index
    valid_index <- is.numeric(index) &&
      length(index) == 1L &&
      !is.na(index) &&
      is.finite(index) &&
      index == floor(index) &&
      index >= 1L &&
      index <= .Machine$integer.max
    if (!valid_index) {
      abort_rlm_control("Malformed RLM query control frame")
    }
    envelope$payload$index <- as.integer(index)
    batch <- envelope$payload$batch
    if (!is.logical(batch) || length(batch) != 1L || is.na(batch)) {
      abort_rlm_control("Malformed RLM query control frame")
    }
    if (isTRUE(batch)) {
      raw_queries <- envelope$payload$queries
      if (
        !"queries" %in% names(envelope$payload) ||
          !is.list(raw_queries)
      ) {
        abort_rlm_control("Malformed RLM batch query control frame")
      }
      queries <- vapply(
        raw_queries,
        function(query) {
          if (
            !is.character(query) ||
              length(query) != 1L ||
              is.na(query)
          ) {
            abort_rlm_control("Malformed RLM batch query control frame")
          }
          query
        },
        character(1)
      )
      envelope$payload$queries <- queries
      slices <- envelope$payload$slices
      if (!is.null(slices)) {
        if (!is.list(slices) || length(slices) != length(queries)) {
          abort_rlm_control("Malformed RLM batch query control frame")
        }
        slices <- vapply(
          slices,
          function(slice) {
            if (
              !is.character(slice) ||
                length(slice) != 1L ||
                is.na(slice)
            ) {
              abort_rlm_control("Malformed RLM batch query control frame")
            }
            slice
          },
          character(1)
        )
        envelope$payload$slices <- slices
      }
    } else {
      query <- envelope$payload$query
      context <- envelope$payload$context
      valid_query <- is.character(query) &&
        length(query) == 1L &&
        !is.na(query)
      valid_context <- is.null(context) ||
        (is.character(context) &&
          length(context) == 1L &&
          !is.na(context))
      if (!valid_query || !valid_context) {
        abort_rlm_control("Malformed RLM query control frame")
      }
    }
    control <- structure(
      envelope$payload,
      class = c("rlm_query_request", class(envelope$payload))
    )
    return(if (isTRUE(.attest)) attest_rlm_control(control) else control)
  }
  if (identical(envelope$kind, "host_tool")) {
    index <- envelope$payload$index
    name <- envelope$payload$name
    arguments <- envelope$payload$arguments
    valid_index <- is.numeric(index) &&
      length(index) == 1L &&
      !is.na(index) &&
      is.finite(index) &&
      index == floor(index) &&
      index >= 1L
    valid_name <- is.character(name) &&
      length(name) == 1L &&
      !is.na(name) &&
      nzchar(name)
    if (!valid_index || !valid_name || !is.list(arguments)) {
      abort_rlm_control("Malformed RLM host-tool control frame")
    }
    envelope$payload$index <- as.integer(index)
    control <- structure(
      envelope$payload,
      class = c("rlm_host_tool_request", class(envelope$payload))
    )
    return(if (isTRUE(.attest)) attest_rlm_control(control) else control)
  }
  abort_rlm_control("Unknown RLM control frame kind")
}


normalize_rlm_control_value <- function(
  result,
  stdout = NULL,
  control_nonce = NULL
) {
  decode_rlm_control(result, control_nonce) %||%
    decode_rlm_control(stdout, control_nonce) %||%
    result
}


#' Check if a value is an RLM final answer
#'
#' @param x Value to check
#' @return Logical indicating if x is an rlm_final value
#'
#' @keywords internal
#' @noRd
is_rlm_final <- function(x, control_nonce = NULL) {
  x <- decode_rlm_control(x, control_nonce) %||% x
  inherits(x, "rlm_final") || isTRUE(attr(x, "rlm_final"))
}


#' Check if a value is an RLM query request
#'
#' @param x Value to check
#' @return Logical indicating if x is an rlm_query_request
#'
#' @keywords internal
#' @noRd
is_rlm_query_request <- function(x, control_nonce = NULL) {
  x <- decode_rlm_control(x, control_nonce) %||% x
  inherits(x, "rlm_query_request")
}


is_rlm_host_tool_request <- function(x, control_nonce = NULL) {
  x <- decode_rlm_control(x, control_nonce) %||% x
  inherits(x, "rlm_host_tool_request")
}


#' Extract value from RLM final answer
#'
#' @param x An rlm_final value
#' @return The underlying value with rlm_final class removed
#'
#' @keywords internal
#' @noRd
extract_rlm_final <- function(x, control_nonce = NULL) {
  x <- decode_rlm_control(x, control_nonce) %||% x
  if (!is_rlm_final(x)) {
    return(x)
  }

  class(x) <- setdiff(class(x), "rlm_final")
  attr(x, "rlm_final") <- NULL
  attr(x, "rlm_control_index") <- NULL
  x
}


#' Strip markdown code fences from RLM-generated code
#'
#' @param code Character string potentially wrapped in markdown fences
#' @return Character string without surrounding code fences
#'
#' @keywords internal
#' @noRd
strip_rlm_code_fences <- function(code) {
  if (!is.character(code) || length(code) != 1) {
    return(code)
  }

  trimmed <- trimws(code)
  match <- regexec(
    "^```(?:r|R)?\\s*\\n([\\s\\S]*)\\n```\\s*$",
    trimmed,
    perl = TRUE
  )
  groups <- regmatches(trimmed, match)[[1]]

  if (length(groups) >= 2) {
    return(groups[2])
  }

  trimmed
}
