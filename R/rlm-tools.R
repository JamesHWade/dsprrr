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
#' - `llm_query(query, context_slice)`: Request a recursive LLM call (returns marker for interception)
#' - `llm_query_batched(queries, slices)`: Request batched LLM calls (returns marker for interception)
#' - `rlm_query()` / `rlm_query_batch()`: Backward-compatible aliases
#'
#' The recursive-query helper functions return special marker objects
#' that the main RLM process intercepts and handles. The actual LLM calls happen
#' in the parent R process, not in the sandboxed code execution environment.
#'
#' @keywords internal
#' @name rlm-tools
NULL


rlm_host_tool_replay_field <- function() {
  ".dsprrr_rlm_host_tool_replay"
}


#' Create RLM Prelude Code
#'
#' @description
#' Generates R code that defines RLM tools in the execution environment.
#'
#' @param max_llm_calls Maximum allowed recursive LLM calls
#' @param has_sub_lm Logical indicating if recursive queries are enabled
#' @param custom_tools Named list of user-defined R functions
#' @param output_fields Character vector of required output field names for SUBMIT()
#' @param control_nonce Per-invocation nonce used to authenticate control frames
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
  control_nonce = rlm_control_nonce(),
  control_frame_limit = Inf
) {
  if (!is.character(output_fields) || length(output_fields) < 1) {
    output_fields <- "answer"
  }

  output_fields <- trimws(output_fields)
  output_fields <- output_fields[nzchar(output_fields)]
  if (length(output_fields) < 1) {
    output_fields <- "answer"
  }

  quoted_fields <- paste(sprintf("\"%s\"", output_fields), collapse = ", ")

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
      "Authenticated RLM control frame exceeds the runner transport limit"
    )
  }
  frame
}
',
    control_nonce_literal,
    rlm_control_prefix(),
    control_frame_limit_literal
  )

  submit_prelude <- sprintf(
    '
# SUBMIT: Terminate and return final answer
# Supports positional args (SUBMIT(v1, v2)) or named args
# (SUBMIT(field1 = v1, field2 = v2)).
SUBMIT <- base::local({
  .encode <- .rlm_control_encode
  .output_fields <- base::c(%s)

  function(...) {
    args <- base::list(...)
    if (base::length(args) == 0) {
      base::stop("SUBMIT() requires at least one output value")
    }

    arg_names <- base::names(args)
    if (base::is.null(arg_names)) {
      arg_names <- base::rep("", base::length(args))
    }
    has_any_names <- base::any(base::nzchar(arg_names))

    if (!has_any_names) {
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

      missing <- base::setdiff(.output_fields, arg_names)
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

      args <- args[.output_fields]
    }

    .encode("final", args)
  }
})
',
    quoted_fields
  )

  # Base tools (always available)
  base_prelude <- '
# ============================================
# RLM Tools - Injected by dsprrr
# ============================================

# peek: View a slice of a character variable
# Useful for exploring large text contexts
peek <- function(var, start = 1L, end = 1000L) {
  if (!base::is.character(var)) {
    var <- base::as.character(var)
  }

  if (base::length(var) > 1) {
    # For character vectors, show elements in range
    n <- base::length(var)
    start <- base::max(1L, base::as.integer(start))
    end <- base::min(n, base::as.integer(end))
    return(var[start:end])
  }

  # For single strings, show character range
  total_chars <- base::nchar(var)
  start <- base::max(1L, base::as.integer(start))
  end <- base::min(total_chars, base::as.integer(end))

  if (start > total_chars) {
    return("")
  }

  base::substr(var, start, end)
}

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
# llm_query: Recursive LLM query
# Returns a request marker - main process will intercept and handle
llm_query <- base::local({
  .encode <- .rlm_control_encode
  function(query, context_slice = NULL) {
    # The actual LLM call happens in the parent R process.
    .encode(
      "query",
      base::list(query = query, context = context_slice, batch = FALSE)
    )
  }
})

# llm_query_batched: Batched recursive queries
# Returns a request marker for batch processing
llm_query_batched <- base::local({
  .encode <- .rlm_control_encode
  function(queries, slices = NULL) {
    if (!base::is.character(queries) || base::anyNA(queries)) {
      base::stop("queries must be a non-missing character vector")
    }

    if (!base::is.null(slices) &&
        base::length(slices) != base::length(queries)) {
      base::stop("slices must have same length as queries")
    }

    .encode(
      "query",
      base::list(queries = queries, slices = slices, batch = TRUE)
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

  # Custom tools remain in the host process. The guest emits one authenticated
  # request, then the host replays the program with an immutable response. This
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
# Authenticated host-tool replay bridge
.rlm_host_tool_replay <- .context[[%s]]
if (base::is.null(.rlm_host_tool_replay)) {
  .rlm_host_tool_replay <- base::list()
}
if (!base::is.list(.rlm_host_tool_replay)) {
  base::stop("Malformed RLM host-tool replay state")
}
.rlm_host_tool_call <- base::local({
  .replay <- .rlm_host_tool_replay
  .index <- 0L
  .encode <- .rlm_control_encode
  .canonical <- function(value) {
    base::as.character(jsonlite::toJSON(
      value,
      auto_unbox = TRUE,
      null = "null",
      na = "null",
      dataframe = "rows",
      digits = NA
    ))
  }

  function(name, arguments) {
    .index <<- .index + 1L
    request <- base::list(
      index = .index,
      name = name,
      arguments = arguments
    )
    if (.index <= base::length(.replay)) {
      response <- .replay[[.index]]
      valid <- base::is.list(response) &&
        base::is.list(response$request) &&
        base::identical(.canonical(response$request), .canonical(request)) &&
        base::is.logical(response$success) &&
        base::length(response$success) == 1L &&
        !base::is.na(response$success)
      if (!valid) {
        base::stop("RLM host-tool replay diverged from its authenticated request")
      }
      if (base::isTRUE(response$success)) {
        return(response$value)
      }
      base::stop(base::as.character(response$error)[[1L]], call. = FALSE)
    }
    base::stop(.encode("host_tool", request), call. = FALSE)
  }
})

%s
',
      encodeString(rlm_host_tool_replay_field(), quote = "\""),
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
    "\nbase::rm(.rlm_control_encode)\n",
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


decode_rlm_control <- function(x, control_nonce = NULL) {
  if (inherits(x, "rlm_final") || inherits(x, "rlm_query_request")) {
    return(x)
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
      any(!nzchar(matches))
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
    return(structure(
      envelope$payload,
      class = c("rlm_final", class(envelope$payload)),
      rlm_final = TRUE
    ))
  }
  if (identical(envelope$kind, "query")) {
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
      if (!is.null(slices) && length(slices) != length(queries)) {
        abort_rlm_control("Malformed RLM batch query control frame")
      }
    } else if (
      !is.character(envelope$payload$query) ||
        length(envelope$payload$query) != 1L ||
        is.na(envelope$payload$query)
    ) {
      abort_rlm_control("Malformed RLM query control frame")
    }
    return(structure(
      envelope$payload,
      class = c("rlm_query_request", class(envelope$payload))
    ))
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
    return(structure(
      envelope$payload,
      class = c("rlm_host_tool_request", class(envelope$payload))
    ))
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
