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
#'
#' Recursive query context accepts legacy text strings or an ordered list of
#' typed content records. Records support text, HTTPS image URLs, and bounded
#' inline images. The bridge never accepts local paths or arbitrary R objects.
#'
#' Recursive-query helpers suspend execution with a nonce-bound, schema-checked
#' request. The main RLM process handles the request and replays the code with
#' an immutable response, so the helpers behave like ordinary value-returning R
#' functions.
#' The actual LLM calls happen in the parent R process, not in the sandboxed code
#' execution environment.
#'
#' @name rlm-tools
#' @noRd
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

# Convert a guest content record to plain data. The host performs the full
# schema validation before it calls a provider.
.rlm_encode_query_content <- function(item, field) {
  if (base::inherits(item, "ellmer::ContentText")) {
    item <- base::list(type = "text", text = item@text)
  } else if (base::inherits(item, "ellmer::ContentImageRemote")) {
    item <- base::list(
      type = "image_url",
      url = item@url,
      detail = item@detail
    )
  } else if (base::inherits(item, "ellmer::ContentImageInline")) {
    item <- base::list(
      type = "image_base64",
      mime_type = item@type,
      data = item@data
    )
  } else if (base::inherits(item, "ellmer::Content")) {
    base::stop(
      field,
      " does not support ellmer content class ",
      base::class(item)[[1L]]
    )
  }

  if (!base::is.list(item) || base::is.object(item)) {
    base::stop(field, " must be a typed content record")
  }
  item_names <- base::names(item)
  type <- item$type
  if (
    base::is.null(item_names) || base::anyNA(item_names) ||
      base::any(!base::nzchar(item_names)) || base::anyDuplicated(item_names) ||
      !base::is.character(type) || base::length(type) != 1L || base::is.na(type)
  ) {
    base::stop(field, " must be a typed content record")
  }
  fields <- switch(
    type,
    text = "text",
    image_url = c("url", if ("detail" %%in%% item_names) "detail"),
    image_base64 = c("mime_type", "data"),
    base::stop(field, " has unsupported content type ", type)
  )
  if (!base::setequal(item_names, c("type", fields))) {
    base::stop(field, " has invalid fields for content type ", type)
  }

  if (base::identical(type, "image_url")) {
    url <- item$url
    if (
      !base::is.character(url) || base::length(url) != 1L || base::is.na(url) ||
        !base::grepl(
          "^https://[^/?#[:space:]@]+(?:[/?#].*)?$",
          url,
          ignore.case = TRUE,
          perl = TRUE
        ) ||
        base::nchar(url, type = "bytes") > 2048L
    ) {
      base::stop(field, " image_url requires HTTPS without user info, up to 2048 bytes")
    }
    if (base::is.null(item$detail)) {
      item$detail <- "auto"
    }
  }
  if (base::identical(type, "image_base64")) {
    data <- item$data
    if (
      !base::is.character(data) || base::length(data) != 1L || base::is.na(data) ||
        base::nchar(data, type = "bytes") > 524288L
    ) {
      base::stop(field, " image_base64 data exceeds 524288 bytes")
    }
  }
  item
}

# Text remains the legacy scalar form. Structured contexts are ordered lists
# of JSON-native records and need no ellmer package in the guest process.
.rlm_encode_query_context <- function(value, field) {
  if (base::is.null(value)) {
    return(NULL)
  }
  if (base::is.character(value)) {
    if (base::length(value) != 1L || base::is.na(value)) {
      base::stop(
        field,
        " must be NULL, one string, or an ordered list of content records"
      )
    }
    return(value)
  }
  if (base::inherits(value, "ellmer::Content")) {
    return(base::list(.rlm_encode_query_content(value, field)))
  }
  if (
    !base::is.list(value) || base::is.object(value) ||
      base::length(value) < 1L || base::length(value) > 32L ||
      !base::is.null(base::names(value))
  ) {
    base::stop(field, " must be an ordered list of 1 to 32 content records")
  }
  base::lapply(
    base::seq_along(value),
    function(index) {
      .rlm_encode_query_content(
        value[[index]],
        base::paste0(field, "[[", index, "]]" )
      )
    }
  )
}

# llm_query: Recursive LLM query
llm_query <- base::local({
  .call <- .rlm_control_call
  .encode_context <- .rlm_encode_query_context
  function(query, context_slice = NULL) {
    if (
      !base::is.character(query) ||
        base::length(query) != 1L ||
        base::is.na(query) ||
        !base::nzchar(query)
    ) {
      base::stop("query must be one non-empty character string")
    }
    .call(
      "query",
      base::list(
        query = query,
        context = .encode_context(context_slice, "context_slice"),
        batch = FALSE
      )
    )
  }
})

# llm_query_batched: Batched recursive queries
llm_query_batched <- base::local({
  .call <- .rlm_control_call
  .encode_context <- .rlm_encode_query_context
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
      if (base::is.character(slices)) {
        if (base::anyNA(slices)) {
          base::stop(
            "slices must contain one non-missing character string per query"
          )
        }
        slices <- base::as.list(slices)
      } else if (!base::is.list(slices) || base::is.object(slices)) {
        base::stop("slices must be text or a list of per-query content contexts")
      }
      invalid_text_slices <- base::vapply(
        slices,
        function(slice) {
          base::is.character(slice) &&
            (base::length(slice) != 1L || base::is.na(slice))
        },
        base::logical(1)
      )
      if (base::any(invalid_text_slices)) {
        base::stop(
          "slices must contain one non-missing character string per query"
        )
      }
      slices <- base::lapply(
        base::seq_along(slices),
        function(index) {
          .encode_context(slices[[index]], base::paste0("slices[[", index, "]]"))
        }
      )
      if (base::all(base::vapply(
        slices,
        function(slice) {
          base::is.character(slice) && base::length(slice) == 1L
        },
        base::logical(1)
      ))) {
        slices <- base::vapply(slices, base::identity, base::character(1))
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


abort_rlm_control_frame_size <- function(frame_bytes, max_bytes) {
  cli::cli_abort(
    c(
      "RLM control frame exceeds the runner transport limit",
      "x" = "The frame has {frame_bytes} bytes; the limit is {max_bytes} bytes."
    ),
    class = c(
      "dsprrr_rlm_control_frame_size_error",
      "dsprrr_rlm_control_error"
    ),
    frame_bytes = frame_bytes,
    max_bytes = max_bytes
  )
}


normalize_rlm_query_content_record <- function(record, field) {
  malformed <- function(reason) {
    abort_rlm_control(paste0(
      "Malformed RLM query control frame: content at ",
      field,
      ": ",
      reason
    ))
  }

  if (!is.list(record) || is.object(record)) {
    malformed("expected a plain JSON object")
  }
  record_names <- names(record)
  if (
    is.null(record_names) ||
      anyNA(record_names) ||
      !all(nzchar(record_names)) ||
      anyDuplicated(record_names) ||
      !"type" %in% record_names
  ) {
    malformed("expected named type-specific fields")
  }
  type <- record$type
  if (
    !is.character(type) ||
      length(type) != 1L ||
      is.na(type) ||
      !nzchar(type)
  ) {
    malformed("type must be one non-empty string")
  }

  if (identical(type, "text")) {
    if (!setequal(record_names, c("type", "text"))) {
      malformed("text records require exactly type and text")
    }
    text <- record$text
    if (!is.character(text) || length(text) != 1L || is.na(text)) {
      malformed("text records require one non-missing string")
    }
    return(list(type = "text", text = text))
  }

  if (identical(type, "image_url")) {
    if (
      !setequal(record_names, c("type", "url", "detail")) &&
        !setequal(record_names, c("type", "url"))
    ) {
      malformed("image_url records require type, url, and optional detail")
    }
    url <- record$url
    if (
      !is.character(url) ||
        length(url) != 1L ||
        is.na(url) ||
        !grepl(
          "^https://[^/?#[:space:]@]+(?:[/?#].*)?$",
          url,
          ignore.case = TRUE,
          perl = TRUE
        ) ||
        nchar(url, type = "bytes") > 2048L
    ) {
      malformed(
        "image_url requires HTTPS without user info, up to 2048 bytes"
      )
    }
    detail <- record$detail %||% "auto"
    if (
      !is.character(detail) ||
        length(detail) != 1L ||
        is.na(detail) ||
        !detail %in% c("auto", "low", "high")
    ) {
      malformed("image_url detail must be auto, low, or high")
    }
    return(list(type = "image_url", url = url, detail = detail))
  }

  if (identical(type, "image_base64")) {
    if (!setequal(record_names, c("type", "mime_type", "data"))) {
      malformed(
        "image_base64 records require exactly type, mime_type, and data"
      )
    }
    mime_type <- record$mime_type
    data <- record$data
    if (
      !is.character(mime_type) ||
        length(mime_type) != 1L ||
        is.na(mime_type) ||
        !mime_type %in% c("image/png", "image/jpeg", "image/gif", "image/webp")
    ) {
      malformed("image_base64 mime_type must be PNG, JPEG, GIF, or WebP")
    }
    if (
      !is.character(data) ||
        length(data) != 1L ||
        is.na(data) ||
        !nzchar(data) ||
        nchar(data, type = "bytes") > 524288L ||
        nchar(data, type = "bytes") %% 4L != 0L ||
        !grepl("^[A-Za-z0-9+/]+={0,2}$", data, perl = TRUE)
    ) {
      malformed("image_base64 data must be valid base64 up to 524288 bytes")
    }
    return(list(type = "image_base64", mime_type = mime_type, data = data))
  }

  malformed(
    paste0(
      "unsupported content type '",
      type,
      "'; supported types are text, image_url, and image_base64"
    )
  )
}


normalize_rlm_query_context <- function(value, field) {
  if (is.null(value)) {
    return(NULL)
  }
  if (is.character(value)) {
    if (length(value) != 1L || is.na(value)) {
      abort_rlm_control(
        paste0(
          "Malformed RLM query context at ",
          field,
          ": expected one text string"
        )
      )
    }
    return(value)
  }
  if (
    !is.list(value) ||
      is.object(value) ||
      !is.null(names(value)) ||
      length(value) < 1L ||
      length(value) > 32L
  ) {
    abort_rlm_control(
      paste0(
        "Malformed RLM query context at ",
        field,
        ": expected text or 1 to 32 ordered records"
      )
    )
  }
  lapply(
    seq_along(value),
    function(index) {
      normalize_rlm_query_content_record(
        value[[index]],
        paste0(field, "[[", index, "]]")
      )
    }
  )
}


rlm_query_content_to_ellmer <- function(record) {
  switch(
    record$type,
    text = ellmer::ContentText(record$text),
    image_url = ellmer::ContentImageRemote(
      url = record$url,
      detail = record$detail
    ),
    image_base64 = ellmer::ContentImageInline(
      type = record$mime_type,
      data = record$data
    ),
    abort_rlm_control("Unsupported decoded RLM query content")
  )
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
  .attest = FALSE,
  max_bytes = NULL
) {
  if (
    !is.null(max_bytes) &&
      (!is.numeric(max_bytes) ||
        length(max_bytes) != 1L ||
        is.na(max_bytes) ||
        !is.finite(max_bytes) ||
        max_bytes < 1 ||
        max_bytes != floor(max_bytes))
  ) {
    abort_rlm_control(
      "RLM control frame byte limit must be one positive integer"
    )
  }
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
  if (!is.null(max_bytes)) {
    frame_bytes <- nchar(matches, type = "bytes")
    if (any(frame_bytes > max_bytes)) {
      abort_rlm_control_frame_size(max(frame_bytes), max_bytes)
    }
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
        slices <- lapply(
          seq_along(slices),
          function(index) {
            normalize_rlm_query_context(
              slices[[index]],
              paste0("slices[[", index, "]]")
            )
          }
        )
        all_text_slices <- all(vapply(
          slices,
          function(slice) is.character(slice) && length(slice) == 1L,
          logical(1)
        ))
        envelope$payload$slices <- if (all_text_slices) {
          vapply(slices, identity, character(1))
        } else {
          slices
        }
      }
    } else {
      query <- envelope$payload$query
      context <- envelope$payload$context
      valid_query <- is.character(query) &&
        length(query) == 1L &&
        !is.na(query)
      if (!valid_query) {
        abort_rlm_control("Malformed RLM query control frame")
      }
      envelope$payload["context"] <- list(normalize_rlm_query_context(
        context,
        "context"
      ))
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
