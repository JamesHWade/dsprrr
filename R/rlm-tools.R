#' RLM Tools - Prelude Generator
#'
#' @description
#' Generates R code that defines RLM tools in the execution environment.
#' This code is run before user-generated code via RCodeRunner.
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


#' Create RLM Prelude Code
#'
#' @description
#' Generates R code that defines RLM tools in the execution environment.
#'
#' @param max_llm_calls Maximum allowed recursive LLM calls
#' @param has_sub_lm Logical indicating if recursive queries are enabled
#' @param custom_tools Named list of user-defined R functions
#' @param output_fields Character vector of required output field names for SUBMIT()
#'
#' @return Character string of R code defining RLM tools
#'
#' @keywords internal
#' @noRd
create_rlm_prelude <- function(
  max_llm_calls = 50L,
  has_sub_lm = FALSE,
  custom_tools = list(),
  output_fields = "answer"
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

  submit_prelude <- sprintf(
    '
# Required output fields for this signature
.rlm_output_fields <- c(%s)

# SUBMIT: Terminate and return final answer
# Supports positional args (SUBMIT(v1, v2)) or named args
# (SUBMIT(field1 = v1, field2 = v2)).
SUBMIT <- function(...) {
  args <- list(...)
  if (length(args) == 0) {
    stop("SUBMIT() requires at least one output value")
  }

  arg_names <- names(args)
  if (is.null(arg_names)) {
    arg_names <- rep("", length(args))
  }
  has_any_names <- any(nzchar(arg_names))

  if (!has_any_names) {
    if (length(args) != length(.rlm_output_fields)) {
      stop(
        "SUBMIT() expected ",
        length(.rlm_output_fields),
        " output(s): ",
        paste(.rlm_output_fields, collapse = ", ")
      )
    }
    names(args) <- .rlm_output_fields
  } else {
    if (any(!nzchar(arg_names))) {
      stop("SUBMIT() cannot mix named and unnamed outputs")
    }

    missing <- setdiff(.rlm_output_fields, arg_names)
    extra <- setdiff(arg_names, .rlm_output_fields)

    if (length(missing) > 0) {
      stop("SUBMIT() missing outputs: ", paste(missing, collapse = ", "))
    }
    if (length(extra) > 0) {
      stop("SUBMIT() unknown outputs: ", paste(extra, collapse = ", "))
    }

    args <- args[.rlm_output_fields]
  }

  class(args) <- c("rlm_final", class(args))
  attr(args, "rlm_final") <- TRUE
  args
}
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
  if (!is.character(var)) {
    var <- as.character(var)
  }

  if (length(var) > 1) {
    # For character vectors, show elements in range
    n <- length(var)
    start <- max(1L, as.integer(start))
    end <- min(n, as.integer(end))
    return(var[start:end])
  }

  # For single strings, show character range
  total_chars <- nchar(var)
  start <- max(1L, as.integer(start))
  end <- min(total_chars, as.integer(end))

  if (start > total_chars) {
    return("")
  }

  substr(var, start, end)
}

# search: Regex search in a variable
# Returns all matches as a character vector
search <- function(var, pattern, ignore_case = FALSE) {
  if (!is.character(var)) {
    var <- as.character(var)
  }

  # Collapse to single string if vector
  if (length(var) > 1) {
    var <- paste(var, collapse = "\\n")
  }

  # Find all matches
  matches <- regmatches(
    var,
    gregexpr(pattern, var, ignore.case = ignore_case, perl = TRUE)
  )

  unlist(matches)
}
'

  # Recursive query tools (only if sub_lm is available)
  if (has_sub_lm) {
    recursive_prelude <- sprintf(
      '
# llm_query: Recursive LLM query
# Returns a request marker - main process will intercept and handle
llm_query <- function(query, context_slice = NULL) {
  # Note: This function returns a marker that the main process intercepts

  # The actual LLM call happens in the parent R process
  structure(
    list(query = query, context = context_slice, batch = FALSE),
    class = "rlm_query_request"
  )
}

# llm_query_batched: Batched recursive queries
# Returns a request marker for batch processing
llm_query_batched <- function(queries, slices = NULL) {
  if (!is.character(queries)) {
    stop("queries must be a character vector")
  }

  if (!is.null(slices) && length(slices) != length(queries)) {
    stop("slices must have same length as queries")
  }

  structure(
    list(queries = queries, slices = slices, batch = TRUE),
    class = "rlm_query_request"
  )
}

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
  stop("Recursive LLM queries are disabled. Provide sub_lm to enable.")
}

llm_query_batched <- function(queries, slices = NULL) {
  stop("Recursive LLM queries are disabled. Provide sub_lm to enable.")
}

# Backward-compatible aliases
rlm_query <- llm_query
rlm_query_batch <- llm_query_batched
'
  }

  # Custom tools
  custom_prelude <- ""
  if (length(custom_tools) > 0) {
    # Serialize each custom function
    tool_defs <- vapply(
      names(custom_tools),
      function(name) {
        fn <- custom_tools[[name]]
        if (!is.function(fn)) {
          # This should not happen if rlm_module() validation is working
          # but provide a clear error in the prelude as defense in depth
          return(sprintf(
            "%s <- function(...) stop('Tool %s is not a function')\n",
            name,
            name
          ))
        }

        # Deparse the function and assign it
        fn_body <- paste(deparse(fn), collapse = "\n")
        sprintf("%s <- %s\n", name, fn_body)
      },
      character(1)
    )

    custom_prelude <- paste0(
      "\n# Custom Tools\n",
      paste(tool_defs, collapse = "\n")
    )
  }

  # Combine all parts
  paste0(
    submit_prelude,
    "\n",
    base_prelude,
    recursive_prelude,
    custom_prelude,
    "\n# ============================================\n"
  )
}


#' Check if a value is an RLM final answer
#'
#' @param x Value to check
#' @return Logical indicating if x is an rlm_final value
#'
#' @keywords internal
#' @noRd
is_rlm_final <- function(x) {
  inherits(x, "rlm_final") || isTRUE(attr(x, "rlm_final"))
}


#' Check if a value is an RLM query request
#'
#' @param x Value to check
#' @return Logical indicating if x is an rlm_query_request
#'
#' @keywords internal
#' @noRd
is_rlm_query_request <- function(x) {
  inherits(x, "rlm_query_request")
}


#' Extract value from RLM final answer
#'
#' @param x An rlm_final value
#' @return The underlying value with rlm_final class removed
#'
#' @keywords internal
#' @noRd
extract_rlm_final <- function(x) {
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
  match <- regexec("^```(?:r|R)?\\s*\\n([\\s\\S]*)\\n```\\s*$", trimmed, perl = TRUE)
  groups <- regmatches(trimmed, match)[[1]]

  if (length(groups) >= 2) {
    return(groups[2])
  }

  trimmed
}
