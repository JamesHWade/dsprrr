#' RLM Tools - Prelude Generator
#'
#' @description
#' Generates R code that defines RLM tools in the execution environment.
#' This code is run before user-generated code via RCodeRunner.
#'
#' @details
#' The prelude defines these functions in the execution environment:
#'
#' - `SUBMIT(answer)`: Terminate and return final answer
#' - `peek(var, start, end)`: View a slice of a variable
#' - `search(var, pattern)`: Regex search in variable
#' - `rlm_query(query, context_slice)`: Request a recursive LLM call (returns marker for interception)
#' - `rlm_query_batch(queries, slices)`: Request batched LLM calls (returns marker for interception)
#'
#' The `rlm_query` and `rlm_query_batch` functions return special marker objects
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
#'
#' @return Character string of R code defining RLM tools
#'
#' @keywords internal
#' @noRd
create_rlm_prelude <- function(
  max_llm_calls = 50L,
  has_sub_lm = FALSE,
  custom_tools = list()
) {
  # Base tools (always available)
  base_prelude <- '
# ============================================
# RLM Tools - Injected by dsprrr
# ============================================

# SUBMIT: Terminate and return final answer
# When code calls SUBMIT(answer), it returns a special object
# that the main process detects to stop iteration
SUBMIT <- function(answer) {
  result <- answer
  class(result) <- c("rlm_final", class(result))
  attr(result, "rlm_final") <- TRUE
  result
}

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
# rlm_query: Recursive LLM query
# Returns a request marker - main process will intercept and handle
rlm_query <- function(query, context_slice = NULL) {
  # Note: This function returns a marker that the main process intercepts

  # The actual LLM call happens in the parent R process
  structure(
    list(query = query, context = context_slice, batch = FALSE),
    class = "rlm_query_request"
  )
}

# rlm_query_batch: Batched recursive queries
# Returns a request marker for batch processing
rlm_query_batch <- function(queries, slices = NULL) {
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

# Note: Maximum LLM calls allowed: %d
# Exceeding this limit will result in an error
',
      max_llm_calls
    )
  } else {
    recursive_prelude <- '
# rlm_query: Disabled (no sub_lm provided)
rlm_query <- function(query, context_slice = NULL) {
  stop("Recursive LLM queries are disabled. Provide sub_lm to enable.")
}

rlm_query_batch <- function(queries, slices = NULL) {
  stop("Recursive LLM queries are disabled. Provide sub_lm to enable.")
}
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
