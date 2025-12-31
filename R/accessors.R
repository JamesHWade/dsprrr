#' Accessor Functions for DSPrrr Results
#'
#' @description
#' Helper functions to extract data from structured results returned by
#' `run()`, `evaluate()`, and other DSPrrr functions.
#'
#' @name accessors
NULL

#' Get output from a result
#'
#' @description
#' Extract the output from a DSPrrr result object.
#'
#' @param x A DSPrrr result object (e.g., from `run()` with `.return_format = "structured"`)
#' @param ... Additional arguments (unused)
#'
#' @return The output value(s) from the result
#' @export
#' @examples
#' \dontrun{
#' result <- run(mod, text = "hello", .return_format = "structured")
#' get_output(result)
#' }
get_output <- function(x, ...) {
  UseMethod("get_output")
}

#' @export
get_output.default <- function(x, ...) {
  if (is.list(x) && "output" %in% names(x)) {
    x$output
  } else if (is.list(x) && "predictions" %in% names(x)) {
    x$predictions
  } else {
    x
  }
}

#' @export
get_output.dsprrr_batch_result <- function(x, ...) {
  lapply(x, function(item) item$output)
}

#' @export
get_output.dsprrr_evaluation <- function(x, ...) {
  x$predictions
}

#' Get metadata from a result
#'
#' @description
#' Extract metadata from a DSPrrr result object.
#'
#' @param x A DSPrrr result object
#' @param ... Additional arguments (unused)
#'
#' @return A list or tibble of metadata
#' @export
#' @examples
#' \dontrun{
#' result <- run(mod, text = "hello", .return_format = "structured")
#' get_metadata(result)
#' }
get_metadata <- function(x, ...) {
  UseMethod("get_metadata")
}

#' @export
get_metadata.default <- function(x, ...) {
  if (is.list(x) && "metadata" %in% names(x)) {
    x$metadata
  } else {
    list()
  }
}

#' @export
get_metadata.dsprrr_batch_result <- function(x, ...) {
  lapply(x, function(item) item$metadata %||% list())
}

#' @export
get_metadata.dsprrr_evaluation <- function(x, ...) {
  x$metadata
}

#' Get token counts from a result
#'
#' @description
#' Extract token usage information from a DSPrrr result object.
#'
#' @param x A DSPrrr result object
#' @param ... Additional arguments (unused)
#'
#' @return A list or tibble with token counts (input_tokens, output_tokens, total_tokens)
#' @export
#' @examples
#' \dontrun{
#' result <- run(mod, text = "hello", .return_format = "structured")
#' get_tokens(result)
#' }
get_tokens <- function(x, ...) {
  UseMethod("get_tokens")
}

#' @export
get_tokens.default <- function(x, ...) {
  meta <- get_metadata(x)
  if (is.list(meta) && length(meta) > 0) {
    list(
      input_tokens = meta$input_tokens %||% NA_integer_,
      output_tokens = meta$output_tokens %||% NA_integer_,
      total_tokens = meta$total_tokens %||% NA_integer_
    )
  } else {
    list(
      input_tokens = NA_integer_,
      output_tokens = NA_integer_,
      total_tokens = NA_integer_
    )
  }
}

#' @export
get_tokens.dsprrr_batch_result <- function(x, ...) {
  tokens <- lapply(x, function(item) {
    meta <- item$metadata %||% list()
    list(
      input_tokens = meta$input_tokens %||% NA_integer_,
      output_tokens = meta$output_tokens %||% NA_integer_,
      total_tokens = meta$total_tokens %||% NA_integer_
    )
  })

  tibble::tibble(
    index = seq_along(tokens),
    input_tokens = vapply(tokens, function(t) t$input_tokens, integer(1)),
    output_tokens = vapply(tokens, function(t) t$output_tokens, integer(1)),
    total_tokens = vapply(tokens, function(t) t$total_tokens, integer(1))
  )
}

#' @export
get_tokens.dsprrr_evaluation <- function(x, ...) {
  meta <- x$metadata
  if (is.null(meta) || length(meta) == 0) {
    return(tibble::tibble(
      index = integer(0),
      input_tokens = integer(0),
      output_tokens = integer(0),
      total_tokens = integer(0)
    ))
  }

  tibble::tibble(
    index = seq_along(meta),
    input_tokens = vapply(
      meta,
      function(m) m$input_tokens %||% NA_integer_,
      integer(1)
    ),
    output_tokens = vapply(
      meta,
      function(m) m$output_tokens %||% NA_integer_,
      integer(1)
    ),
    total_tokens = vapply(
      meta,
      function(m) m$total_tokens %||% NA_integer_,
      integer(1)
    )
  )
}

#' Get cost from a result
#'
#' @description
#' Extract cost information from a DSPrrr result object.
#'
#' @param x A DSPrrr result object
#' @param ... Additional arguments (unused)
#'
#' @return A numeric value (for single results) or a `dsprrr_cost_summary` object
#'   (for batch results and evaluations) containing:
#'   - `costs`: A tibble with per-item costs
#'   - `total`: Total cost across all items
#'
#'   - `n_missing`: Count of items with missing cost data
#'
#' @seealso [session_cost()] for session-level cost tracking
#' @export
#' @examples
#' \dontrun{
#' result <- run(mod, text = "hello", .return_format = "structured")
#' get_cost(result)
#' }
get_cost <- function(x, ...) {
  UseMethod("get_cost")
}

#' @export
get_cost.default <- function(x, ...) {
  meta <- get_metadata(x)
  if (is.list(meta) && "cost" %in% names(meta)) {
    meta$cost
  } else {
    NA_real_
  }
}

#' @export
get_cost.dsprrr_batch_result <- function(x, ...) {
  costs <- vapply(
    x,
    function(item) {
      meta <- item$metadata %||% list()
      meta$cost %||% NA_real_
    },
    numeric(1)
  )

  n_missing <- sum(is.na(costs))
  if (n_missing > 0) {
    cli::cli_warn(c(
      "Cost data missing for {n_missing} of {length(costs)} items",
      "i" = "Total cost may be underreported"
    ))
  }

  structure(
    list(
      costs = tibble::tibble(
        index = seq_along(costs),
        cost = costs
      ),
      total = sum(costs, na.rm = TRUE),
      n_missing = n_missing
    ),
    class = "dsprrr_cost_summary"
  )
}

#' @export
get_cost.dsprrr_evaluation <- function(x, ...) {
  meta <- x$metadata
  if (is.null(meta) || length(meta) == 0) {
    return(structure(
      list(
        costs = tibble::tibble(
          index = integer(0),
          cost = numeric(0)
        ),
        total = 0,
        n_missing = 0L
      ),
      class = "dsprrr_cost_summary"
    ))
  }

  costs <- vapply(
    meta,
    function(m) m$cost %||% NA_real_,
    numeric(1)
  )

  n_missing <- sum(is.na(costs))
  if (n_missing > 0) {
    cli::cli_warn(c(
      "Cost data missing for {n_missing} of {length(costs)} items",
      "i" = "Total cost may be underreported"
    ))
  }

  structure(
    list(
      costs = tibble::tibble(
        index = seq_along(costs),
        cost = costs
      ),
      total = sum(costs, na.rm = TRUE),
      n_missing = n_missing
    ),
    class = "dsprrr_cost_summary"
  )
}

#' Print method for dsprrr_cost_summary
#' @param x A dsprrr_cost_summary object
#' @param ... Additional arguments (unused)
#' @export
print.dsprrr_cost_summary <- function(x, ...) {
  cli::cli_h3("DSPrrr Cost Summary")

  if (nrow(x$costs) == 0) {
    cli::cli_alert_info("No cost data available")
    return(invisible(x))
  }

  cli::cli_alert_success("Total Cost: ${round(x$total, 4)}")
  cli::cli_text("{.field Items}: {nrow(x$costs)}")

  if (x$n_missing > 0) {
    cli::cli_alert_warning(
      "{.field Missing}: {x$n_missing} items without cost data"
    )
  }

  invisible(x)
}
