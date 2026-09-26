#' Extract outputs, metadata and costs from results
#'
#' @description
#' These functions read the parts of a result so you do not need to know its
#' structure. They work on results of [run()] with
#' `.return_format = "structured"`, for single inputs and batches, and on
#' [evaluate()] results.
#'
#' - `get_output()` returns the outputs: the named list of output fields for
#'   a single result, a list of them for a batch, and the predictions of an
#'   evaluation.
#' - `get_metadata()` returns the call metadata: a list for a single result
#'   and one list per row for a batch or an evaluation.
#' - `get_tokens()` returns `input_tokens`, `output_tokens` and
#'   `total_tokens`.
#' - `get_cost()` returns the estimated cost in US dollars.
#'
#' For [run_dataset()] results, use the `result` and `.metadata` columns
#' instead.
#'
#' @param x A result: a `dsprrr_result` or `dsprrr_batch_result` from [run()]
#'   with `.return_format = "structured"`, or a `dsprrr_evaluation` from
#'   [evaluate()]. For other objects, `get_output()` returns `x` itself and
#'   the other functions return empty or missing values.
#' @param ... Not used.
#'
#' @return
#' - `get_output()`: the outputs, as described above.
#' - `get_metadata()`: a list, or a list of lists; an empty list when `x` has
#'   no metadata.
#' - `get_tokens()`: a list of the three counts for a single result, or a
#'   tibble with columns `index`, `input_tokens`, `output_tokens` and
#'   `total_tokens` for a batch or an evaluation. Unknown counts are `NA`.
#' - `get_cost()`: a number (`NA` when unknown) for a single result. For a
#'   batch or an evaluation, a list of class `dsprrr_cost_summary` with
#'   `costs` (a tibble of `index` and `cost`), `total` and `n_missing`;
#'   missing costs give a warning and make `total` `NA` instead of counting
#'   as free.
#'
#' @family inspection
#' @examples
#' shout <- module_fn("text -> reply", function(text) toupper(text))
#' result <- run(shout, text = "hello", .return_format = "structured")
#'
#' get_output(result)
#' get_metadata(result)$latency_ms
#' # Function-backed modules make no model calls, so these are NA
#' get_tokens(result)
#' get_cost(result)
#'
#' \dontrun{
#' qa <- module(signature("question -> answer"))
#' batch <- run(
#'   qa,
#'   question = c("What is 2 + 2?", "What is 3 + 3?"),
#'   .llm = ellmer::chat_openai(model = "gpt-6-luna"),
#'   .return_format = "structured"
#' )
#' get_tokens(batch)
#' get_cost(batch)$total
#' }
#' @name accessors
NULL

#' @rdname accessors
#' @export
get_output <- function(x, ...) {
  UseMethod("get_output")
}

#' @export
get_output.default <- function(x, ...) {
  if (is.list(x) && "output" %in% names(x)) {
    x$output
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

#' @rdname accessors
#' @export
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

#' @rdname accessors
#' @export
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

#' @rdname accessors
#' @export
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
      "i" = "Total cost is unknown; missing prices are not treated as free"
    ))
  }

  structure(
    list(
      costs = tibble::tibble(
        index = seq_along(costs),
        cost = costs
      ),
      total = sum_cost_values(costs),
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
      "i" = "Total cost is unknown; missing prices are not treated as free"
    ))
  }

  total <- if (!is.null(x$total_cost) && length(x$total_cost) == 1L) {
    as.numeric(x$total_cost)
  } else {
    sum_cost_values(costs)
  }

  structure(
    list(
      costs = tibble::tibble(
        index = seq_along(costs),
        cost = costs
      ),
      total = total,
      n_missing = n_missing
    ),
    class = "dsprrr_cost_summary"
  )
}

#' Print method for dsprrr_cost_summary
#' @param x A dsprrr_cost_summary object
#' @param ... Additional arguments (unused)
#' @noRd
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
