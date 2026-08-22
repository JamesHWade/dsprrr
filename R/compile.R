#' Compile a program
#'
#' @description
#' Optimize a dsprrr program with a teleprompter. This is the single
#' user-facing compilation entry point and is ordered for the native pipe:
#' `program |> compile(teleprompter, trainset)`.
#'
#' @param program A dsprrr module or compositional program to optimize.
#' @param teleprompter A Teleprompter defining the optimization strategy.
#' @param ... Additional arguments. The first is normally `trainset`, a data
#'   frame. Optimizers may also accept `valset`, `.llm`, and
#'   `.trace_context`.
#'
#' @return An optimized program.
#' @export
#' @examples
#' \dontrun{
#' classifier <- module(signature("text -> sentiment"))
#' trainset <- data.frame(
#'   text = c("I love it!", "Terrible experience"),
#'   sentiment = c("positive", "negative")
#' )
#'
#' optimized <- classifier |>
#'   compile(LabeledFewShot(k = 2L), trainset)
#' }
compile <- S7::new_generic("compile", c("program", "teleprompter"))

# Methods are registered in zzz.R after all teleprompter classes are loaded.

#' Validate and normalize shared compilation inputs
#' @noRd
validate_compile_inputs <- function(program, teleprompter, trainset, dots) {
  if (!inherits(teleprompter, "dsprrr::Teleprompter")) {
    cli::cli_abort(c(
      "`teleprompter` must be a Teleprompter object",
      "x" = "Got {.cls {class(teleprompter)[1]}}"
    ))
  }
  if (!is.data.frame(trainset)) {
    cli::cli_abort(
      "trainset must be a data frame",
      class = "dsprrr_compile_argument_error"
    )
  }

  dot_names <- names(dots) %||% rep("", length(dots))
  valset_index <- which(dot_names == "valset")
  if (length(valset_index) > 1L) {
    cli::cli_abort("`valset` must be supplied at most once")
  }
  if (
    length(valset_index) == 0L && length(dots) > 0L && dot_names[[1L]] == ""
  ) {
    valset_index <- 1L
  }
  if (length(valset_index) == 1L && !is.null(dots[[valset_index]])) {
    dots[[valset_index]] <- tryCatch(
      as.data.frame(dots[[valset_index]]),
      error = function(error) {
        cli::cli_abort(c(
          "`valset` must be convertible to a data frame",
          "x" = conditionMessage(error)
        ))
      }
    )
  }

  if (inherits(program, "Module") && program$is_compiled()) {
    cli::cli_warn(c(
      "Program appears to be already compiled",
      "i" = "Previous teleprompter: {program$config$teleprompter}",
      "i" = "Recompiling with: {class(teleprompter)[1]}"
    ))
  }

  dots
}

#' Invoke a compiler with correlation-only trace context
#' @noRd
compile_with_trace_context <- function(
  compiler,
  program,
  teleprompter,
  trainset,
  ...,
  .llm = NULL
) {
  assert_ellmer_chat(.llm, arg = ".llm", allow_null = TRUE)
  compiler_expression <- substitute(compiler)
  compiler_name <- if (is.symbol(compiler_expression)) {
    as.character(compiler_expression)
  } else {
    NULL
  }

  dots <- rlang::list2(...)
  dots <- validate_compile_inputs(program, teleprompter, trainset, dots)
  dot_names <- names(dots) %||% rep("", length(dots))
  context_index <- which(dot_names == ".trace_context")
  if (length(context_index) > 1L) {
    cli::cli_abort(
      "{.arg .trace_context} must be supplied at most once",
      class = "dsprrr_trace_context_error"
    )
  }
  context_supplied <- length(context_index) == 1L
  context <- if (context_supplied) {
    dots[[context_index]]
  } else {
    list()
  }
  if (context_supplied) {
    dots <- dots[-context_index]
  }
  context <- trace_context_resolve(context, supplied = context_supplied)
  previous_trace_context <- trace_context_enter(context)
  on.exit(trace_context_restore(previous_trace_context), add = TRUE)

  do.call(
    compiler_name %||% compiler,
    c(list(teleprompter, program, trainset, .llm = .llm), dots),
    envir = environment(compiler) %||% parent.frame()
  )
}
