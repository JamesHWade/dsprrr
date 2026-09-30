#' Optimize a program with a teleprompter
#'
#' @description
#' `compile()` improves a program (its demos, instructions or other settings)
#' with a teleprompter such as [LabeledFewShot()], [BootstrapFewShot()] or
#' [MIPROv2()], using a training set. It returns a new program and leaves the
#' input unchanged. Its argument order suits the native pipe:
#' `program |> compile(teleprompter, trainset)`.
#'
#' @param program The module or pipeline to optimize. Programs made with
#'   [module_fn()] cannot be compiled.
#' @param teleprompter A teleprompter object that sets the optimization
#'   strategy. Integer settings of teleprompters need integer literals, as in
#'   `LabeledFewShot(k = 3L)`; `k = 3` is an error.
#' @param ... The training set and optimizer options:
#'   - `trainset` (required, third argument): a data frame with the program's
#'     input columns and the expected outputs.
#'   - `valset`: an optional validation data frame, for teleprompters that
#'     use one. It may also be given as the fourth argument.
#'   - `.llm`: an ellmer Chat for the program's calls (see [run()]).
#'   - `.trace_context`: a named, JSON-compatible list copied into the
#'     metadata and traces of the calls made while compiling.
#'
#'   Some teleprompters take further arguments; see their help pages.
#'
#' @details
#' Teleprompters that score candidates call the metric as
#' `metric(prediction, expected)`, where `expected` is the whole training
#' row, so give built-in metrics a `field`, as in
#' `metric_exact_match(field = "sentiment")`. Compiling a program that is
#' already compiled works but gives a warning.
#'
#' @return A new, compiled program of the same kind as `program`. Check it
#'   with `compiled$is_compiled()`.
#' @export
#' @family teleprompters
#' @examples
#' classifier <- module(
#'   signature("text -> sentiment: enum('positive', 'negative')")
#' )
#' trainset <- data.frame(
#'   text = c("I love it!", "Terrible experience", "Works great", "Broke in a day"),
#'   sentiment = c("positive", "negative", "positive", "negative")
#' )
#'
#' # LabeledFewShot copies training rows into the prompt as demos, so it
#' # needs no model calls
#' compiled <- classifier |> compile(LabeledFewShot(k = 2L), trainset)
#' compiled$is_compiled()
#' classifier$is_compiled()
#' compiled$demo_table
#'
#' \dontrun{
#' # BootstrapFewShot runs the program and keeps demos that pass the metric
#' bootstrapped <- classifier |>
#'   compile(
#'     BootstrapFewShot(
#'       metric = metric_exact_match(field = "sentiment"),
#'       max_bootstrapped_demos = 2L
#'     ),
#'     trainset,
#'     .llm = ellmer::chat_openai(model = "gpt-6-luna")
#'   )
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
