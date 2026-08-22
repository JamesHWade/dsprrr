#' Compile S7 Generic and Methods
#'
#' This file defines the compile generic and its methods for optimizing
#' DSPrrr modules using teleprompters.

#' Compile Generic
#'
#' @description
#' Generic method for compiling/optimizing a module using a teleprompter.
#'
#' @param teleprompter A Teleprompter object
#' @param program A module to optimize
#' @param ... Additional arguments including `trainset` (training data) and
#'   optional `.trace_context`, a named JSON-compatible list propagated to
#'   evaluations, optimizer trials, and execution traces.
#'
#' @return An optimized module
#' @seealso [compile_module()] for the pipe-friendly wrapper with
#'   validation and friendlier argument order
#' @examples
#' \dontrun{
#' classifier <- module(signature("text -> sentiment"), type = "predict")
#' trainset <- data.frame(
#'   text = c("I love it!", "Terrible experience"),
#'   sentiment = c("positive", "negative")
#' )
#' optimized <- compile(LabeledFewShot(k = 2L), classifier, trainset)
#' }
#' @export
compile <- S7::new_generic("compile", c("teleprompter", "program"))

# Method registration moved to zzz.R to ensure proper loading order

compile_with_trace_context <- function(
  compiler,
  teleprompter,
  program,
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

#' Compile a DSPrrr Program
#'
#' @description
#' Main user-facing function to compile/optimize a DSPrrr module using
#' a teleprompter optimization strategy.
#'
#' @param program A DSPrrr module to optimize (e.g., from `module()`)
#' @param teleprompter A Teleprompter object defining the optimization strategy
#' @param trainset Training data as a data frame
#' @param valset Optional validation set for evaluation
#' @param .llm Optional ellmer chat object to reuse during compilation
#' @param .trace_context A named, JSON-compatible list propagated to
#'   evaluations, optimizer trials, and execution traces.
#' @param ... Additional arguments passed to the teleprompter
#'
#' @return An optimized module with updated demonstrations and/or instructions
#'
#' @export
#' @examples
#' \dontrun{
#' # Create a simple module
#' classifier <- signature("text -> sentiment") |>
#'   module(type = "predict")
#'
#' # Prepare training data
#' trainset <- data.frame(
#'   text = c("I love it!", "Terrible experience"),
#'   sentiment = c("positive", "negative")
#' )
#'
#' # Compile with LabeledFewShot
#' tp <- LabeledFewShot(k = 2)
#' optimized <- compile_module(classifier, tp, trainset)
#'
#' # Compile with GridSearch
#' variants <- data.frame(
#'   id = c("terse", "detailed"),
#'   instructions_suffix = c(
#'     "Be concise.",
#'     "Provide detailed reasoning."
#'   )
#' )
#' tp <- GridSearchTeleprompter(
#'   variants = variants,
#'   metric = metric_exact_match(field = "sentiment")
#' )
#' optimized <- compile_module(classifier, tp, trainset)
#' }
compile_module <- function(
  program,
  teleprompter,
  trainset,
  valset = NULL,
  .llm = NULL,
  ...,
  .trace_context = list()
) {
  trace_context_supplied <- !missing(.trace_context)
  # Validate inputs
  if (!inherits(teleprompter, "dsprrr::Teleprompter")) {
    cli::cli_abort(c(
      "teleprompter must be a Teleprompter object",
      "i" = "Got: {.cls {class(teleprompter)[1]}}"
    ))
  }

  if (!is.data.frame(trainset)) {
    cli::cli_abort("trainset must be a data frame")
  }

  if (!is.null(valset) && !is.data.frame(valset)) {
    valset <- tryCatch(
      as.data.frame(valset),
      error = function(e) {
        cli::cli_abort(c(
          "valset must be convertible to a data frame",
          "x" = e$message
        ))
      }
    )
  }

  # Check if program is already compiled and warn
  if (inherits(program, "Module") && program$is_compiled()) {
    cli::cli_warn(c(
      "Program appears to be already compiled",
      "i" = "Previous teleprompter: {program$config$teleprompter}",
      "i" = "Recompiling with: {class(teleprompter)[1]}"
    ))
  }

  # Dispatch to appropriate compile method
  args <- c(
    list(
      teleprompter = teleprompter,
      program = program,
      trainset = trainset,
      valset = valset,
      .llm = .llm
    ),
    list(...)
  )
  if (trace_context_supplied) {
    args$.trace_context <- .trace_context
  }
  do.call(compile, args)
}
