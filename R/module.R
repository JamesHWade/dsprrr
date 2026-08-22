#' Create a prediction module
#'
#' @description
#' Create a standard structured-prediction module. This is the primary
#' constructor in the dsprrr journey:
#' [signature()] -> `module()` -> [run()] -> [evaluate()] -> [compile()].
#'
#' Agentic, reasoning, and code-executing programs use explicit constructors
#' such as [react()], [chain_of_thought()], [multi_chain_comparison()],
#' [program_of_thought()], [code_act()], [rlm_module()], and [flex()]. Keeping
#' those choices in the function name prevents configuration from silently
#' changing the kind of program being built.
#'
#' @param signature A Signature object defining the module interface.
#' @param chat Optional ellmer Chat object. When supplied, [run()] uses it unless
#'   an explicit `.llm` is provided.
#' @param template Optional glue template for prompt generation.
#' @param demos Optional list of demonstration examples.
#' @param config Optional prediction configuration. Model parameters such as
#'   temperature belong here, for example `config = list(temperature = 0.2)`.
#' @param ... Must be empty. Use a dedicated constructor for advanced module
#'   behavior.
#'
#' @return A PredictModule executed with [run()].
#' @export
#' @examples
#' classifier <- signature("text -> sentiment") |>
#'   module(template = "Analyze: {text}")
#'
#' configured <- signature("question -> answer") |>
#'   module(config = list(temperature = 0.2))
#'
#' \dontrun{
#' llm <- ellmer::chat_openai()
#' result <- classifier |>
#'   run(text = "Great package!", .llm = llm)
#' }
module <- function(
  signature,
  chat = NULL,
  template = "",
  demos = list(),
  config = list(),
  ...
) {
  reject_partial_argument_matches(sys.call(), sys.function())
  reject_module_arguments(...)

  if (!inherits(signature, "dsprrr::Signature")) {
    cli::cli_abort(c(
      "First argument must be a Signature object",
      "x" = "Got {.cls {class(signature)[1]}}",
      "i" = "Create one with: {.code signature('question -> answer')}"
    ))
  }

  assert_ellmer_chat(chat, arg = "chat", allow_null = TRUE)

  mod <- PredictModule$new(
    signature = signature,
    template = template,
    demos = demos,
    config = config,
    chat = chat
  )
  stamp_module_kind(mod, "predict")
}

#' Record the constructor contract on a module
#' @noRd
stamp_module_kind <- function(module, kind) {
  module$config <- normalize_module_config(module$config)
  module$config$.module_kind <- kind
  module
}


#' Construct a validated module kind for internal program interpreters
#'
#' Flex reads a validated primitive name from program source. Keeping this
#' router private preserves that interpreter capability without restoring the
#' public `module(type = ...)` dispatcher.
#' @noRd
construct_module_kind <- function(kind, signature, ...) {
  switch(
    kind,
    predict = module(signature, ...),
    react = react(signature, ...),
    chain_of_thought = chain_of_thought(signature, ...),
    multichain = multi_chain_comparison(signature, ...),
    program_of_thought = program_of_thought(signature, ...),
    codeact = code_act(signature, ...),
    rlm = rlm_module(signature, ...),
    flex = flex(signature, ...),
    cli::cli_abort(
      "Unsupported internal module kind {.val {kind}}",
      class = "dsprrr_module_kind_error",
      kind = kind
    )
  )
}

#' Reject advanced behavior passed to `module()`
#' @noRd
reject_module_arguments <- function(...) {
  dots <- rlang::dots_list(
    ...,
    .ignore_empty = "none",
    .homonyms = "error",
    .check_assign = TRUE
  )
  if (length(dots) == 0L) {
    return(invisible(NULL))
  }

  supplied <- names(dots)
  supplied[is.na(supplied) | supplied == ""] <- "<unnamed>"
  argument_list <- paste0("`", supplied, "`", collapse = ", ")

  constructor_hint <- if ("type" %in% supplied) {
    paste0(
      "Choose the constructor directly: react(), chain_of_thought(), ",
      "multi_chain_comparison(), program_of_thought(), code_act(), ",
      "rlm_module(), or flex()."
    )
  } else if (any(supplied %in% c("tools", "max_iterations"))) {
    "Use react() for tool use, or code_act() when code execution is also required."
  } else if (any(supplied %in% c("M", "temperature", "inner_module"))) {
    paste0(
      "Use multi_chain_comparison() for multiple reasoning chains. ",
      "For ordinary model temperature, use config = list(temperature = ...)."
    )
  } else if (
    any(
      supplied %in%
        c(
          "runner",
          "interpreter_factory",
          "max_iters",
          "extract_answer"
        )
    )
  ) {
    "Use program_of_thought(), code_act(), rlm_module(), or flex()."
  } else {
    "Remove the argument or choose the dedicated advanced constructor that owns it."
  }

  cli::cli_abort(
    c(
      "`module()` creates standard prediction modules and does not accept {argument_list}.",
      "i" = constructor_hint
    ),
    class = "dsprrr_module_argument_error",
    arguments = supplied
  )
}


#' Reject arguments that belong to another constructor
#' @noRd
reject_constructor_arguments <- function(constructor, ..., hint = NULL) {
  dots <- rlang::dots_list(
    ...,
    .ignore_empty = "none",
    .homonyms = "error",
    .check_assign = TRUE
  )
  if (length(dots) == 0L) {
    return(invisible(NULL))
  }

  supplied <- names(dots)
  supplied[is.na(supplied) | supplied == ""] <- "<unnamed>"
  argument_list <- paste0("`", supplied, "`", collapse = ", ")
  if (is.null(hint)) {
    hint <- "Remove arguments that are not documented for this constructor."
  }

  cli::cli_abort(
    c(
      "{.fn {constructor}} does not accept {argument_list}.",
      "i" = hint
    ),
    class = "dsprrr_module_argument_error",
    constructor = constructor,
    arguments = supplied
  )
}
