#' Create a prediction module
#'
#' @description
#' `module()` turns a signature into a prediction module: one structured model
#' call per input, whose output follows the signature. It is the usual
#' starting point:
#' [signature()] -> `module()` -> [run()] -> [evaluate()] -> [compile()].
#'
#' Other kinds of program have their own constructors: [chain_of_thought()]
#' adds a reasoning step, [react()] calls tools, [multi_chain_comparison()]
#' compares several reasoning chains, and [program_of_thought()],
#' [code_act()], [rlm_module()] and [flex()] run code.
#'
#' @param signature A signature from [signature()]. A string is not accepted
#'   here; wrap it in [signature()].
#' @param chat An ellmer Chat stored on the module. [run()] uses it unless you
#'   pass `.llm`; see [get_default_chat()] for the full order.
#' @param template A glue template for the input part of the prompt, with
#'   input fields in single braces, as in `"Review: {text}"`. The default
#'   (`""`) lists each input as `name: value`.
#' @param demos Worked examples placed before the input in every prompt: a
#'   list of `list(inputs = list(...), output = list(...))`. [compile()] sets
#'   demos for you.
#' @param config Model settings applied to a copy of the chat on every call:
#'   `temperature`, `top_p`, `reasoning_effort`, `max_tokens`,
#'   `max_output_tokens`, `frequency_penalty`, `presence_penalty` and
#'   `service_tier`, for example `config = list(reasoning_effort = "low")`.
#'   Reasoning models such as gpt-6-luna accept `temperature` and `top_p` only
#'   with `reasoning_effort = "none"`. Chat settings such as `model` or
#'   `provider` are an error here; set them on the chat instead.
#' @param ... Must be empty. Arguments of other constructors, such as `tools`
#'   or `type`, give an error that names the constructor to use.
#'
#' @return A prediction module (an R6 object of class `PredictModule`) to use
#'   with [run()], [run_dataset()], [evaluate()] and [compile()].
#' @export
#' @family program constructors
#' @examples
#' classifier <- module(
#'   signature("text -> sentiment: enum('positive', 'negative', 'neutral')"),
#'   template = "Classify the sentiment of this review:\n{text}",
#'   demos = list(
#'     list(
#'       inputs = list(text = "Arrived broken."),
#'       output = list(sentiment = "negative")
#'     )
#'   ),
#'   config = list(reasoning_effort = "low")
#' )
#' classifier
#'
#' \dontrun{
#' run(
#'   classifier,
#'   text = "Great package!",
#'   .llm = ellmer::chat_openai(model = "gpt-6-luna")
#' )
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
