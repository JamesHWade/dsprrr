rlm_optimizer_type_fields <- function(type) {
  if (
    inherits(type, "ellmer::TypeObject") &&
      methods::.hasSlot(type, "properties")
  ) {
    return(names(type@properties))
  }
  character()
}

make_rlm_optimizer_chat <- function() {
  state <- new.env(parent = emptyenv())
  state$action_tuned <- FALSE
  state$action_prompts <- character()
  state$extract_prompts <- character()
  state$reflection_prompts <- character()

  new_chat <- NULL
  new_chat <- function() {
    chat <- new_test_chat(
      clone = function(deep = TRUE) new_chat(),
      model = "rlm-optimizer-test",
      chat_structured = function(prompt, type, ...) {
        fields <- rlm_optimizer_type_fields(type)

        if (identical(fields, "instructions")) {
          state$reflection_prompts <- c(state$reflection_prompts, prompt)
          if (
            grepl("Choose the next useful R operation", prompt, fixed = TRUE) ||
              grepl("ACTION-TUNED", prompt, fixed = TRUE)
          ) {
            return(list(instructions = "ACTION-TUNED"))
          }
          return(list(instructions = "EXTRACT-TUNED"))
        }

        if (setequal(fields, c("reasoning", "code"))) {
          state$action_prompts <- c(state$action_prompts, prompt)
          state$action_tuned <- grepl("ACTION-TUNED", prompt, fixed = TRUE) ||
            grepl("ACTION-HARNESS", prompt, fixed = TRUE)
          return(list(
            reasoning = "Inspect a bounded piece of evidence.",
            code = paste0(
              "paste0('TRACE_HEAD_', strrep('x', 500L), ",
              "'_TRACE_TAIL')"
            )
          ))
        }

        state$extract_prompts <- c(state$extract_prompts, prompt)
        extract_tuned <- grepl("EXTRACT-TUNED", prompt, fixed = TRUE) ||
          grepl("EXTRACT-HARNESS", prompt, fixed = TRUE)
        list(answer = if (state$action_tuned && extract_tuned) "yes" else "no")
      }
    )
    chat
  }

  chat <- new_chat()
  chat$optimizer_state <- state
  chat
}

make_rlm_optimizer_program <- function(runner, max_output_chars = 96L) {
  rlm_module(
    signature(
      "question -> answer",
      instructions = "Answer from the inspected evidence."
    ),
    runner = runner,
    max_iterations = 1L,
    max_llm_calls = 0L,
    max_output_chars = max_output_chars
  )
}

rlm_optimizer_accuracy <- function(prediction, expected) {
  predicted <- if (is.list(prediction)) prediction$answer else prediction
  as.numeric(identical(as.character(predicted), expected$answer))
}

capture_rlm_optimizer_warnings <- function(expr) {
  messages <- character()
  value <- withCallingHandlers(
    force(expr),
    warning = function(warning) {
      messages <<- c(messages, conditionMessage(warning))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, messages = messages)
}

expect_only_rlm_fallback_warnings <- function(result) {
  expect_gt(length(result$messages), 0L)
  expect_true(all(grepl(
    "RLM reached max_iterations",
    result$messages,
    fixed = TRUE
  )))
  invisible(result$value)
}
