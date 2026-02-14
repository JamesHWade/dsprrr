# rlm-live.R - Live mode: async RLM execution

#' Run an RLM query asynchronously
#' @param session Shiny session
#' @param config List with api_key, question, model, context fields
run_live_rlm <- function(session, config) {
  post_message(session, "live_status", list(status = "running"))

  promises::future_promise(
    {
      library(dsprrr)
      library(ellmer)

      llm <- ellmer::chat_openai(
        model = config$model %||% "gpt-4o-mini",
        api_key = config$api_key
      )

      runner <- r_code_runner(timeout = 30)

      rlm <- rlm_module(
        signature = "question -> answer",
        runner = runner,
        max_iterations = 15L,
        verbose = FALSE
      )

      result <- run(rlm, question = config$question, .llm = llm)

      history <- rlm$get_repl_history()
      last_run <- history[[length(history)]]

      list(
        run_id = paste0("live-", format(Sys.time(), "%H%M%S")),
        timestamp = as.character(Sys.time()),
        question = config$question,
        model = config$model %||% "gpt-4o-mini",
        context_variables = list(),
        iterations = lapply(last_run$history, function(h) {
          list(
            iteration = h$iteration,
            reasoning = h$reasoning,
            code = h$code,
            output = h$output,
            success = h$success,
            is_final = h$is_final
          )
        }),
        final_answer = get_output(result)$answer,
        iterations_used = last_run$iterations_used,
        llm_calls_used = last_run$llm_calls_used
      )
    },
    seed = NULL
  ) |> promises::then(
    onFulfilled = function(trace_result) {
      post_message(session, "live_result", trace_result)
      post_message(session, "live_status", list(status = "complete"))
    },
    onRejected = function(err) {
      post_message(session, "live_status", list(
        status = "error",
        message = conditionMessage(err)
      ))
    }
  )
}
