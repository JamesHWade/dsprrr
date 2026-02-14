# rlm-live.R - Live mode: async RLM execution

#' Create an ellmer chat object from provider config
#' @param config List with provider, model, and optional api_key
#' @return An ellmer Chat object
create_chat <- function(config) {
  provider <- config$provider %||% "openai"
  model <- config$model %||% "gpt-5-mini"
  api_key <- config$api_key

  switch(provider,
    openai = {
      args <- list(model = model)
      if (!is.null(api_key) && nzchar(api_key)) args$api_key <- api_key
      do.call(ellmer::chat_openai, args)
    },
    anthropic = {
      args <- list(model = model)
      if (!is.null(api_key) && nzchar(api_key)) args$api_key <- api_key
      do.call(ellmer::chat_anthropic, args)
    },
    google = {
      args <- list(model = model)
      if (!is.null(api_key) && nzchar(api_key)) args$api_key <- api_key
      do.call(ellmer::chat_google_gemini, args)
    },
    groq = {
      args <- list(model = model)
      if (!is.null(api_key) && nzchar(api_key)) args$api_key <- api_key
      do.call(ellmer::chat_groq, args)
    },
    github = {
      args <- list(model = model)
      if (!is.null(api_key) && nzchar(api_key)) args$api_key <- api_key
      do.call(ellmer::chat_github, args)
    },
    cli::cli_abort("Unknown provider: {provider}")
  )
}

#' Run an RLM query asynchronously
#' @param session Shiny session
#' @param config List with provider, model, api_key, question fields
run_live_rlm <- function(session, config) {
  post_message(session, "live_status", list(status = "running"))

  promises::future_promise(
    {
      library(dsprrr)
      library(ellmer)

      llm <- create_chat(config)

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
        model = config$model %||% "gpt-5-mini",
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
