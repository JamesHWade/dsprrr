# Frozen, hand-authored examples of the format-version 2 persistence contract.
# Do not regenerate these records with serialize_module_config_v2(): they are
# migration inputs whose shape must remain independent of the current writer.

artifact_v2_fixture_signature <- function(input, output) {
  input_description <- paste("Input:", input)
  output_properties <- list(ellmer::type_string())
  names(output_properties) <- output

  list(
    inputs = list(list(
      name = input,
      description = input_description,
      type = ellmer::type_string(description = input_description),
      extra = list(.type_explicit = FALSE)
    )),
    output_type = do.call(ellmer::type_object, output_properties),
    instructions = paste0(
      "Given the fields `",
      input,
      "`, produce the fields `",
      output,
      "`."
    )
  )
}

artifact_v2_fixture_state <- function(
  compiled = FALSE,
  best_score = NULL,
  best_trial = NULL,
  best_params = list()
) {
  list(
    compiled = compiled,
    best_score = best_score,
    best_trial = best_trial,
    best_params = best_params,
    trials = tibble::tibble(),
    last_grid = tibble::tibble(),
    optimization_history = list()
  )
}

artifact_v2_fixture_predict <- function(
  input = "text",
  output = "answer",
  module_kind = "predict",
  template = "Legacy template: {text}",
  demos = list(list(text = "old", answer = "format"))
) {
  list(
    format_version = 2L,
    module_kind = module_kind,
    signature = artifact_v2_fixture_signature(input, output),
    config = list(temperature = 0.3),
    state = artifact_v2_fixture_state(),
    fields = list(template = template, demos = demos),
    metadata = list(
      module_class = "PredictModule",
      dsprrr_version = "0.0.0.9000"
    )
  )
}

artifact_v2_fixtures <- list(
  predict = artifact_v2_fixture_predict(
    demos = list(list(
      text = "old",
      answer = "format",
      private_key = "DROP_V2_SECRET"
    ))
  ),
  react = list(
    format_version = 2L,
    module_kind = "react",
    signature = artifact_v2_fixture_signature("question", "answer"),
    config = list(temperature = 0.1),
    state = artifact_v2_fixture_state(),
    fields = list(
      template = "Legacy ReAct template: {question}",
      demos = list(),
      max_iterations = 7L,
      tools = list()
    ),
    metadata = list(
      module_class = "ReactModule",
      dsprrr_version = "0.0.0.9000"
    )
  ),
  chain_of_thought = artifact_v2_fixture_predict(
    input = "question",
    module_kind = "chain_of_thought",
    template = "Reason before answering: {question}"
  ),
  multichain = list(
    format_version = 2L,
    module_kind = "multichain",
    signature = artifact_v2_fixture_signature("question", "answer"),
    config = list(temperature = 0.4),
    state = artifact_v2_fixture_state(),
    fields = list(
      M = 5L,
      temperature = 0.4,
      comparison_template = "Compare the candidate answers.",
      inner_module = artifact_v2_fixture_predict(
        input = "question",
        module_kind = "chain_of_thought",
        template = "Inner legacy reasoning: {question}"
      )
    ),
    metadata = list(
      module_class = "MultiChainComparisonModule",
      dsprrr_version = "0.0.0.9000"
    )
  )
)

artifact_v2_fixtures$predict$config$client_secret <- "DROP_V2_SECRET"
artifact_v2_fixtures$predict$state <- artifact_v2_fixture_state(
  compiled = TRUE,
  best_score = 0.9,
  best_trial = 2L,
  best_params = list(
    temperature = 0.3,
    access_token = "DROP_V2_SECRET"
  )
)
artifact_v2_fixtures$predict$state$trials <- tibble::tibble(
  trial = 1L,
  evaluation = list(structure(
    list(note = "DROP_V2_RUNTIME_HISTORY"),
    class = "legacy_evaluation"
  ))
)
artifact_v2_fixtures$predict$state$optimization_history <- list(structure(
  list(note = "DROP_V2_RUNTIME_HISTORY"),
  class = "legacy_optimizer_history"
))

rm(
  artifact_v2_fixture_predict,
  artifact_v2_fixture_signature,
  artifact_v2_fixture_state
)
