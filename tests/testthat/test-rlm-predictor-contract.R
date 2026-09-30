rlm_contract_runner <- function() {
  list(
    execute = function(code, context = list()) {
      list(success = TRUE, result = NULL)
    },
    policy = function() {
      list(
        backend = "test",
        trust = "test",
        sandboxed = TRUE,
        persistent = TRUE
      )
    }
  )
}


rlm_contract_output <- function(
  answer_required = TRUE,
  ignore_required = TRUE,
  descriptions = "original"
) {
  ellmer::type_object(
    .description = paste(descriptions, "root"),
    answer = ellmer::type_string(
      description = paste(descriptions, "answer"),
      required = answer_required
    ),
    details = ellmer::type_object(
      .description = paste(descriptions, "details"),
      count = ellmer::type_integer(
        description = paste(descriptions, "count"),
        required = FALSE
      ),
      internal = ellmer::TypeIgnore(required = ignore_required)
    )
  )
}


rlm_contract_signature <- function(output_type = rlm_contract_output()) {
  signature(
    inputs = list(input("question", type = ellmer::type_string())),
    output_type = output_type
  )
}


rlm_contract_action <- function(
  state_type = ellmer::type_string(),
  output_type = ellmer::type_object(
    reasoning = ellmer::type_string(),
    code = ellmer::type_string()
  ),
  extra_inputs = list()
) {
  PredictModule$new(
    signature = signature(
      inputs = c(
        list(input(
          "state",
          type = state_type,
          description = "A replaceable state description"
        )),
        extra_inputs
      ),
      output_type = output_type
    )
  )
}


rlm_contract_extract <- function(output_type) {
  PredictModule$new(
    signature = signature(
      inputs = list(input(
        "state",
        type = ellmer::type_string(description = "Different description")
      )),
      output_type = output_type
    )
  )
}


test_that("RLM predictor contracts accept defaults and ignore descriptions", {
  runner <- rlm_contract_runner()
  root_signature <- rlm_contract_signature()

  defaults <- RLMModule$new(signature = root_signature, runner = runner)
  expect_s3_class(defaults$generate_action, "Module")
  expect_s3_class(defaults$extract, "Module")

  custom <- RLMModule$new(
    signature = root_signature,
    runner = runner,
    generate_action = rlm_contract_action(
      state_type = ellmer::type_string(description = "Custom state"),
      output_type = ellmer::type_object(
        .description = "Custom action",
        reasoning = ellmer::type_string(description = "Custom reasoning"),
        code = ellmer::type_string(description = "Custom code")
      )
    ),
    extract = rlm_contract_extract(
      rlm_contract_output(descriptions = "replacement")
    )
  )

  expect_s3_class(custom, "RLMModule")
})


test_that("RLM constructor rejects incompatible generate_action contracts", {
  runner <- rlm_contract_runner()
  root_signature <- rlm_contract_signature()
  extract <- rlm_contract_extract(root_signature@output_type)
  invalid <- list(
    optional_state = rlm_contract_action(
      state_type = ellmer::type_string(required = FALSE)
    ),
    extra_input = rlm_contract_action(
      extra_inputs = list(input("other", type = ellmer::type_string()))
    ),
    wrong_field_type = rlm_contract_action(
      output_type = ellmer::type_object(
        reasoning = ellmer::type_number(),
        code = ellmer::type_string()
      )
    ),
    optional_code = rlm_contract_action(
      output_type = ellmer::type_object(
        reasoning = ellmer::type_string(),
        code = ellmer::type_string(required = FALSE)
      )
    ),
    extra_output = rlm_contract_action(
      output_type = ellmer::type_object(
        reasoning = ellmer::type_string(),
        code = ellmer::type_string(),
        confidence = ellmer::type_number()
      )
    ),
    open_output = rlm_contract_action(
      output_type = ellmer::TypeObject(
        properties = list(
          reasoning = ellmer::type_string(),
          code = ellmer::type_string()
        ),
        additional_properties = TRUE
      )
    )
  )

  for (name in names(invalid)) {
    expect_error(
      RLMModule$new(
        signature = root_signature,
        runner = runner,
        generate_action = invalid[[name]],
        extract = extract
      ),
      class = "dsprrr_rlm_generate_action_contract_error",
      info = name
    )
  }
})


test_that("RLM constructor matches the complete extract output contract", {
  runner <- rlm_contract_runner()
  root_signature <- rlm_contract_signature()
  action <- rlm_contract_action()

  expect_error(
    RLMModule$new(
      signature = root_signature,
      runner = runner,
      generate_action = action,
      extract = rlm_contract_extract(
        rlm_contract_output(answer_required = FALSE)
      )
    ),
    class = "dsprrr_rlm_extract_contract_error"
  )

  expect_error(
    RLMModule$new(
      signature = root_signature,
      runner = runner,
      generate_action = action,
      extract = rlm_contract_extract(
        rlm_contract_output(ignore_required = FALSE)
      )
    ),
    class = "dsprrr_rlm_extract_contract_error"
  )

  missing_ignore <- ellmer::type_object(
    answer = ellmer::type_string(),
    details = ellmer::type_object(
      count = ellmer::type_integer(required = FALSE)
    )
  )
  expect_error(
    RLMModule$new(
      signature = root_signature,
      runner = runner,
      generate_action = action,
      extract = rlm_contract_extract(missing_ignore)
    ),
    class = "dsprrr_rlm_extract_contract_error"
  )
})


test_that("RLM graph child replacement validates atomically", {
  module <- RLMModule$new(
    signature = rlm_contract_signature(),
    runner = rlm_contract_runner()
  )
  original_action <- module$generate_action
  original_extract <- module$extract

  invalid_action <- rlm_contract_action(
    state_type = ellmer::type_number()
  )
  expect_error(
    module$set_graph_children(list(
      generate_action = invalid_action,
      extract = original_extract
    )),
    class = "dsprrr_rlm_generate_action_contract_error"
  )
  expect_identical(module$generate_action, original_action)
  expect_identical(module$extract, original_extract)

  invalid_extract <- rlm_contract_extract(
    rlm_contract_output(answer_required = FALSE)
  )
  expect_error(
    module$set_graph_children(list(
      generate_action = original_action,
      extract = invalid_extract
    )),
    class = "dsprrr_rlm_extract_contract_error"
  )
  expect_identical(module$generate_action, original_action)
  expect_identical(module$extract, original_extract)
})


test_that("RLM artifact restoration rejects incompatible predictor contracts", {
  runner <- rlm_contract_runner()
  artifact <- program_artifact(
    RLMModule$new(
      signature = rlm_contract_signature(),
      runner = runner
    ),
    registry = list(runner = runner)
  )
  restore_condition <- function(value) {
    value$integrity <- dsprrr:::artifact_integrity(value)
    rlang::catch_cnd(restore_module_config(
      value,
      registry = list(runner = runner)
    ))
  }

  invalid_action <- artifact
  invalid_action$graph$nodes[[
    "$/generate_action"
  ]]$signature$output_type$properties$code$required <- FALSE
  condition <- restore_condition(invalid_action)

  expect_s3_class(condition, "dsprrr_artifact_malformed")
  expect_s3_class(
    condition$parent,
    "dsprrr_rlm_generate_action_contract_error"
  )

  invalid_extract <- artifact
  invalid_extract$graph$nodes[[
    "$/extract"
  ]]$signature$output_type$properties$answer$required <- FALSE
  condition <- restore_condition(invalid_extract)

  expect_s3_class(condition, "dsprrr_artifact_malformed")
  expect_s3_class(condition$parent, "dsprrr_rlm_extract_contract_error")
})
