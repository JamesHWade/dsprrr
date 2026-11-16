test_that("ellmer adapter preserves runtime Chat settings on a clone", {
  chat <- expect_no_warning(ellmer::chat_openai(
    credentials = function() "dummy-key",
    model = "compatibility-model"
  ))
  turns <- list(ellmer::UserTurn("prior turn"))
  chat$set_turns(turns)

  original_fingerprint <- dsprrr:::cache_provider_fingerprint(chat)
  configured <- expect_no_warning(dsprrr:::apply_chat_params(
    chat,
    list(temperature = 0.2, custom_flag = TRUE)
  ))
  configured_fingerprint <- expect_no_warning(
    dsprrr:::cache_provider_fingerprint(configured)
  )
  model <- dsprrr:::ellmer_chat_model_values(configured, required = TRUE)

  expect_false(identical(configured, chat))
  expect_identical(configured$get_turns(), turns)
  expect_identical(model$name, "compatibility-model")
  expect_identical(model$params$temperature, 0.2)
  expect_identical(model$extra_args, list(custom_flag = TRUE))
  expect_null(chat$get_model_object()@params$temperature)
  expect_identical(original_fingerprint$model, "compatibility-model")
  expect_false(identical(
    original_fingerprint$config,
    configured_fingerprint$config
  ))
  expect_true(dsprrr:::ellmer_chat_cache_supported(chat))
})

test_that("unsupported ellmer Chat layouts raise a typed compatibility error", {
  chat <- ellmer::chat_openai(
    credentials = function() "dummy-key",
    model = "compatibility-model"
  )
  chat$.__enclos_env__$private$model <- NULL

  error <- tryCatch(
    dsprrr:::apply_chat_params(chat, list(temperature = 0.2)),
    error = identity
  )

  expect_s3_class(error, "dsprrr_ellmer_compatibility_error")
  expect_s3_class(error, "dsprrr_chat_params_error")
  expect_match(
    conditionMessage(error),
    "Chat model binding is not a compatible"
  )
})

test_that("cache compatibility rejects overridden getters before calling them", {
  chat <- ellmer::chat_openai(
    credentials = function() "dummy-key",
    model = "compatibility-model"
  )
  provider_calls <- 0L
  override_test_chat_method(chat, "get_provider", function() {
    provider_calls <<- provider_calls + 1L
    stop("overridden provider getter must not run")
  })

  supported <- dsprrr:::ellmer_chat_cache_supported(chat)

  expect_identical(supported, FALSE)
  expect_identical(provider_calls, 0L)
})

test_that("cache compatibility rejects non-ellmer namespaces before getters", {
  chat <- new_test_chat()
  provider_calls <- 0L
  override_test_chat_method(chat, "get_provider", function() {
    provider_calls <<- provider_calls + 1L
    stop("custom provider getter must not run")
  })

  supported <- dsprrr:::ellmer_chat_cache_supported(chat)

  expect_identical(supported, FALSE)
  expect_identical(provider_calls, 0L)
})

test_that("ellmer JSON content reconstruction stays behind the adapter", {
  content <- dsprrr:::ellmer_content_json(data = list(answer = "ok"))

  expect_s3_class(content, "ellmer::ContentJson")
  expect_identical(content@data, list(answer = "ok"))
})

test_that("missing ellmer content constructors raise a compatibility error", {
  local_mocked_bindings(
    ellmer_internal_constructor = function(name) NULL,
    .package = "dsprrr"
  )

  error <- tryCatch(
    dsprrr:::ellmer_content_json(data = list(answer = "ok")),
    error = identity
  )

  expect_s3_class(error, "dsprrr_ellmer_compatibility_error")
  expect_match(conditionMessage(error), "does not provide the ContentJson")
})
