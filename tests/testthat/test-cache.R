# Tests for LLM response caching
# Note: local_reset_cache() helper is defined in helper-cache.R

local_cache_openai_backend <- function(
  calls,
  response = list(answer = "ok"),
  .env = parent.frame()
) {
  testthat::local_mocked_bindings(
    req_perform = function(req) {
      calls$n <- calls$n + 1L
      value <- if (is.function(response)) response(calls$n) else response
      body <- list(
        id = paste0("resp_", calls$n),
        object = "response",
        created_at = 1L,
        status = "completed",
        model = "model-a",
        output = list(list(
          id = paste0("msg_", calls$n),
          type = "message",
          status = "completed",
          role = "assistant",
          content = list(list(
            type = "output_text",
            annotations = list(),
            logprobs = list(),
            text = as.character(jsonlite::toJSON(
              value,
              auto_unbox = TRUE,
              null = "null"
            ))
          ))
        )),
        usage = list(
          input_tokens = 8L,
          input_tokens_details = list(cached_tokens = 0L),
          output_tokens = 2L,
          output_tokens_details = list(reasoning_tokens = 0L),
          total_tokens = 10L
        ),
        service_tier = "default",
        metadata = list()
      )
      getFromNamespace("response", "httr2")(
        headers = list(`content-type` = "application/json"),
        body = charToRaw(jsonlite::toJSON(
          body,
          auto_unbox = TRUE,
          null = "null"
        ))
      )
    },
    .package = "ellmer",
    .env = .env
  )
  invisible(calls)
}

cache_real_chat <- function(initial_turns = list(), system_prompt = NULL) {
  chat <- suppressWarnings(ellmer::chat_openai(
    api_key = "dummy-key",
    model = "model-a",
    system_prompt = system_prompt
  ))
  chat$set_turns(initial_turns)
  chat
}

cache_test_key <- function(chat, payload, output_type, rollout_id = NULL) {
  fingerprint <- dsprrr:::cache_request_fingerprint(
    llm = chat,
    payload = payload,
    output_type = output_type,
    rollout_id = rollout_id
  )
  dsprrr:::cache_key(
    prompt = payload,
    model = "",
    output_type = output_type,
    fingerprint = fingerprint
  )
}

test_that("configure_cache sets default values", {
  local_reset_cache()

  configure_cache()

  config <- dsprrr:::get_cache_config()
  expect_true(config$enable)
  expect_true(config$enable_memory)
  expect_true(config$enable_disk)
  expect_equal(config$memory_max_entries, 1000L)
})

test_that("configure_cache can disable caching", {
  local_reset_cache()

  configure_cache(enable = FALSE)

  config <- dsprrr:::get_cache_config()
  expect_false(config$enable)
})

test_that("configure_cache can disable disk cache", {
  local_reset_cache()

  configure_cache(enable_disk = FALSE)

  config <- dsprrr:::get_cache_config()
  expect_true(config$enable)
  expect_true(config$enable_memory)
  expect_false(config$enable_disk)
})

test_that("configure_cache returns previous config", {
  local_reset_cache()

  configure_cache(memory_max_entries = 500L)
  old <- configure_cache(memory_max_entries = 1000L)

  expect_equal(old$memory_max_entries, 500L)
})

test_that("cache_key produces consistent keys", {
  key1 <- dsprrr:::cache_key("prompt", "gpt-4o", 0.7, "string")
  key2 <- dsprrr:::cache_key("prompt", "gpt-4o", 0.7, "string")

  expect_equal(key1, key2)
  expect_type(key1, "character")
  expect_equal(nchar(key1), 64)
})

test_that("cache_key differs with different prompts", {
  key1 <- dsprrr:::cache_key("prompt1", "gpt-4o", 0.7, "string")
  key2 <- dsprrr:::cache_key("prompt2", "gpt-4o", 0.7, "string")

  expect_false(key1 == key2)
})

test_that("cache_key differs with different models", {
  key1 <- dsprrr:::cache_key("prompt", "gpt-4o", 0.7, "string")
  key2 <- dsprrr:::cache_key("prompt", "gpt-4o-mini", 0.7, "string")

  expect_false(key1 == key2)
})

test_that("cache_key differs with different temperatures", {
  key1 <- dsprrr:::cache_key("prompt", "gpt-4o", 0.7, "string")
  key2 <- dsprrr:::cache_key("prompt", "gpt-4o", 0.5, "string")

  expect_false(key1 == key2)
})

test_that("cache_key differs with rollout_id", {
  key1 <- dsprrr:::cache_key("prompt", "gpt-4o", 0.7, "string", rollout_id = 1)
  key2 <- dsprrr:::cache_key("prompt", "gpt-4o", 0.7, "string", rollout_id = 2)

  expect_false(key1 == key2)
})

test_that("cache_key without rollout_id differs from with rollout_id", {
  key1 <- dsprrr:::cache_key("prompt", "gpt-4o", 0.7, "string")
  key2 <- dsprrr:::cache_key("prompt", "gpt-4o", 0.7, "string", rollout_id = 1)

  expect_false(key1 == key2)
})

test_that("cache keys include exact recursive output schemas", {
  string <- ellmer::type_string()
  integer <- ellmer::type_integer()
  nested_a <- ellmer::type_object(
    answer = ellmer::type_array(
      ellmer::type_object(score = ellmer::type_number(required = TRUE))
    )
  )
  nested_b <- ellmer::type_object(
    answer = ellmer::type_array(
      ellmer::type_object(score = ellmer::type_integer(required = TRUE))
    )
  )
  json_a <- ellmer::type_from_schema(
    text = '{"type":"object","properties":{"password":{"type":"string"}}}'
  )
  json_b <- ellmer::type_from_schema(
    text = '{"type":"object","properties":{"password":{"type":"integer"}}}'
  )

  expect_false(identical(
    dsprrr:::serialize_output_type(string),
    dsprrr:::serialize_output_type(integer)
  ))
  expect_false(identical(
    dsprrr:::serialize_output_type(nested_a),
    dsprrr:::serialize_output_type(nested_b)
  ))
  expect_false(identical(
    dsprrr:::serialize_output_type(json_a),
    dsprrr:::serialize_output_type(json_b)
  ))
  expect_identical(
    dsprrr:::serialize_output_type(nested_a),
    dsprrr:::serialize_output_type(nested_a)
  )
})

test_that("request fingerprints partition all output-affecting Chat state", {
  make_chat <- function(
    system_prompt = NULL,
    base_url = "https://example.test/v1",
    model = "model-a",
    params = list(temperature = 0.2, top_p = 0.8, max_tokens = 50),
    api_args = list(tool_choice = "auto")
  ) {
    suppressWarnings(ellmer::chat_openai(
      system_prompt = system_prompt,
      base_url = base_url,
      api_key = "dummy-key",
      model = model,
      params = params,
      api_args = api_args
    ))
  }

  output_type <- ellmer::type_object(answer = ellmer::type_string())
  base <- make_chat()
  base_key <- cache_test_key(base, "prompt", output_type)
  same_key <- cache_test_key(base$clone(deep = TRUE), "prompt", output_type)
  expect_identical(base_key, same_key)

  variants <- list(
    make_chat(system_prompt = "different system prompt"),
    make_chat(base_url = "https://other.example.test/v1"),
    make_chat(model = "model-b"),
    make_chat(params = list(temperature = 0.3, top_p = 0.8, max_tokens = 50)),
    make_chat(params = list(temperature = 0.2, top_p = 0.7, max_tokens = 50)),
    make_chat(params = list(temperature = 0.2, top_p = 0.8, max_tokens = 51)),
    make_chat(api_args = list(tool_choice = "required")),
    suppressWarnings(
      ellmer::chat_anthropic(api_key = "dummy-key", model = "model-a")
    )
  )

  history_chat <- base$clone(deep = TRUE)
  history_chat$set_turns(list(
    ellmer::UserTurn(contents = list(ellmer::ContentText("prior question"))),
    ellmer::AssistantTurn(contents = list(ellmer::ContentText("prior answer")))
  ))
  variants <- c(variants, list(history_chat))

  variant_keys <- vapply(
    variants,
    cache_test_key,
    character(1),
    payload = "prompt",
    output_type = output_type
  )
  expect_false(any(variant_keys == base_key))

  expect_false(identical(
    cache_test_key(base, "prompt", output_type, rollout_id = 1),
    cache_test_key(base, "prompt", output_type, rollout_id = 2)
  ))
})

test_that("fingerprints hash content and partition credentials", {
  chat <- suppressWarnings(ellmer::chat_openai(
    system_prompt = "SYSTEM-SECRET",
    base_url = "https://example.test/v1?token=URL-SECRET",
    api_key = "API-SECRET",
    model = "model-a",
    params = list(temperature = 0.2),
    api_args = list(tool_choice = "auto", api_key = "ARG-SECRET"),
    api_headers = c(
      Authorization = "Bearer HEADER-SECRET",
      `X-Route` = "private-route"
    )
  ))
  chat$set_turns(list(
    ellmer::UserTurn(contents = list(ellmer::ContentText("HISTORY-SECRET")))
  ))

  fingerprint <- dsprrr:::cache_request_fingerprint(
    chat,
    "PROMPT-SECRET",
    ellmer::type_string()
  )
  material <- dsprrr:::cache_fingerprint_json(fingerprint)

  secrets <- c(
    "SYSTEM-SECRET",
    "URL-SECRET",
    "API-SECRET",
    "ARG-SECRET",
    "Authorization",
    "HEADER-SECRET",
    "private-route",
    "HISTORY-SECRET",
    "PROMPT-SECRET"
  )
  expect_false(any(vapply(
    secrets,
    grepl,
    logical(1),
    x = material,
    fixed = TRUE
  )))

  other_credentials <- suppressWarnings(ellmer::chat_openai(
    system_prompt = "SYSTEM-SECRET",
    base_url = "https://example.test/v1?token=URL-SECRET",
    api_key = "OTHER-API-SECRET",
    model = "model-a",
    params = list(temperature = 0.2),
    api_args = list(tool_choice = "auto", api_key = "ARG-SECRET"),
    api_headers = c(
      Authorization = "Bearer HEADER-SECRET",
      `X-Route` = "private-route"
    )
  ))
  other_credentials$set_turns(chat$get_turns())
  other_material <- dsprrr:::cache_fingerprint_json(
    dsprrr:::cache_request_fingerprint(
      other_credentials,
      "PROMPT-SECRET",
      ellmer::type_string()
    )
  )
  expect_false(grepl("OTHER-API-SECRET", other_material, fixed = TRUE))
  expect_false(identical(material, other_material))
  expect_false(identical(
    fingerprint$provider$account_partition,
    dsprrr:::cache_request_fingerprint(
      other_credentials,
      "PROMPT-SECRET",
      ellmer::type_string()
    )$provider$account_partition
  ))

  expect_true(all(vapply(
    c("token", "refresh_token", "session", "bearer"),
    dsprrr:::cache_is_secret_name,
    logical(1)
  )))
  expect_false(dsprrr:::cache_is_secret_name("max_tokens"))

  partition_a <- dsprrr:::cache_account_partition(list(
    credentials = function() "credential-a",
    token = "token-a",
    refresh_token = "refresh-a",
    session = "session-a",
    bearer = "bearer-a",
    max_tokens = 10L
  ))
  partition_b <- dsprrr:::cache_account_partition(list(
    credentials = function() "credential-b",
    token = "token-b",
    refresh_token = "refresh-b",
    session = "session-b",
    bearer = "bearer-b",
    max_tokens = 10L
  ))
  partition_max_tokens <- dsprrr:::cache_account_partition(list(
    credentials = function() "credential-a",
    token = "token-a",
    refresh_token = "refresh-a",
    session = "session-a",
    bearer = "bearer-a",
    max_tokens = 999L
  ))
  expect_false(identical(partition_a, partition_b))
  expect_identical(partition_a, partition_max_tokens)
})

test_that("multimodal content identity is hashed and partitions cache keys", {
  chat <- suppressWarnings(ellmer::chat_openai(
    api_key = "dummy-key",
    model = "model-a"
  ))
  output_type <- ellmer::type_string()
  payload <- function(image) {
    list(
      ellmer::ContentText("private prompt"),
      image
    )
  }

  inline_a <- ellmer::ContentImageInline(type = "image/png", data = "YWJj")
  inline_b <- ellmer::ContentImageInline(type = "image/png", data = "YWJk")
  remote_a <- ellmer::ContentImageRemote(
    url = "https://example.test/image?token=secret-a",
    detail = "low"
  )
  remote_b <- ellmer::ContentImageRemote(
    url = "https://example.test/image?token=secret-a",
    detail = "high"
  )

  keys <- c(
    cache_test_key(chat, payload(inline_a), output_type),
    cache_test_key(chat, payload(inline_b), output_type),
    cache_test_key(chat, payload(remote_a), output_type),
    cache_test_key(chat, payload(remote_b), output_type)
  )
  expect_length(unique(keys), 4)

  material <- dsprrr:::cache_fingerprint_json(
    dsprrr:::cache_request_fingerprint(
      chat,
      payload(remote_a),
      output_type
    )
  )
  expect_false(grepl("private prompt|secret-a|example.test", material))
})

test_that("legacy cache identities cannot satisfy current requests", {
  local_reset_cache()
  configure_cache(enable_memory = TRUE, enable_disk = FALSE)

  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  local_cache_openai_backend(calls)
  chat <- cache_real_chat()
  output_type <- ellmer::type_object(answer = ellmer::type_string())
  fingerprint <- dsprrr:::cache_request_fingerprint(
    chat,
    "prompt",
    output_type
  )
  legacy <- fingerprint
  legacy$version <- 1L
  legacy_key <- dsprrr:::cache_key(
    "prompt",
    "model-a",
    output_type = output_type,
    fingerprint = legacy
  )
  dsprrr:::get_cache()$set(legacy_key, list(answer = "legacy"))

  result <- dsprrr:::cached_chat_structured(
    chat,
    "prompt",
    output_type
  )
  expect_equal(result$answer, "ok")
  expect_equal(calls$n, 1L)
})

test_that("registered tools bypass cache and preserve implementations", {
  local_reset_cache()
  configure_cache(enable_memory = TRUE, enable_disk = FALSE)

  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  local_cache_openai_backend(calls)

  make_chat <- function(implementation) {
    tool <- ellmer::tool(
      implementation,
      name = "same_name",
      description = "Same metadata",
      arguments = list(value = ellmer::type_string())
    )
    chat <- cache_real_chat()
    chat$register_tool(tool)
    chat
  }
  effects <- new.env(parent = emptyenv())
  effects$a <- 0L
  effects$b <- 0L
  first_chat <- make_chat(function(value) {
    effects$a <- effects$a + 1L
    paste0("a-", value)
  })
  second_chat <- make_chat(function(value) {
    effects$b <- effects$b + 10L
    paste0("b-", value)
  })
  output_type <- ellmer::type_object(answer = ellmer::type_string())

  expect_warning(
    first <- dsprrr:::cached_chat_structured(
      first_chat,
      "prompt",
      output_type
    ),
    "Registered tools disable"
  )
  repeat_first <- suppressWarnings(
    dsprrr:::cached_chat_structured(first_chat, "prompt", output_type)
  )
  second <- suppressWarnings(
    dsprrr:::cached_chat_structured(second_chat, "prompt", output_type)
  )

  expect_equal(first$answer, "ok")
  expect_equal(repeat_first$answer, "ok")
  expect_equal(second$answer, "ok")
  expect_equal(calls$n, 3L)
  expect_equal(first_chat$get_tools()[[1]]("value"), "a-value")
  expect_equal(second_chat$get_tools()[[1]]("value"), "b-value")
  expect_equal(effects$a, 1L)
  expect_equal(effects$b, 10L)
  expect_equal(cache_stats()$hits, 0L)
})

test_that("get_cache returns NULL when disabled", {
  local_reset_cache()

  configure_cache(enable = FALSE)

  cache <- dsprrr:::get_cache()
  expect_null(cache)
})

test_that("get_cache creates memory-only cache", {
  local_reset_cache()

  # Use temp directory for disk to avoid pollution
  configure_cache(
    enable_memory = TRUE,
    enable_disk = FALSE
  )

  cache <- dsprrr:::get_cache()
  expect_false(is.null(cache))
})

test_that("clear_cache resets cache", {
  local_reset_cache()

  configure_cache(enable_memory = TRUE, enable_disk = FALSE)

  cache <- dsprrr:::get_cache()
  cache$set("test_key", "test_value")

  clear_cache()

  # After clear, key should be missing
  result <- cache$get("test_key")
  expect_true(cachem::is.key_missing(result))
})

test_that("cache_stats returns statistics structure", {
  local_reset_cache()

  stats <- cache_stats()

  expect_s3_class(stats, "dsprrr_cache_stats")
  expect_true("enabled" %in% names(stats))
  expect_true("hits" %in% names(stats))
  expect_true("misses" %in% names(stats))
  expect_true("hit_rate" %in% names(stats))
})

test_that("cache_stats hit_rate is correct", {
  local_reset_cache()

  # Simulate some hits and misses
  dsprrr:::increment_cache_stats("hits")
  dsprrr:::increment_cache_stats("hits")
  dsprrr:::increment_cache_stats("misses")

  stats <- cache_stats()
  expect_equal(stats$hits, 2L)
  expect_equal(stats$misses, 1L)
  expect_equal(stats$hit_rate, 2 / 3)
})

test_that("cache_enabled returns correct state", {
  local_reset_cache()

  configure_cache(enable = TRUE)
  expect_true(dsprrr:::cache_enabled())

  configure_cache(enable = FALSE)
  expect_false(dsprrr:::cache_enabled())
})

test_that("print.dsprrr_cache_stats works", {
  local_reset_cache()

  stats <- cache_stats()

  # Should not error when printed
  expect_no_error(print(stats))

  # Verify print method is dispatched (check class is set)
  expect_s3_class(stats, "dsprrr_cache_stats")
})

test_that("cache stores and retrieves values", {
  local_reset_cache()

  configure_cache(enable_memory = TRUE, enable_disk = FALSE)

  cache <- dsprrr:::get_cache()

  # Store a value
  cache$set("my_key", list(answer = "cached_result"))

  # Retrieve it
  result <- cache$get("my_key")

  expect_false(cachem::is.key_missing(result))
  expect_equal(result$answer, "cached_result")
})

test_that("clear_cache with 'memory' only clears memory", {
  local_reset_cache()

  # Memory only for simplicity
  configure_cache(enable_memory = TRUE, enable_disk = FALSE)

  cache <- dsprrr:::get_cache()
  cache$set("test_key", "test_value")

  clear_cache("memory")

  result <- cache$get("test_key")
  expect_true(cachem::is.key_missing(result))
})

test_that("DSPRRR_CACHE_ENABLED=false disables cache", {
  local_reset_cache()

  # Reset config to force re-read of env var
  pkg_env <- asNamespace("dsprrr")$.dsprrr_env
  pkg_env$cache_config <- NULL

  withr::local_envvar(DSPRRR_CACHE_ENABLED = "false")

  config <- dsprrr:::get_cache_config()
  expect_false(config$enable)
})

test_that("DSPRRR_CACHE_ENABLED=0 disables cache", {
  local_reset_cache()

  # Reset config to force re-read of env var
  pkg_env <- asNamespace("dsprrr")$.dsprrr_env
  pkg_env$cache_config <- NULL

  withr::local_envvar(DSPRRR_CACHE_ENABLED = "0")

  config <- dsprrr:::get_cache_config()
  expect_false(config$enable)
})

test_that("DSPRRR_CACHE_ENABLED unset enables cache by default", {
  local_reset_cache()

  # Reset config to force re-read of env var
  pkg_env <- asNamespace("dsprrr")$.dsprrr_env
  pkg_env$cache_config <- NULL

  # Ensure env var is not set
  withr::local_envvar(DSPRRR_CACHE_ENABLED = NA)

  config <- dsprrr:::get_cache_config()
  expect_true(config$enable)
})

test_that("cached_chat_structured uses cache for repeated calls", {
  local_reset_cache()

  configure_cache(enable_memory = TRUE, enable_disk = FALSE)
  clear_cache()

  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  local_cache_openai_backend(calls)
  base <- cache_real_chat()
  miss_chat <- base$clone(deep = TRUE)
  hit_chat <- base$clone(deep = TRUE)

  output_type <- ellmer::type_object(answer = ellmer::type_string())

  # First call - cache miss
  result1 <- dsprrr:::cached_chat_structured(
    llm = miss_chat,
    prompt = "test prompt",
    output_type = output_type
  )

  expect_equal(calls$n, 1L)
  expect_equal(result1$answer, "ok")

  # Second call with same params - cache hit
  result2 <- dsprrr:::cached_chat_structured(
    llm = hit_chat,
    prompt = "test prompt",
    output_type = output_type
  )

  # Should NOT have called LLM again

  expect_equal(calls$n, 1L)
  expect_equal(result2$answer, "ok")

  # Verify cache stats
  stats <- cache_stats()
  expect_equal(stats$hits, 1L)
  expect_equal(stats$misses, 1L)
})

test_that("real structured Chat branches replay ContentJson equivalently", {
  local_reset_cache()
  configure_cache(enable_memory = TRUE, enable_disk = FALSE)

  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  local_cache_openai_backend(calls)
  base <- cache_real_chat()
  first <- base$clone(deep = TRUE)
  second <- base$clone(deep = TRUE)
  output_type <- ellmer::type_object(answer = ellmer::type_string())
  initial_fingerprint <- dsprrr:::cache_request_fingerprint(
    first,
    "prompt",
    output_type
  )
  initial_key <- dsprrr:::cache_key(
    "prompt",
    "model-a",
    output_type = output_type,
    fingerprint = initial_fingerprint
  )

  dsprrr:::cached_chat_structured(first, "prompt", output_type)
  envelope <- dsprrr:::get_cache()$get(initial_key)
  expect_true(dsprrr:::is_cache_envelope(envelope))
  expect_identical(envelope$version, dsprrr:::cache_envelope_schema_version())
  expect_length(envelope$turn_delta$turns, 2)
  dsprrr:::cached_chat_structured(second, "prompt", output_type)
  expect_equal(calls$n, 1L)
  expect_length(second$get_turns(), 2)

  miss_next <- dsprrr:::cache_request_fingerprint(
    first,
    "next prompt",
    output_type
  )
  hit_next <- dsprrr:::cache_request_fingerprint(
    second,
    "next prompt",
    output_type
  )
  expect_identical(
    dsprrr:::cache_fingerprint_json(miss_next),
    dsprrr:::cache_fingerprint_json(hit_next)
  )
  miss_assistant <- first$get_turns()[[2]]
  hit_assistant <- second$get_turns()[[2]]
  expect_identical(hit_assistant@contents, miss_assistant@contents)
  expect_s3_class(hit_assistant@contents[[1]], "ellmer::ContentJson")
  expect_identical(hit_assistant@contents[[1]]@data, list(answer = "ok"))
  expect_null(hit_assistant@contents[[1]]@string)
  expect_identical(
    hit_assistant@contents[[1]]@parsed,
    miss_assistant@contents[[1]]@parsed
  )
  expect_identical(hit_assistant@json, miss_assistant@json)
  expect_identical(hit_assistant@finish_reason, miss_assistant@finish_reason)
  expect_true(all(is.na(hit_assistant@tokens)))
  expect_true(is.na(hit_assistant@cost))
  expect_true(is.na(hit_assistant@duration))

  dsprrr:::cached_chat_structured(second, "prompt", output_type)
  expect_equal(calls$n, 2L)
  expect_length(second$get_turns(), 4)
})

test_that("ContentJson data and string forms fingerprint and replay exactly", {
  constructor <- getFromNamespace("ContentJson", "ellmer")
  contents <- list(
    constructor(data = list(answer = "ok", score = 1L)),
    constructor(string = '{"answer":"ok","score":1}')
  )

  for (content in contents) {
    replayed <- dsprrr:::cache_replay_content(content)
    expect_s3_class(replayed, "ellmer::ContentJson")
    expect_identical(replayed@data, content@data)
    expect_identical(replayed@string, content@string)
    expect_identical(replayed@parsed, content@parsed)
    expect_identical(
      dsprrr:::cache_content_fingerprint(replayed),
      dsprrr:::cache_content_fingerprint(content)
    )
  }
})

test_that("opaque Chats with unavailable state inspection never cache", {
  local_reset_cache()
  configure_cache(enable_memory = TRUE, enable_disk = FALSE)

  output_type <- ellmer::type_object(answer = ellmer::type_string())
  calls <- 0L
  opaque <- structure(
    list(
      chat_structured = function(...) {
        calls <<- calls + 1L
        list(answer = paste0("fresh-", calls))
      }
    ),
    class = "Chat"
  )

  expect_no_warning(
    first <- dsprrr:::cached_chat_structured(
      opaque,
      "prompt",
      output_type
    )
  )
  second <- suppressWarnings(
    dsprrr:::cached_chat_structured(opaque, "prompt", output_type)
  )

  expect_identical(first$answer, "fresh-1")
  expect_identical(second$answer, "fresh-2")
  expect_equal(calls, 2L)
  expect_equal(cache_stats()$hits, 0L)
  expect_equal(cache_stats()$misses, 0L)
  expect_equal(dsprrr:::get_cache()$size(), 0L)
})

test_that("untrusted destructive setters are never invoked during replay", {
  provider <- suppressWarnings(
    ellmer::chat_openai(api_key = "dummy-key", model = "model-a")$get_provider()
  )
  state <- new.env(parent = emptyenv())
  state$turns <- list(
    ellmer::UserTurn(contents = list(ellmer::ContentText("baseline")))
  )
  state$sets <- 0L
  state$calls <- 0L
  unsafe <- structure(
    list(
      get_provider = function() provider,
      get_model = function() provider@model,
      get_tools = function() list(),
      get_system_prompt = function() NULL,
      get_turns = function(...) state$turns,
      set_turns = function(turns) {
        state$sets <- state$sets + 1L
        state$turns <- list()
        stop("cleared then failed")
      },
      chat_structured = function(...) {
        state$calls <- state$calls + 1L
        list(answer = "unexpected")
      }
    ),
    class = "Chat"
  )
  before <- serialize(state$turns, connection = NULL, version = 3)
  delta <- list(
    mode = "turns",
    turns = list(
      ellmer::UserTurn(contents = list(ellmer::ContentText("cached")))
    )
  )

  expect_false(
    dsprrr:::cache_replay_turn_delta(unsafe, delta)
  )
  expect_identical(
    serialize(state$turns, connection = NULL, version = 3),
    before
  )
  expect_equal(state$sets, 0L)
  expect_equal(state$calls, 0L)
})

test_that("cached_chat_structured bypasses cache when disabled", {
  local_reset_cache()

  configure_cache(enable = FALSE)

  # Create a mock LLM that tracks calls
  call_count <- 0
  mock_llm <- list(
    get_model = function() "mock-model",
    chat_structured = function(prompt, type, echo = "none") {
      call_count <<- call_count + 1
      list(answer = paste("response", call_count))
    }
  )
  class(mock_llm) <- "Chat"

  output_type <- ellmer::type_object(answer = ellmer::type_string())

  # First call
  result1 <- dsprrr:::cached_chat_structured(
    llm = mock_llm,
    prompt = "test prompt",
    output_type = output_type
  )

  expect_equal(call_count, 1)

  # Second call - should call LLM again (no caching)
  result2 <- dsprrr:::cached_chat_structured(
    llm = mock_llm,
    prompt = "test prompt",
    output_type = output_type
  )

  expect_equal(call_count, 2)
  expect_equal(result2$answer, "response 2")
})

# Disk cache integration tests

test_that("disk cache persists data across cache resets", {
  local_reset_cache()

  tmp_dir <- tempfile("cache_test_")
  withr::defer(unlink(tmp_dir, recursive = TRUE))

  # Configure with only disk cache
  configure_cache(
    enable_memory = FALSE,
    enable_disk = TRUE,
    disk_path = tmp_dir
  )

  cache <- dsprrr:::get_cache()
  expect_false(is.null(cache))

  # Store a value
  cache$set("persist_key", list(answer = "persisted value"))

  # Verify it's there
  result <- cache$get("persist_key")
  expect_false(cachem::is.key_missing(result))
  expect_equal(result$answer, "persisted value")

  # Reset memory references (simulating session refresh)
  pkg_env <- asNamespace("dsprrr")$.dsprrr_env
  pkg_env$cache <- NULL
  pkg_env$cache_disk <- NULL

  # Reconfigure with same disk path
  configure_cache(
    enable_memory = FALSE,
    enable_disk = TRUE,
    disk_path = tmp_dir
  )

  # Get new cache handle
  cache2 <- dsprrr:::get_cache()

  # Data should still be there

  result2 <- cache2$get("persist_key")
  expect_false(cachem::is.key_missing(result2))
  expect_equal(result2$answer, "persisted value")
})

test_that("layered cache uses both memory and disk tiers", {
  local_reset_cache()

  tmp_dir <- tempfile("cache_test_layered_")
  withr::defer(unlink(tmp_dir, recursive = TRUE))

  # Configure with both tiers
  configure_cache(enable_memory = TRUE, enable_disk = TRUE, disk_path = tmp_dir)

  cache <- dsprrr:::get_cache()
  expect_false(is.null(cache))

  # Store a value
  cache$set("layered_key", list(answer = "layered value"))

  # Should be in memory cache
  pkg_env <- asNamespace("dsprrr")$.dsprrr_env
  expect_false(is.null(pkg_env$cache_memory))
  mem_result <- pkg_env$cache_memory$get("layered_key")
  expect_false(cachem::is.key_missing(mem_result))

  # Should also be in disk cache
  expect_false(is.null(pkg_env$cache_disk))
  disk_result <- pkg_env$cache_disk$get("layered_key")
  expect_false(cachem::is.key_missing(disk_result))
})

test_that("disk cache creates directory when it doesn't exist", {
  local_reset_cache()

  # Use a nested path that doesn't exist
  tmp_dir <- file.path(tempdir(), "dsprrr_test", "nested", "cache")
  withr::defer(unlink(file.path(tempdir(), "dsprrr_test"), recursive = TRUE))

  # Directory should not exist yet
  expect_false(dir.exists(tmp_dir))

  # Configure disk cache - should create directory
  configure_cache(
    enable_memory = FALSE,
    enable_disk = TRUE,
    disk_path = tmp_dir
  )

  cache <- dsprrr:::get_cache()

  # If we got a cache, directory should exist now
  if (!is.null(cache)) {
    expect_true(dir.exists(tmp_dir))
  }
})

test_that("disk cache handles race condition when directory created by another process", {
  local_reset_cache()

  # Create a directory that will trigger the "already exists" warning
  # by creating it between the dir.exists() check and the dir.create() call
  tmp_dir <- file.path(tempdir(), "dsprrr_race_test", "cache")
  withr::defer(unlink(
    file.path(tempdir(), "dsprrr_race_test"),
    recursive = TRUE
  ))

  # Pre-create the directory to simulate race condition
  # When configure_cache runs, it will see dir doesn't exist, then
  # dir.create will warn "already exists" - this should NOT disable disk caching
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

  # Now configure cache with that path
  # The internal logic checks !dir.exists() first, but we create the dir
  # to simulate the race. We need to manipulate this more carefully.

  # Actually, to properly test this, we need to:
  # 1. Have the dir not exist when check happens
  # 2. Have it exist when dir.create runs (creating the warning)
  # This is hard to simulate deterministically, so instead we'll verify
  # that if dir.create warns but directory exists, cache still works.

  # Reset and configure - the dir already exists, so no race happens in
  # this simple test. But we can at least verify the cache works with
  # an existing directory (which is the end state of a race).
  unlink(tmp_dir, recursive = TRUE)

  # Configure disk cache - should create directory normally
  configure_cache(
    enable_memory = FALSE,
    enable_disk = TRUE,
    disk_path = tmp_dir
  )

  cache <- dsprrr:::get_cache()

  # Cache should be available and directory should exist
  expect_false(is.null(cache))
  expect_true(dir.exists(tmp_dir))

  # Verify we can use the cache
  cache$set("race_test_key", "race_test_value")
  result <- cache$get("race_test_key")
  expect_false(cachem::is.key_missing(result))
  expect_equal(result, "race_test_value")
})

test_that("different mock LLMs don't share cache entries", {
  local_reset_cache()

  configure_cache(enable_memory = TRUE, enable_disk = FALSE)
  clear_cache()

  # Create two mock LLMs with same "unknown" model behavior
  mock_llm1 <- list(
    get_model = function() stop("no model"),
    chat_structured = function(prompt, type, echo = "none") {
      list(answer = "response from LLM 1")
    }
  )
  class(mock_llm1) <- "Chat"

  mock_llm2 <- list(
    get_model = function() stop("no model"),
    chat_structured = function(prompt, type, echo = "none") {
      list(answer = "response from LLM 2")
    }
  )
  class(mock_llm2) <- "Chat"

  output_type <- ellmer::type_object(answer = ellmer::type_string())

  # Call with first LLM (expect warning about model extraction)
  result1 <- suppressWarnings(
    dsprrr:::cached_chat_structured(
      llm = mock_llm1,
      prompt = "test prompt",
      output_type = output_type
    )
  )

  expect_equal(result1$answer, "response from LLM 1")

  # Call with second LLM - should NOT get cached result from first
  result2 <- suppressWarnings(
    dsprrr:::cached_chat_structured(
      llm = mock_llm2,
      prompt = "test prompt",
      output_type = output_type
    )
  )

  # Different LLM objects should not share cache
  expect_equal(result2$answer, "response from LLM 2")
})

# New cache ergonomics features tests

test_that("first cache hit shows informative message", {
  local_reset_cache()

  configure_cache(enable_memory = TRUE, enable_disk = FALSE)
  clear_cache()

  # Reset the first-hit flag
  pkg_env <- asNamespace("dsprrr")$.dsprrr_env
  pkg_env$cache_first_hit_shown <- FALSE

  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  local_cache_openai_backend(calls)
  base <- cache_real_chat()
  chats <- lapply(seq_len(3), function(i) base$clone(deep = TRUE))

  output_type <- ellmer::type_object(answer = ellmer::type_string())

  # First call - cache miss
  dsprrr:::cached_chat_structured(
    llm = chats[[1]],
    prompt = "test prompt",
    output_type = output_type
  )

  # Second call - cache hit, should show message
  expect_message(
    dsprrr:::cached_chat_structured(
      llm = chats[[2]],
      prompt = "test prompt",
      output_type = output_type
    ),
    "Using cached LLM responses"
  )

  # Flag should now be set
  expect_true(pkg_env$cache_first_hit_shown)

  # Third call - should NOT show message again
  expect_no_message(
    dsprrr:::cached_chat_structured(
      llm = chats[[3]],
      prompt = "test prompt",
      output_type = output_type
    )
  )
})

test_that("clear_cache resets first-hit flag", {
  local_reset_cache()

  configure_cache(enable_memory = TRUE, enable_disk = FALSE)
  clear_cache()

  # Set the flag manually
  pkg_env <- asNamespace("dsprrr")$.dsprrr_env
  pkg_env$cache_first_hit_shown <- TRUE

  # Clear cache should reset it
  clear_cache()

  expect_false(isTRUE(pkg_env$cache_first_hit_shown))
})

test_that(".cache = FALSE bypasses cache for single call", {
  local_reset_cache()

  configure_cache(enable_memory = TRUE, enable_disk = FALSE)
  clear_cache()

  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  local_cache_openai_backend(
    calls,
    response = function(n) list(answer = paste("response", n))
  )
  base <- cache_real_chat()
  chats <- lapply(seq_len(3), function(i) base$clone(deep = TRUE))

  output_type <- ellmer::type_object(answer = ellmer::type_string())

  # First call - cache miss
  result1 <- dsprrr:::cached_chat_structured(
    llm = chats[[1]],
    prompt = "test prompt",
    output_type = output_type
  )

  expect_equal(calls$n, 1L)
  expect_equal(result1$answer, "response 1")

  # Second call with .cache = FALSE - should bypass cache
  result2 <- suppressMessages(
    dsprrr:::cached_chat_structured(
      llm = chats[[2]],
      prompt = "test prompt",
      output_type = output_type,
      .cache = FALSE
    )
  )

  # Should have called LLM again
  expect_equal(calls$n, 2L)
  expect_equal(result2$answer, "response 2")

  # Third call without .cache (uses global config) - cache hit
  result3 <- suppressMessages(
    dsprrr:::cached_chat_structured(
      llm = chats[[3]],
      prompt = "test prompt",
      output_type = output_type
    )
  )

  # Should NOT have called LLM again (cache hit from first call)
  expect_equal(calls$n, 2L)
  expect_equal(result3$answer, "response 1")
})

test_that(".cache = TRUE has no effect when caching globally disabled", {
  local_reset_cache()

  configure_cache(enable = FALSE)

  # Create a mock LLM that tracks calls
  call_count <- 0
  mock_llm <- list(
    get_model = function() "mock-model",
    chat_structured = function(prompt, type, echo = "none") {
      call_count <<- call_count + 1
      list(answer = paste("response", call_count))
    },
    `.__enclos_env__` = list(private = list(api_args = list(temperature = 0.7)))
  )
  class(mock_llm) <- "Chat"

  output_type <- ellmer::type_object(answer = ellmer::type_string())

  # First call with .cache = TRUE
  # With caching globally disabled, the cache object itself is NULL
  # So .cache = TRUE won't actually cache - it will still make the call
  result1 <- dsprrr:::cached_chat_structured(
    llm = mock_llm,
    prompt = "test prompt",
    output_type = output_type,
    .cache = TRUE
  )

  expect_equal(call_count, 1)

  # Second call with same prompt and .cache = TRUE
  # Should make a new call since cache is globally disabled
  result2 <- dsprrr:::cached_chat_structured(
    llm = mock_llm,
    prompt = "test prompt",
    output_type = output_type,
    .cache = TRUE
  )

  # Verify no caching occurred - second call was made
  expect_equal(call_count, 2)
})

# Integration tests for .cache parameter through run() API
test_that("run() respects .cache = FALSE parameter (single input)", {
  local_reset_cache()
  # Use memory-only cache to avoid disk pollution from previous runs
  configure_cache(enable = TRUE, enable_memory = TRUE, enable_disk = FALSE)

  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  local_cache_openai_backend(calls, list(sentiment = "positive"))
  base <- cache_real_chat()
  chats <- lapply(seq_len(4), function(i) base$clone(deep = TRUE))

  sig <- signature("text -> sentiment: enum('positive', 'negative', 'neutral')")
  mod <- module(sig, type = "predict")

  # First call - cache miss
  result1 <- run(mod, text = "Great!", .llm = chats[[1]])
  expect_equal(calls$n, 1L)

  # Second call with same input - should hit cache
  result2 <- run(mod, text = "Great!", .llm = chats[[2]])
  expect_equal(calls$n, 1L) # No new call

  # Third call with .cache = FALSE - should bypass cache
  result3 <- run(mod, text = "Great!", .llm = chats[[3]], .cache = FALSE)
  expect_equal(calls$n, 2L) # New call made

  # Fourth call with .cache = TRUE - should use cache
  result4 <- run(mod, text = "Great!", .llm = chats[[4]], .cache = TRUE)
  expect_equal(calls$n, 2L) # No new call
})

# --- Cache-hit semantic turn replay tests -------------------------------------

test_that("cache hit replays semantic turns into the Chat object", {
  local_reset_cache()
  configure_cache(enable = TRUE, enable_memory = TRUE, enable_disk = FALSE)

  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  local_cache_openai_backend(calls, list(sentiment = "positive"))
  base <- cache_real_chat()
  miss_chat <- base$clone(deep = TRUE)
  hit_chat <- base$clone(deep = TRUE)

  sig <- signature("text -> sentiment: enum('positive', 'negative', 'neutral')")
  mod <- module(sig, type = "predict")

  invisible(run(mod, text = "Great!", .llm = miss_chat))
  expect_length(miss_chat$get_turns(), 2)
  expect_equal(calls$n, 1L)

  mod$state$traces <- list()
  invisible(run(mod, text = "Great!", .llm = hit_chat))
  turns <- hit_chat$get_turns()
  expect_length(turns, 2)
  expect_equal(calls$n, 1L)

  last_user <- turns[[1]]
  last_assistant <- turns[[2]]
  expect_s3_class(last_user, "ellmer::UserTurn")
  expect_s3_class(last_assistant, "ellmer::AssistantTurn")
  expect_match(last_user@contents[[1]]@text, "Great!")
  expect_s3_class(last_assistant@contents[[1]], "ellmer::ContentJson")
  expect_identical(
    last_assistant@contents[[1]]@parsed,
    list(sentiment = "positive")
  )

  trace <- mod$state$traces[[1]]
  expect_s3_class(trace$user_turn, "ellmer::UserTurn")
  expect_s3_class(trace$assistant_turn, "ellmer::AssistantTurn")
})

test_that("different pre-existing history misses cache and is preserved", {
  local_reset_cache()
  configure_cache(enable = TRUE, enable_memory = TRUE, enable_disk = FALSE)

  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  local_cache_openai_backend(calls, list(sentiment = "positive"))
  base <- cache_real_chat()

  sig <- signature("text -> sentiment: enum('positive', 'negative', 'neutral')")
  mod <- module(sig, type = "predict")

  llm_warm <- base$clone(deep = TRUE)
  invisible(run(mod, text = "Great!", .llm = llm_warm))
  expect_equal(calls$n, 1L)

  # Use a different chat with stale pre-existing history
  old_user <- ellmer::UserTurn(
    contents = list(ellmer::ContentText("OLD PROMPT"))
  )
  old_assistant <- ellmer::AssistantTurn(
    contents = list(ellmer::ContentText("OLD RESPONSE"))
  )
  llm_with_history <- base$clone(deep = TRUE)
  llm_with_history$set_turns(list(old_user, old_assistant))

  mod$state$traces <- list()
  invisible(run(mod, text = "Great!", .llm = llm_with_history))
  expect_equal(calls$n, 2L)

  # Pre-existing turns are part of the identity and remain in the continuation.
  expect_equal(length(llm_with_history$get_turns()), 4)

  # Trace points to the new provider turns, not the old ones
  trace <- mod$state$traces[[1]]
  expect_match(trace$user_turn@contents[[1]]@text, "Great!")
  expect_false(
    identical(trace$user_turn@contents[[1]]@text, "OLD PROMPT")
  )
  expect_identical(
    trace$assistant_turn@contents[[1]]@parsed,
    list(sentiment = "positive")
  )
})

test_that("Chats without state getters execute but never cache", {
  local_reset_cache()
  configure_cache(enable = TRUE, enable_memory = TRUE, enable_disk = FALSE)

  calls <- 0L
  mock_env <- new.env()
  mock_env$mock_llm <- structure(
    list(
      get_model = function() "mock-model",
      chat_structured = function(prompt, type, echo = "none") {
        calls <<- calls + 1L
        list(sentiment = "positive")
      },
      `.__enclos_env__` = list(
        private = list(api_args = list(temperature = 0.7))
      ),
      clone = function(...) mock_env$mock_llm
    ),
    class = "Chat"
  )

  sig <- signature("text -> sentiment: enum('positive', 'negative', 'neutral')")
  mod <- module(sig, type = "predict")

  suppressWarnings(run(mod, text = "Great!", .llm = mock_env$mock_llm))
  suppressWarnings(run(mod, text = "Great!", .llm = mock_env$mock_llm))
  expect_equal(calls, 2L)
  expect_equal(cache_stats()$hits, 0L)
  expect_equal(cache_stats()$misses, 0L)
})

test_that("replayed turn tokens/cost/duration are NA (not 0)", {
  local_reset_cache()
  configure_cache(enable = TRUE, enable_memory = TRUE, enable_disk = FALSE)

  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  local_cache_openai_backend(calls, list(answer = "42"))
  base <- cache_real_chat()
  miss_chat <- base$clone(deep = TRUE)
  hit_chat <- base$clone(deep = TRUE)

  sig <- signature("question -> answer: string")
  mod <- module(sig, type = "predict")

  # Cache miss — real call
  invisible(run(mod, question = "What?", .llm = miss_chat))

  mod$state$traces <- list()
  invisible(run(mod, question = "What?", .llm = hit_chat))

  replayed_assistant <- hit_chat$get_turns()[[2]]
  expect_true(all(is.na(replayed_assistant@tokens)))
  expect_true(is.na(replayed_assistant@cost))
  expect_true(is.na(replayed_assistant@duration))
})

test_that("run() respects .cache = FALSE in batch processing", {
  local_reset_cache()
  # Use memory-only cache to avoid disk pollution from previous runs
  configure_cache(enable = TRUE, enable_memory = TRUE, enable_disk = FALSE)

  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  local_cache_openai_backend(calls, list(sentiment = "positive"))
  chat <- cache_real_chat()

  sig <- signature("text -> sentiment: enum('positive', 'negative', 'neutral')")
  mod <- module(sig, type = "predict")

  # First batch call - cache miss for both
  results1 <- run(
    mod,
    text = c("Great!", "Awesome!"),
    .llm = chat,
    .progress = FALSE
  )
  expect_equal(calls$n, 2L)

  # Second batch call with same inputs - should hit cache
  results2 <- run(
    mod,
    text = c("Great!", "Awesome!"),
    .llm = chat,
    .progress = FALSE
  )
  expect_equal(calls$n, 2L) # No new calls

  # Third batch call with .cache = FALSE - should bypass cache
  results3 <- run(
    mod,
    text = c("Great!", "Awesome!"),
    .llm = chat,
    .cache = FALSE,
    .progress = FALSE
  )
  expect_equal(calls$n, 4L) # Two new calls
})

test_that("run() validates invalid .cache parameter values", {
  sig <- signature("text -> answer")
  mod <- module(sig, type = "predict")

  # Invalid: string instead of logical
  expect_error(
    run(mod, text = "test", .cache = "false"),
    class = "rlang_error"
  )

  # Invalid: NA value
  expect_error(
    run(mod, text = "test", .cache = NA),
    class = "rlang_error"
  )

  # Invalid: numeric instead of logical
  expect_error(
    run(mod, text = "test", .cache = 1),
    class = "rlang_error"
  )

  # Invalid: vector of length > 1
  expect_error(
    run(mod, text = "test", .cache = c(TRUE, FALSE)),
    class = "rlang_error"
  )
})

test_that("dsprrr_sitrep shows cache configuration", {
  local_reset_cache()

  configure_cache(enable = TRUE)

  # Should not error and output cache section
  result <- expect_no_error(dsprrr_sitrep())

  # Should return invisibly a list with cache info
  expect_type(result, "list")
  expect_true("cache_enabled" %in% names(result))
  expect_true(result$cache_enabled)

  # Should have cache_stats if there's cache activity
  if (!is.null(result$cache_stats)) {
    expect_s3_class(result$cache_stats, "dsprrr_cache_stats")
  }
})

test_that("dsprrr_sitrep shows disabled cache", {
  local_reset_cache()

  configure_cache(enable = FALSE)

  result <- expect_no_error(suppressMessages(dsprrr_sitrep()))

  expect_false(result$cache_enabled)
})

test_that(".cache is accepted and validated for non-Predict modules (dsprrr-jup)", {
  # Regression: only run.PredictModule declared .cache; for every other module
  # type and pipelines it fell into ... -> treated as an unknown signature input
  # (spurious warning) and dropped.
  mod <- module_fn("text -> answer", function(text) paste("Echo:", text))

  res <- expect_no_warning(run(mod, text = "hi", .cache = FALSE))
  expect_equal(res$answer, "Echo: hi")

  # Invalid .cache values are rejected the same way as for PredictModule.
  expect_error(run(mod, text = "hi", .cache = "yes"), "must be")
  expect_error(run(mod, text = "hi", .cache = NA), "must be")
})

test_that("rollout_id threads from forward() into the cache key (dsprrr-pcd)", {
  # Regression: rollout_id was implemented in the cache layer but no caller
  # passed it, so BestOfN/Refine retries with caching enabled were served
  # identical cached responses (effective N = 1).
  local_reset_cache()
  configure_cache(enable = TRUE, enable_memory = TRUE, enable_disk = FALSE)

  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  local_cache_openai_backend(
    calls,
    response = function(n) list(wrapper = paste0("resp-", n))
  )
  base <- cache_real_chat()
  chats <- lapply(seq_len(3), function(i) base$clone(deep = TRUE))
  sig <- Signature(
    inputs = list(input(name = "q", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = ""
  )
  mod <- module(signature = sig, type = "predict", template = "{q}")

  # Same prompt, different rollout_id -> both miss the cache -> 2 real calls.
  mod$forward(list(q = "x"), .llm = chats[[1]], rollout_id = 1)
  mod$forward(list(q = "x"), .llm = chats[[2]], rollout_id = 2)
  expect_equal(calls$n, 2L)

  # Repeating a rollout_id hits the cache -> no new call.
  mod$forward(list(q = "x"), .llm = chats[[3]], rollout_id = 1)
  expect_equal(calls$n, 2L)
})
