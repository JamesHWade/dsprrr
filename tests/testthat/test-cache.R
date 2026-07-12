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

cache_test_mode <- function(path) {
  sprintf("%04o", dsprrr:::cache_path_mode(path))
}

test_that("configure_cache sets default values", {
  local_reset_cache()

  configure_cache()

  config <- dsprrr:::get_cache_config()
  expect_true(config$enable)
  expect_true(config$enable_memory)
  expect_true(config$enable_disk)
  expect_true(config$disk_private)
  expect_equal(config$memory_max_entries, 1000L)
})

test_that("default disk path is per-user and preserves its override", {
  cache_root <- tempfile("dsprrr-user-cache-")
  withr::local_envvar(c(
    DSPRRR_CACHE_PATH = NA,
    R_USER_CACHE_DIR = cache_root
  ))

  expect_identical(
    dsprrr:::default_disk_cache_path(),
    file.path(cache_root, "R", "dsprrr")
  )

  override <- tempfile("dsprrr-cache-override-")
  withr::local_envvar(DSPRRR_CACHE_PATH = override)
  expect_identical(dsprrr:::default_disk_cache_path(), override)

  withr::local_envvar(DSPRRR_CACHE_PATH = "")
  expect_identical(
    dsprrr:::default_disk_cache_path(),
    file.path(cache_root, "R", "dsprrr")
  )
})

test_that("configure_cache validates its privacy mode", {
  local_reset_cache()

  expect_snapshot(error = TRUE, configure_cache(disk_private = NA))
  expect_snapshot(error = TRUE, configure_cache(disk_private = "yes"))
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
  rebuilt <- dsprrr:::get_cache()
  expect_s3_class(rebuilt, "cache_mem")
  rebuilt$set("fresh_key", "fresh_value")
  expect_identical(rebuilt$get("fresh_key"), "fresh_value")
  expect_true(dsprrr:::get_cache_config()$enable_memory)
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

test_that("private disk caches preserve parents and use owner-only modes", {
  skip_on_os("windows")
  local_reset_cache()

  parent <- tempfile("cache_private_parent_")
  disk_path <- file.path(parent, "cache")
  withr::defer(unlink(parent, recursive = TRUE))
  dir.create(parent)
  Sys.chmod(parent, mode = "0755", use_umask = FALSE)
  original_umask <- Sys.umask()

  configure_cache(
    enable_memory = FALSE,
    enable_disk = TRUE,
    disk_path = disk_path
  )
  cache <- dsprrr:::get_cache()
  cache$set("secure_key", list(answer = "first"))
  cache$set("secure_key", list(answer = "second"))

  expect_identical(cache_test_mode(parent), "0755")
  expect_identical(cache_test_mode(disk_path), "0700")
  expect_identical(
    cache_test_mode(file.path(disk_path, "secure_key.rds")),
    "0600"
  )
  expect_identical(cache$get("secure_key")$answer, "second")
  expect_length(list.files(disk_path, pattern = "-temp-"), 0L)
  expect_identical(Sys.umask(), original_umask)
  expect_identical(
    asNamespace("dsprrr")$.dsprrr_env$cache_privacy_status,
    "verified_posix_modes"
  )
  expect_match(
    asNamespace("dsprrr")$.dsprrr_env$cache_privacy_reason,
    "extended ACLs were not checked"
  )
})

test_that("live private path replacement never returns or rewrites poison", {
  skip_on_os("windows")
  local_reset_cache()

  root <- tempfile("cache_live_replacement_")
  disk_path <- file.path(root, "cache")
  displaced <- file.path(root, "audited-cache")
  withr::defer(unlink(root, recursive = TRUE))
  dir.create(root)

  configure_cache(
    enable_memory = TRUE,
    enable_disk = TRUE,
    disk_path = disk_path
  )
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  local_cache_openai_backend(
    calls,
    response = function(n) list(answer = paste0("provider-", n))
  )
  base <- cache_real_chat()
  mod <- module(signature("text -> answer"), type = "predict")

  first <- run(
    mod,
    text = "same",
    .llm = base$clone(deep = TRUE),
    .return_format = "structured"
  )
  expect_identical(first$output$answer, "provider-1")
  response_file <- list.files(disk_path, full.names = TRUE)
  expect_length(response_file, 1L)
  poisoned <- readRDS(response_file)
  poisoned$result$answer <- "POISONED"

  # Force the next request through the retained disk handle while keeping
  # memory enabled in configuration for post-degradation recovery.
  clear_cache("memory")

  expect_true(file.rename(disk_path, displaced))
  dir.create(disk_path)
  Sys.chmod(disk_path, mode = "0777", use_umask = FALSE)
  poisoned_file <- file.path(disk_path, basename(response_file))
  saveRDS(poisoned, poisoned_file)
  Sys.chmod(poisoned_file, mode = "0666", use_umask = FALSE)

  expect_warning(
    second <- run(
      mod,
      text = "same",
      .llm = base$clone(deep = TRUE),
      .return_format = "structured"
    ),
    class = "dsprrr_cache_security_warning"
  )
  expect_identical(second$output$answer, "provider-2")
  expect_identical(second$metadata$cache, "bypass")
  expect_identical(calls$n, 2L)
  expect_identical(readRDS(poisoned_file)$result$answer, "POISONED")

  pkg_env <- asNamespace("dsprrr")$.dsprrr_env
  expect_true(pkg_env$cache_degraded)
  expect_null(pkg_env$cache_disk)
  expect_null(pkg_env$cache_disk_guard)
  expect_identical(pkg_env$cache_privacy_status, "degraded")

  third <- run(
    mod,
    text = "same",
    .llm = base$clone(deep = TRUE),
    .return_format = "structured"
  )
  fourth <- run(
    mod,
    text = "same",
    .llm = base$clone(deep = TRUE),
    .return_format = "structured"
  )
  expect_identical(third$output$answer, "provider-3")
  expect_identical(third$metadata$cache, "miss")
  expect_identical(fourth$output$answer, "provider-3")
  expect_identical(fourth$metadata$cache, "hit")
  expect_identical(calls$n, 3L)
  expect_identical(readRDS(poisoned_file)$result$answer, "POISONED")
})

test_that("warn-as-error cannot strand an invalid global disk guard", {
  skip_on_os("windows")
  local_reset_cache()

  root <- tempfile("cache_warn_error_cleanup_")
  disk_path <- file.path(root, "cache")
  displaced <- file.path(root, "audited-cache")
  withr::defer(unlink(root, recursive = TRUE))
  dir.create(root)
  configure_cache(
    enable_memory = TRUE,
    enable_disk = TRUE,
    disk_path = disk_path
  )
  layered <- dsprrr:::get_cache()
  layered$set("safe", list(answer = "safe"))
  pkg_env <- asNamespace("dsprrr")$.dsprrr_env
  stale_disk <- pkg_env$cache_disk

  clear_cache("memory")
  expect_true(file.rename(disk_path, displaced))
  dir.create(disk_path)
  Sys.chmod(disk_path, mode = "0777", use_umask = FALSE)
  poisoned_file <- file.path(disk_path, "safe.rds")
  saveRDS(list(answer = "POISONED"), poisoned_file)
  Sys.chmod(poisoned_file, mode = "0666", use_umask = FALSE)

  withr::local_options(warn = 2)
  error <- tryCatch(stale_disk$get("safe"), error = identity)
  expect_s3_class(error, "error")
  expect_match(conditionMessage(error), "Disk caching is unavailable")
  expect_null(pkg_env$cache_disk_guard)
  expect_null(pkg_env$cache_disk)
  expect_true(pkg_env$cache_degraded)

  expect_true(cachem::is.key_missing(stale_disk$get("safe")))
  expect_identical(stale_disk$set("fresh", list(answer = "fresh")), FALSE)
  expect_false(file.exists(file.path(disk_path, "fresh.rds")))
  expect_identical(readRDS(poisoned_file)$answer, "POISONED")

  memory <- dsprrr:::get_cache()
  expect_s3_class(memory, "cache_mem")
  memory$set("memory", list(answer = "usable"))
  expect_identical(memory$get("memory")$answer, "usable")
})

test_that("private audits require verifiable effective ownership", {
  skip_on_os("windows")
  local_reset_cache()

  disk_path <- tempfile("cache_owner_check_")
  withr::defer(unlink(disk_path, recursive = TRUE))
  dir.create(disk_path)
  Sys.chmod(disk_path, mode = "0700", use_umask = FALSE)
  effective_owner <- dsprrr:::cache_effective_owner_id()
  expect_false(is.na(effective_owner))
  expect_identical(
    dsprrr:::cache_path_owner_id(disk_path),
    effective_owner
  )

  audit <- testthat::with_mocked_bindings(
    dsprrr:::audit_private_cache_directory(disk_path),
    cache_effective_owner_id = function() effective_owner + 1L,
    .package = "dsprrr"
  )
  expect_false(audit$ok)
  expect_match(audit$reason, "not owned by the effective user")

  foreign <- file.path(disk_path, "foreign.rds")
  saveRDS(list(answer = "foreign"), foreign)
  Sys.chmod(foreign, mode = "0600", use_umask = FALSE)
  entry_audit <- testthat::with_mocked_bindings(
    dsprrr:::audit_private_cache_entries(disk_path),
    cache_path_owner_id = function(path) {
      if (identical(basename(path), "foreign.rds")) {
        effective_owner + 1L
      } else {
        effective_owner
      }
    },
    .package = "dsprrr"
  )
  expect_false(entry_audit$ok)
  expect_match(entry_audit$reason, "files not owned by the effective user")
})

test_that("private writes verify 0600 staging before serialization", {
  skip_on_os("windows")
  local_reset_cache()

  disk_path <- tempfile("cache_prewrite_mode_")
  withr::defer(unlink(disk_path, recursive = TRUE))
  observations <- list()
  testthat::local_mocked_bindings(
    cache_save_rds = function(value, file) {
      observations[[length(observations) + 1L]] <<- list(
        mode = cache_test_mode(file),
        owned = dsprrr:::cache_paths_owned_by_effective_user(file),
        size = file.info(file)$size[[1]]
      )
      base::saveRDS(value, file)
    },
    .package = "dsprrr"
  )
  configure_cache(
    enable_memory = FALSE,
    enable_disk = TRUE,
    disk_path = disk_path
  )
  cache <- dsprrr:::get_cache()
  cache$set("key", list(answer = "private"))

  expect_gte(length(observations), 2L)
  expect_true(all(vapply(
    observations,
    function(observation) identical(observation$mode, "0600"),
    logical(1)
  )))
  expect_true(all(vapply(observations, `[[`, logical(1), "owned")))
  expect_true(all(vapply(observations, `[[`, numeric(1), "size") == 0))
})

test_that("unsafe non-sticky parents fail and sticky parents remain usable", {
  skip_on_os("windows")
  local_reset_cache()

  root <- tempfile("cache_parent_safety_")
  withr::defer(unlink(root, recursive = TRUE))
  dir.create(root)
  Sys.chmod(root, mode = "0777", use_umask = FALSE)
  unsafe <- dsprrr:::prepare_cache_directory(
    file.path(root, "unsafe-cache"),
    private = TRUE
  )
  expect_false(unsafe$ok)
  expect_match(unsafe$reason, "non-sticky cache ancestor is writable")
  expect_false(dir.exists(file.path(root, "unsafe-cache")))

  Sys.chmod(root, mode = "1777", use_umask = FALSE)
  safe <- dsprrr:::prepare_cache_directory(
    file.path(root, "sticky-cache"),
    private = TRUE
  )
  expect_true(safe$ok)
  expect_identical(safe$trust$owner_id, dsprrr:::cache_effective_owner_id())
})

test_that("readable existing disk caches are repaired before reuse", {
  skip_on_os("windows")
  local_reset_cache()

  disk_path <- tempfile("cache_repair_")
  withr::defer(unlink(disk_path, recursive = TRUE))
  dir.create(disk_path)
  legacy_file <- file.path(disk_path, "legacy.rds")
  saveRDS(list(answer = "legacy"), legacy_file)
  Sys.chmod(disk_path, mode = "0755", use_umask = FALSE)
  Sys.chmod(legacy_file, mode = "0644", use_umask = FALSE)

  configure_cache(
    enable_memory = FALSE,
    enable_disk = TRUE,
    disk_path = disk_path
  )
  expect_warning(
    cache <- dsprrr:::get_cache(),
    class = "dsprrr_cache_permissions_repaired"
  )

  expect_identical(cache_test_mode(disk_path), "0700")
  expect_identical(cache_test_mode(legacy_file), "0600")
  expect_identical(cache$get("legacy")$answer, "legacy")

  configure_cache(
    enable_memory = FALSE,
    enable_disk = TRUE,
    disk_path = disk_path
  )
  expect_no_warning(dsprrr:::get_cache())
})

test_that("writable cache directories and files fail closed", {
  skip_on_os("windows")
  local_reset_cache()

  root <- tempfile("cache_untrusted_")
  withr::defer(unlink(root, recursive = TRUE))
  dir.create(root)

  writable_dir <- file.path(root, "writable-dir")
  dir.create(writable_dir)
  saveRDS(list(answer = "untrusted"), file.path(writable_dir, "key.rds"))
  Sys.chmod(writable_dir, mode = "0777", use_umask = FALSE)

  configure_cache(
    enable_memory = TRUE,
    enable_disk = TRUE,
    disk_path = writable_dir
  )
  expect_warning(
    cache <- dsprrr:::get_cache(),
    class = "dsprrr_cache_security_warning"
  )
  expect_s3_class(cache, "cache_mem")
  expect_null(asNamespace("dsprrr")$.dsprrr_env$cache_disk)
  expect_match(
    asNamespace("dsprrr")$.dsprrr_env$cache_degraded_reason,
    "writable by another local account"
  )

  writable_file <- file.path(root, "writable-file")
  dir.create(writable_file, mode = "0700")
  response_file <- file.path(writable_file, "key.rds")
  saveRDS(list(answer = "untrusted"), response_file)
  Sys.chmod(writable_file, mode = "0700", use_umask = FALSE)
  Sys.chmod(response_file, mode = "0666", use_umask = FALSE)

  configure_cache(
    enable_memory = FALSE,
    enable_disk = TRUE,
    disk_path = writable_file
  )
  warning_message <- NULL
  cache <- withCallingHandlers(
    dsprrr:::get_cache(),
    dsprrr_cache_security_warning = function(cnd) {
      warning_message <<- conditionMessage(cnd)
      invokeRestart("muffleWarning")
    }
  )
  expect_null(cache)
  expect_null(asNamespace("dsprrr")$.dsprrr_env$cache_disk)
  expect_match(warning_message, "No cache tier remains enabled")
})

test_that("owner-unreadable directories cannot hide writable RDS entries", {
  skip_on_os("windows")
  local_reset_cache()

  disk_path <- tempfile("cache_owner_unreadable_")
  withr::defer({
    Sys.chmod(disk_path, mode = "0700", use_umask = FALSE)
    unlink(disk_path, recursive = TRUE)
  })
  dir.create(disk_path)
  evil <- file.path(disk_path, "evil.rds")
  saveRDS(list(answer = "poisoned"), evil)
  Sys.chmod(evil, mode = "0666", use_umask = FALSE)
  Sys.chmod(disk_path, mode = "0300", use_umask = FALSE)

  cachem_initialized <- FALSE
  testthat::local_mocked_bindings(
    cache_disk = function(...) {
      cachem_initialized <<- TRUE
      stop("unsafe cache reached cachem")
    },
    .package = "cachem"
  )
  configure_cache(
    enable_memory = TRUE,
    enable_disk = TRUE,
    disk_path = disk_path
  )

  expect_warning(
    cache <- dsprrr:::get_cache(),
    class = "dsprrr_cache_security_warning"
  )
  expect_false(cachem_initialized)
  expect_s3_class(cache, "cache_mem")
  expect_null(asNamespace("dsprrr")$.dsprrr_env$cache_disk)
  expect_true(cachem::is.key_missing(cache$get("evil")))
  expect_identical(cache_test_mode(evil), "0666")
})

test_that("non-regular cache entries fail closed before cachem", {
  skip_on_os("windows")
  skip_if(Sys.which("mkfifo") == "", "mkfifo unavailable")
  local_reset_cache()

  disk_path <- tempfile("cache_fifo_")
  withr::defer(unlink(disk_path, recursive = TRUE))
  dir.create(disk_path, mode = "0700")
  fifo <- file.path(disk_path, "adversarial name.rds")
  status <- system2(Sys.which("mkfifo"), shQuote(fifo))
  skip_if(status != 0L, "could not create FIFO")
  Sys.chmod(disk_path, mode = "0700", use_umask = FALSE)
  Sys.chmod(fifo, mode = "0600", use_umask = FALSE)

  cachem_initialized <- FALSE
  testthat::local_mocked_bindings(
    cache_disk = function(...) {
      cachem_initialized <<- TRUE
      stop("non-regular entry reached cachem")
    },
    .package = "cachem"
  )
  configure_cache(
    enable_memory = TRUE,
    enable_disk = TRUE,
    disk_path = disk_path
  )

  warning_message <- NULL
  cache <- withCallingHandlers(
    dsprrr:::get_cache(),
    dsprrr_cache_security_warning = function(cnd) {
      warning_message <<- conditionMessage(cnd)
      invokeRestart("muffleWarning")
    }
  )
  expect_false(cachem_initialized)
  expect_s3_class(cache, "cache_mem")
  expect_match(warning_message, "non-regular filesystem entry")
})

test_that("symlinked cache directories and entries fail closed", {
  skip_on_os("windows")
  local_reset_cache()

  root <- tempfile("cache_symlink_")
  withr::defer(unlink(root, recursive = TRUE))
  dir.create(root)

  target_dir <- file.path(root, "target")
  linked_dir <- file.path(root, "linked")
  dir.create(target_dir, mode = "0700")
  skip_if_not(file.symlink(target_dir, linked_dir), "symlinks unavailable")

  configure_cache(
    enable_memory = TRUE,
    enable_disk = TRUE,
    disk_path = linked_dir
  )
  expect_warning(
    cache <- dsprrr:::get_cache(),
    class = "dsprrr_cache_security_warning"
  )
  expect_s3_class(cache, "cache_mem")

  entry_dir <- file.path(root, "entry")
  entry_target <- file.path(root, "target.rds")
  dir.create(entry_dir, mode = "0700")
  saveRDS(list(answer = "untrusted"), entry_target)
  Sys.chmod(entry_dir, mode = "0700", use_umask = FALSE)
  Sys.chmod(entry_target, mode = "0600", use_umask = FALSE)
  expect_true(file.symlink(entry_target, file.path(entry_dir, "key.rds")))

  configure_cache(
    enable_memory = TRUE,
    enable_disk = TRUE,
    disk_path = entry_dir
  )
  expect_warning(
    cache <- dsprrr:::get_cache(),
    class = "dsprrr_cache_security_warning"
  )
  expect_s3_class(cache, "cache_mem")
})

test_that("permission verification failures fall back to memory", {
  skip_on_os("windows")
  local_reset_cache()

  disk_path <- tempfile("cache_chmod_failure_")
  withr::defer(unlink(disk_path, recursive = TRUE))
  testthat::local_mocked_bindings(
    cache_set_private_mode = function(...) FALSE,
    .package = "dsprrr"
  )

  configure_cache(
    enable_memory = TRUE,
    enable_disk = TRUE,
    disk_path = disk_path
  )
  expect_warning(
    cache <- dsprrr:::get_cache(),
    class = "dsprrr_cache_security_warning"
  )
  expect_s3_class(cache, "cache_mem")
  expect_identical(
    asNamespace("dsprrr")$.dsprrr_env$cache_privacy_status,
    "degraded"
  )
})

test_that("non-Unix private caches rely on inherited ACLs", {
  local_reset_cache()

  disk_path <- tempfile("cache_windows_acl_")
  withr::defer(unlink(disk_path, recursive = TRUE))
  testthat::local_mocked_bindings(
    cache_private_modes_supported = function() FALSE,
    .package = "dsprrr"
  )

  configure_cache(
    enable_memory = FALSE,
    enable_disk = TRUE,
    disk_path = disk_path
  )
  cache <- expect_no_warning(dsprrr:::get_cache())
  cache$set("key", list(answer = "inherited"))

  expect_identical(cache$get("key")$answer, "inherited")
  expect_identical(
    asNamespace("dsprrr")$.dsprrr_env$cache_privacy_status,
    "unverified_windows"
  )
})

test_that("privacy enforcement can be disabled only explicitly", {
  skip_on_os("windows")
  local_reset_cache()

  disk_path <- tempfile("cache_trusted_shared_")
  withr::defer(unlink(disk_path, recursive = TRUE))
  dir.create(disk_path)
  Sys.chmod(disk_path, mode = "0777", use_umask = FALSE)

  configure_cache(
    enable_memory = FALSE,
    enable_disk = TRUE,
    disk_path = disk_path,
    disk_private = FALSE
  )
  cache <- expect_no_warning(dsprrr:::get_cache())
  cache$set("key", list(answer = "trusted"))

  expect_identical(cache$get("key")$answer, "trusted")
  expect_identical(
    asNamespace("dsprrr")$.dsprrr_env$cache_privacy_status,
    "disabled"
  )
})

test_that("clear all invalidates stale disk handles before degradation", {
  skip_on_os("windows")
  local_reset_cache()

  disk_path <- tempfile("cache_stale_handle_")
  withr::defer({
    Sys.chmod(disk_path, mode = "0700", use_umask = FALSE)
    unlink(disk_path, recursive = TRUE)
  })
  configure_cache(
    enable_memory = TRUE,
    enable_disk = TRUE,
    disk_path = disk_path
  )
  first <- dsprrr:::get_cache()
  first$set("safe", list(answer = "safe"))
  pkg_env <- asNamespace("dsprrr")$.dsprrr_env
  old_disk <- pkg_env$cache_disk
  expect_s3_class(old_disk, "cache_disk")

  clear_cache("all")
  expect_null(pkg_env$cache)
  expect_null(pkg_env$cache_memory)
  expect_null(pkg_env$cache_disk)
  expect_identical(pkg_env$cache_privacy_status, "not_checked")
  old_read <- tryCatch(old_disk$get("safe"), error = identity)
  expect_s3_class(old_read, "error")
  expect_match(conditionMessage(old_read), "destroyed")

  dir.create(disk_path)
  saveRDS(list(answer = "poisoned"), file.path(disk_path, "evil.rds"))
  Sys.chmod(disk_path, mode = "0777", use_umask = FALSE)
  expect_warning(
    second <- dsprrr:::get_cache(),
    class = "dsprrr_cache_security_warning"
  )

  expect_s3_class(second, "cache_mem")
  expect_null(pkg_env$cache_disk)
  expect_true(cachem::is.key_missing(second$get("evil")))
  expect_identical(pkg_env$cache_privacy_status, "degraded")
})

test_that("clear all detaches state and attempts every tier before errors", {
  local_reset_cache()

  pkg_env <- asNamespace("dsprrr")$.dsprrr_env
  calls <- new.env(parent = emptyenv())
  calls$memory <- 0L
  calls$disk <- 0L
  memory <- list(reset = function() {
    calls$memory <- calls$memory + 1L
    stop("memory reset failed")
  })
  disk <- list(destroy = function() {
    calls$disk <- calls$disk + 1L
    stop("disk destroy failed")
  })
  stale <- list(name = "stale-layer")
  pkg_env$cache <- stale
  pkg_env$cache_memory <- memory
  pkg_env$cache_disk <- disk
  pkg_env$cache_disk_guard <- NULL
  pkg_env$cache_degraded <- TRUE
  pkg_env$cache_degraded_reason <- "old failure"
  pkg_env$cache_privacy_status <- "degraded"
  pkg_env$cache_privacy_reason <- "old failure"
  pkg_env$cache_stats <- list(hits = 3L, misses = 4L)
  pkg_env$cache_first_hit_shown <- TRUE

  error <- rlang::catch_cnd(clear_cache("all"))
  expect_s3_class(error, "dsprrr_cache_clear_error")
  expect_identical(calls$memory, 1L)
  expect_identical(calls$disk, 1L)
  expect_null(pkg_env$cache)
  expect_null(pkg_env$cache_memory)
  expect_null(pkg_env$cache_disk)
  expect_null(pkg_env$cache_disk_guard)
  expect_false(pkg_env$cache_degraded)
  expect_null(pkg_env$cache_degraded_reason)
  expect_identical(pkg_env$cache_privacy_status, "not_checked")
  expect_identical(pkg_env$cache_stats, list(hits = 0L, misses = 0L))
  expect_false(pkg_env$cache_first_hit_shown)
})

test_that("targeted clears preserve configured tiers and untouched entries", {
  skip_on_os("windows")
  local_reset_cache()

  disk_path <- tempfile("cache_targeted_clear_")
  withr::defer(unlink(disk_path, recursive = TRUE))
  configure_cache(
    enable_memory = TRUE,
    enable_disk = TRUE,
    disk_path = disk_path
  )
  cache <- dsprrr:::get_cache()
  cache$set("key", list(answer = "both"))
  pkg_env <- asNamespace("dsprrr")$.dsprrr_env
  memory <- pkg_env$cache_memory
  disk <- pkg_env$cache_disk
  guard <- pkg_env$cache_disk_guard

  clear_cache("memory")
  expect_s3_class(pkg_env$cache, "cache_layered")
  expect_identical(pkg_env$cache_memory, memory)
  expect_identical(pkg_env$cache_disk, disk)
  expect_identical(pkg_env$cache_disk_guard, guard)
  expect_identical(memory$size(), 0L)
  expect_identical(pkg_env$cache$get("key")$answer, "both")
  expect_true(dsprrr:::get_cache_config()$enable_memory)

  configure_cache(
    enable_memory = TRUE,
    enable_disk = TRUE,
    disk_path = disk_path
  )
  cache <- dsprrr:::get_cache()
  cache$set("key", list(answer = "both-again"))
  memory <- pkg_env$cache_memory
  disk <- pkg_env$cache_disk
  guard <- pkg_env$cache_disk_guard
  clear_cache("disk")
  expect_s3_class(pkg_env$cache, "cache_layered")
  expect_identical(pkg_env$cache_memory, memory)
  expect_identical(pkg_env$cache_disk, disk)
  expect_identical(pkg_env$cache_disk_guard, guard)
  expect_identical(disk$size(), 0L)
  expect_identical(pkg_env$cache$get("key")$answer, "both-again")
  expect_true(dsprrr:::get_cache_config()$enable_disk)
})

test_that("failed targeted cleanup still leaves only the safe tier reachable", {
  local_reset_cache()

  pkg_env <- asNamespace("dsprrr")$.dsprrr_env
  remaining_disk <- list(name = "remaining-disk")
  failing_memory <- list(reset = function() stop("memory failed"))
  pkg_env$cache <- list(name = "stale-layer")
  pkg_env$cache_memory <- failing_memory
  pkg_env$cache_disk <- remaining_disk
  pkg_env$cache_disk_guard <- NULL

  error <- rlang::catch_cnd(clear_cache("memory"))
  expect_s3_class(error, "dsprrr_cache_clear_error")
  expect_identical(pkg_env$cache, remaining_disk)
  expect_null(pkg_env$cache_memory)
  expect_identical(pkg_env$cache_disk, remaining_disk)
})

test_that("cleanup FALSE is reported and detached guards stay detached", {
  local_reset_cache()

  pkg_env <- asNamespace("dsprrr")$.dsprrr_env
  guard <- new.env(parent = emptyenv())
  guard$invalid <- TRUE
  guard$reason <- "detached failure"
  guard$warning_emitted <- FALSE
  guard$trust <- list(path = tempfile("detached-cache-"))
  disk <- list(destroy = function() FALSE)
  pkg_env$cache <- disk
  pkg_env$cache_memory <- NULL
  pkg_env$cache_disk <- disk
  pkg_env$cache_disk_guard <- guard

  error <- rlang::catch_cnd(clear_cache("all"))
  expect_s3_class(error, "dsprrr_cache_clear_error")
  expect_false(pkg_env$cache_degraded)
  expect_identical(pkg_env$cache_privacy_status, "not_checked")
  expect_false(guard$warning_emitted)
})

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
  expect_identical(result$cache_disk_private, TRUE)
  expect_identical(result$cache_privacy_status, "not_checked")
  expect_identical(
    result$cache_disk_path,
    dsprrr:::get_cache_config()$disk_path
  )

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

test_that("dsprrr_sitrep reports a degraded private disk cache", {
  skip_on_os("windows")
  local_reset_cache()

  disk_path <- tempfile("cache_sitrep_degraded_")
  withr::defer(unlink(disk_path, recursive = TRUE))
  dir.create(disk_path)
  Sys.chmod(disk_path, mode = "0777", use_umask = FALSE)
  configure_cache(disk_path = disk_path)
  suppressWarnings(dsprrr:::get_cache())

  result <- expect_no_error(suppressMessages(dsprrr_sitrep()))
  expect_true(result$cache_degraded)
  expect_identical(result$cache_privacy_status, "degraded")
  expect_match(
    result$cache_degraded_reason,
    "writable by another local account"
  )
})

test_that("dsprrr_sitrep does not claim memory fallback without memory", {
  skip_on_os("windows")
  local_reset_cache()

  disk_path <- tempfile("cache_sitrep_no_memory_")
  withr::defer(unlink(disk_path, recursive = TRUE))
  dir.create(disk_path)
  Sys.chmod(disk_path, mode = "0777", use_umask = FALSE)
  configure_cache(
    enable_memory = FALSE,
    enable_disk = TRUE,
    disk_path = disk_path
  )
  expect_warning(
    dsprrr:::get_cache(),
    class = "dsprrr_cache_security_warning"
  )

  output <- cli::cli_fmt(dsprrr_sitrep())
  expect_true(any(grepl(
    "no cache tier remains enabled",
    output,
    fixed = TRUE
  )))
  expect_false(any(grepl("using memory only", output, fixed = TRUE)))
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
