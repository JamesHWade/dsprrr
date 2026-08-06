artifact_leaf <- function(input = "text", output = "answer") {
  module(signature(paste0(input, " -> ", output)))
}

ArtifactCycleProgram <- R6::R6Class(
  "ArtifactCycleProgram",
  inherit = dsprrr:::Module,
  public = list(
    children = NULL,
    initialize = function() {
      super$initialize(signature("text -> answer"))
      self$children <- list()
    },
    graph_children = function() self$children,
    set_graph_children = function(children) {
      self$children <- children
      invisible(self)
    },
    forward = function(...) cli::cli_abort("Traversal-only test module")
  )
)

artifact_codec_fixtures <- function() {
  forward_fn <- function(text, ...) list(answer = text)
  reward_fn <- function(prediction, inputs) 1
  condition_fn <- function(output) TRUE
  vectorizer <- function(text) matrix(1, nrow = length(text))
  input_text <- function(inputs) inputs$text
  retriever <- function(query, k) rep(query, k)
  repl_tool <- function(x) x
  runner <- list(
    execute = function(code, context = list()) list(success = TRUE),
    policy = function() {
      list(backend = "test", trust = "test", sandboxed = TRUE)
    }
  )
  tool_fn <- function(query) query
  tool <- ellmer::tool(
    tool_fn,
    description = "Echo",
    arguments = list(query = ellmer::type_string()),
    name = "echo"
  )
  registry <- list(
    forward = forward_fn,
    reward = reward_fn,
    condition = condition_fn,
    vectorizer = vectorizer,
    input_text = input_text,
    retriever = retriever,
    repl_tool = repl_tool,
    runner = runner,
    tool = tool
  )
  programs <- list(
    predict = artifact_leaf(),
    react = module(
      signature("question -> answer"),
      type = "react",
      tools = list(tool)
    ),
    pipeline = pipeline(artifact_leaf(), artifact_leaf("answer", "summary")),
    ensemble = ensemble(list(artifact_leaf(), artifact_leaf())),
    multichain = multi_chain_comparison("question -> answer"),
    refine = refine(artifact_leaf(), reward_fn = reward_fn),
    best_of_n = best_of_n(artifact_leaf(), reward_fn = reward_fn),
    assert = with_assertions(
      artifact_leaf(),
      list(Assertion(condition = condition_fn, message = "valid"))
    ),
    knn = dsprrr:::KNNFewShotModule$new(
      module = artifact_leaf(),
      k = 1L,
      vectorizer = vectorizer,
      input_text = input_text,
      train_embeddings = matrix(1, nrow = 1),
      trainset_demos = list(list(text = "x", answer = "y"))
    ),
    fn = module_fn("text -> answer", forward_fn),
    program_of_thought = dsprrr:::ProgramOfThoughtModule$new(
      signature = signature("question -> answer"),
      runner = runner
    ),
    codeact = dsprrr:::CodeActModule$new(
      signature = signature("question -> answer"),
      tools = list(tool),
      runner = runner
    ),
    rlm = dsprrr:::RLMModule$new(
      signature = signature("question -> answer"),
      runner = runner,
      tools = list(echo = repl_tool)
    ),
    rag = dsprrr:::RAGModule$new(
      signature = signature("question -> answer"),
      retriever = retriever
    )
  )
  artifacts <- lapply(programs, program_artifact, registry = registry)
  list(artifacts = artifacts, registry = registry)
}

artifact_rehash <- function(artifact) {
  artifact$integrity <- dsprrr:::artifact_integrity(artifact)
  artifact
}

expect_artifact_manifest_rejected <- function(artifact, info) {
  condition <- rlang::catch_cnd(
    dsprrr:::artifact_validate_manifest(artifact_rehash(artifact))
  )
  expect_false(is.null(condition), info = info)
  expect_true(
    inherits(condition, "dsprrr_artifact_malformed") ||
      inherits(condition, "dsprrr_artifact_unsupported_type") ||
      inherits(condition, "dsprrr_artifact_unsafe_value"),
    info = info
  )
}

test_that("artifacts round-trip nested graphs and shared identity", {
  shared <- artifact_leaf()
  shared$demos <- list(list(text = "example", answer = "response"))
  shared$state$compiled <- TRUE
  shared$config$optimizer <- list(
    method = "test-optimizer",
    budget_summary = list(successes = 4L, errors = 0L)
  )
  vote <- ensemble(list(left = shared, right = shared))
  wrapped <- best_of_n(vote, N = 2L)
  summarize <- artifact_leaf("answer", "summary")
  freeze_modules(summarize)
  program <- pipeline(vote = wrapped, summarize = summarize)

  artifact <- program_artifact(program)
  second <- program_artifact(program)
  restored <- restore_module_config(artifact)

  expect_identical(
    names(artifact$graph$nodes),
    module_graph(program)$canonical_path[
      !module_graph(program)$shared
    ]
  )
  expect_identical(artifact$graph, second$graph)
  expect_identical(
    artifact$integrity$payload_sha256,
    second$integrity$payload_sha256
  )
  expect_identical(module_graph(restored)$path, module_graph(program)$path)
  expect_identical(
    module_graph(restored)$canonical_path,
    module_graph(program)$canonical_path
  )

  restored_vote <- restored$steps$vote@module$module
  expect_identical(restored_vote$modules$left, restored_vote$modules$right)
  expect_identical(restored_vote$modules$left$demos, shared$demos)
  expect_identical(restored_vote$modules$left$is_compiled(), TRUE)
  expect_identical(
    restored_vote$modules$left$config$optimizer$budget_summary$successes,
    4L
  )
  expect_identical(
    is_module_frozen(restored$steps$summarize@module),
    TRUE
  )
  round_trip <- program_artifact(restored)
  expect_identical(round_trip$graph, artifact$graph)
  expect_identical(round_trip$integrity, artifact$integrity)
})

test_that("safe artifact lists preserve nested named and unnamed NULLs", {
  program <- artifact_leaf()
  program$config$null_contract <- list(
    named = NULL,
    unnamed = list("before", NULL, list(inner = NULL), "after"),
    nested = list(
      left = list(NULL, value = 1L),
      right = list(value = 2L, NULL)
    ),
    api_key = "must-not-be-persisted"
  )

  artifact <- program_artifact(program)
  restored <- restore_module_config(artifact)
  contract <- restored$config$null_contract

  expect_named(contract, c("named", "unnamed", "nested"))
  expect_null(contract$named)
  expect_length(contract$unnamed, 4L)
  expect_null(contract$unnamed[[2L]])
  expect_null(contract$unnamed[[3L]]$inner)
  expect_identical(contract$unnamed[c(1L, 4L)], list("before", "after"))
  expect_null(contract$nested$left[[1L]])
  expect_null(contract$nested$right[[2L]])
  expect_false("api_key" %in% names(contract))
})

test_that("signature schemas and complex strings round-trip exactly", {
  sig <- signature(
    inputs = list(
      input(
        "payload",
        ellmer::type_object(
          title = ellmer::type_string("Title"),
          tags = ellmer::type_array(ellmer::type_enum(c("a", "b")))
        ),
        description = "Nested input"
      )
    ),
    output_type = ellmer::type_object(
      answer = ellmer::type_string("Answer"),
      confidence = ellmer::type_number("Confidence", required = FALSE),
      flags = ellmer::type_array(ellmer::type_boolean())
    ),
    instructions = "Quote: \"yes\"\nBackslash: \\ and unicode: λ"
  )
  complex <- "line one\nline \"two\" \\ {literal} λ"
  program <- module(
    sig,
    template = complex,
    demos = list(list(payload = complex, answer = complex)),
    config = list(label = complex, nested = list(values = c("a", "b")))
  )

  restored <- restore_module_config(program_artifact(program))

  expect_identical(
    signature_to_json_schema(restored$signature),
    signature_to_json_schema(program$signature)
  )
  expect_identical(restored$signature@instructions, sig@instructions)
  expect_identical(restored$template, complex)
  expect_identical(restored$demos, program$demos)
  expect_identical(restored$config$label, complex)
  expect_identical(restored$config$nested, program$config$nested)

  json_type <- ellmer::type_from_schema(
    text = '{"type":"object","properties":{"answer":{"type":"string"}}}'
  )
  json_program <- module(signature(
    inputs = list(input("text")),
    output_type = json_type,
    instructions = "JSON schema"
  ))
  json_restored <- restore_module_config(program_artifact(json_program))
  expect_identical(json_restored$signature@output_type@json, json_type@json)
})

test_that("the closed value codec preserves supported R data types", {
  timestamp <- as.POSIXct("2026-07-11 12:34:56", tz = "UTC")
  values <- list(
    matrix = matrix(
      1:4,
      nrow = 2,
      dimnames = list(c("r1", "r2"), c("c1", "c2"))
    ),
    frame = data.frame(
      group = factor(c("a", "b"), levels = c("b", "a")),
      date = as.Date(c("2026-07-10", "2026-07-11"))
    ),
    timestamp = timestamp,
    duration = difftime(timestamp + 60, timestamp, units = "secs"),
    raw = as.raw(c(0, 127, 255)),
    complex = c(1 + 2i, NA_complex_),
    special = c(NA_real_, NaN, Inf, -Inf)
  )
  program <- artifact_leaf()
  program$config$values <- values

  restored <- restore_module_config(program_artifact(program))
  expect_identical(restored$config$values, values)

  unsafe <- artifact_leaf()
  unsafe$config$value <- structure("value", class = "custom_atomic")
  expect_snapshot(program_artifact(unsafe), error = TRUE)
})

test_that("credentials and runtime history are excluded recursively", {
  sentinel <- "ARTIFACT_SECRET_SENTINEL"
  program <- artifact_leaf()
  program$config$api_key <- sentinel
  program$config$client_secret <- sentinel
  program$config$private_key <- sentinel
  program$config$cookie <- sentinel
  program$config$base_url <- paste0(
    "https://user:",
    sentinel,
    "@example.test?token=",
    sentinel
  )
  program$config$nested <- list(access_token = sentinel, safe = "kept")
  program$config$named <- c(api_key = sentinel, safe = "kept")
  program$config$prompt <- sentinel
  program$config$optimizer <- list(
    method = "safe",
    candidate_instructions = sentinel,
    instruction_candidates = sentinel,
    all_generations = sentinel,
    trial_history = sentinel,
    stop_reason = "complete"
  )
  program$state$best_params <- list(temperature = 0.2, credentials = sentinel)
  program$state$traces <- list(list(prompt = sentinel, response = sentinel))
  program$state$trials <- tibble::tibble(prompt = sentinel)
  program$state$optimization_history <- list(response = sentinel)

  artifact <- program_artifact(program)
  rendered <- paste(capture.output(dput(artifact)), collapse = "\n")
  restored <- restore_module_config(artifact)

  expect_identical(grepl(sentinel, rendered, fixed = TRUE), FALSE)
  expect_null(restored$config$api_key)
  expect_null(restored$config$client_secret)
  expect_null(restored$config$private_key)
  expect_null(restored$config$cookie)
  expect_null(restored$config$base_url)
  expect_null(restored$config$nested$access_token)
  expect_identical(restored$config$nested$safe, "kept")
  expect_identical(restored$config$named, c(safe = "kept"))
  expect_null(restored$config$prompt)
  expect_identical(restored$config$optimizer$method, "safe")
  expect_identical(restored$config$optimizer$stop_reason, "complete")
  expect_null(restored$config$optimizer$candidate_instructions)
  expect_null(restored$config$optimizer$instruction_candidates)
  expect_null(restored$config$optimizer$all_generations)
  expect_null(restored$config$optimizer$trial_history)
  expect_identical(restored$state$best_params$temperature, 0.2)
  expect_null(restored$state$best_params$credentials)
  expect_length(restored$state$traces, 0L)
  expect_length(restored$state$trials, 0L)
  expect_setequal(
    vapply(artifact$exclusions, `[[`, character(1), "reason"),
    c("credential", "runtime-data")
  )
})

test_that("semantic demo fields are never silently redacted", {
  sentinel <- "ARTIFACT_SEMANTIC_FIELD_SENTINEL"
  program <- module(signature("token -> answer"))
  program$demos <- list(list(token = sentinel, answer = "world"))

  condition <- rlang::catch_cnd(program_artifact(program))

  expect_s3_class(condition, "dsprrr_artifact_unsafe_value")
  expect_match(conditionMessage(condition), "cannot be silently removed")
  expect_false(grepl(sentinel, conditionMessage(condition), fixed = TRUE))

  extra <- artifact_leaf()
  extra$demos <- list(list(
    text = "safe",
    password = sentinel,
    answer = "world"
  ))
  extra_condition <- rlang::catch_cnd(program_artifact(extra))
  expect_s3_class(extra_condition, "dsprrr_artifact_unsafe_value")
  expect_false(grepl(
    sentinel,
    conditionMessage(extra_condition),
    fixed = TRUE
  ))
})

test_that("credential names fail closed across camel and acronym styles", {
  sentinel <- "ARTIFACT_CAMEL_SECRET_SENTINEL"
  credential_names <- c(
    "openaiApiKey",
    "openaiAPIKey",
    "OpenAIAPIKey",
    "OAuthAccessToken",
    "accessToken",
    "clientSecret",
    "ClientSecret",
    "privateKey",
    "refreshToken",
    "sessionToken",
    "apiKeyFile"
  )
  benign_names <- c(
    "apiKeyboard",
    "accessTokenizer",
    "clientSecretariat",
    "privateKeyboard",
    "secretary",
    "monKey"
  )

  expect_true(all(vapply(
    credential_names,
    dsprrr:::artifact_is_secret_name,
    logical(1)
  )))
  expect_false(any(vapply(
    benign_names,
    dsprrr:::artifact_is_secret_name,
    logical(1)
  )))

  program <- artifact_leaf()
  for (name in credential_names) {
    program$config[[name]] <- sentinel
  }
  for (name in benign_names) {
    program$config[[name]] <- paste0("benign-", name)
  }

  artifact <- program_artifact(program)
  rendered <- paste(capture.output(dput(artifact)), collapse = "\n")
  restored <- restore_module_config(artifact)

  expect_false(grepl(sentinel, rendered, fixed = TRUE))
  expect_false(any(credential_names %in% names(restored$config)))
  expect_identical(
    unname(unlist(restored$config[benign_names], use.names = FALSE)),
    paste0("benign-", benign_names)
  )
})

test_that("credential-bearing connection fields fail closed", {
  credential_names <- c(
    "databaseUrl",
    "DATABASE_URL",
    "DatabaseURI",
    "dbUrl",
    "connectionString",
    "ConnectionString",
    "CONNECTION_STRING",
    "connectionUri",
    "primaryConnectionUri",
    "readReplicaConnectionUrl",
    "databaseConnectionUrl",
    "redisConnectionUri",
    "databaseConnectionString",
    "redisConnectionString",
    "dsn",
    "DSN",
    "sentryDsn",
    "databaseDsn",
    "odbcDsn",
    "dataSourceName",
    "odbcDataSourceName",
    "jdbcUrl",
    "jdbcDatabaseUrl",
    "redisUrl",
    "mongoUri",
    "neo4jUri",
    "cassandraUrl",
    "clickhouseUrl",
    "couchbaseUri",
    "influxdbUrl",
    "postgresqlUrl",
    "mysqlUri",
    "springDatasourceUrl"
  )
  benign_names <- c(
    "databaseUrlPolicy",
    "databaseUriParser",
    "connectionStringFormat",
    "connectionStringer",
    "primaryConnectionUriPolicy",
    "readReplicaConnectionUrlDocumentation",
    "databaseConnectionUrlFormat",
    "redisConnectionUriPolicy",
    "databaseConnectionStringFormat",
    "redisConnectionStringPolicy",
    "dsnDocumentation",
    "sentryDsnFormat",
    "databaseDsnPolicy",
    "odbcDsnDocumentation",
    "dataSourceNamespace",
    "odbcDataSourceNameDocumentation",
    "jdbcDatabaseUrlTemplate",
    "redisUrlPolicy",
    "mongoUriTemplate",
    "neo4jUriTemplate",
    "cassandraUrlPolicy",
    "clickhouseUrlFormat",
    "couchbaseUriTemplate",
    "influxdbUrlDocumentation",
    "springDatasourceUrlDocumentation",
    "webUrl"
  )
  sentinels <- stats::setNames(
    paste0("ARTIFACT_CONNECTION_SECRET_SENTINEL_", seq_along(credential_names)),
    credential_names
  )

  expect_true(all(vapply(
    credential_names,
    dsprrr:::artifact_is_secret_name,
    logical(1)
  )))
  expect_false(any(vapply(
    benign_names,
    dsprrr:::artifact_is_secret_name,
    logical(1)
  )))

  program <- artifact_leaf()
  for (name in credential_names) {
    program$config[[name]] <- sentinels[[name]]
  }
  for (name in benign_names) {
    program$config[[name]] <- paste0("benign-", name)
  }

  artifact <- program_artifact(program)
  rendered <- paste(capture.output(dput(artifact)), collapse = "\n")
  restored <- restore_module_config(artifact)

  expect_false(any(vapply(
    sentinels,
    grepl,
    logical(1),
    x = rendered,
    fixed = TRUE
  )))
  expect_false(any(credential_names %in% names(restored$config)))
  expect_identical(
    restored$config[benign_names],
    as.list(stats::setNames(paste0("benign-", benign_names), benign_names))
  )
})

test_that("credential-bearing key material fields fail closed", {
  credential_names <- c(
    "passphrase",
    "passPhrase",
    "databasePassphrase",
    "encryptionKey",
    "ENCRYPTION_KEY",
    "signingKey",
    "jwtSigningKey",
    "signingKeyPem",
    "signingKeyJwk",
    "signingKeyDer",
    "signingKeyPkcs8",
    "signingKeyB64",
    "sshKey",
    "sshKeyFile",
    "sshKeyPem",
    "hmacKey",
    "tlsKey",
    "sslKeyData",
    "masterKeyHex",
    "fernetKeyBase64",
    "licenseKey",
    "licenseKeyValue",
    "serviceAccountKey",
    "storageAccountKey",
    "azureStorageAccountKey",
    "serviceAccountJson",
    "totpSeed",
    "totpSeedBase32"
  )
  benign_names <- c(
    "signingKeyAlgorithm",
    "signingKeyPemFormat",
    "signingKeyJwkParser",
    "signingKeyDerFormat",
    "signingKeyPkcs8Policy",
    "signingKeyB64Template",
    "sshKeyPemParser",
    "hmacKeyAlgorithm",
    "tlsKeyUsage",
    "sslKeyDataFormat",
    "masterKeyHexParser",
    "fernetKeyBase64Policy",
    "licenseKeyFormat",
    "serviceAccountKeyPolicy",
    "storageAccountKeyPolicy",
    "azureStorageAccountKeyFormat",
    "serviceAccountJsonSchema",
    "passphraseHint",
    "encryptionKeyboard",
    "sshKeynote",
    "totpSeedLength",
    "totpSeedBase32Format"
  )
  sentinels <- stats::setNames(
    paste0("ARTIFACT_KEY_MATERIAL_SENTINEL_", seq_along(credential_names)),
    credential_names
  )

  expect_true(all(vapply(
    credential_names,
    dsprrr:::artifact_is_secret_name,
    logical(1)
  )))
  expect_false(any(vapply(
    benign_names,
    dsprrr:::artifact_is_secret_name,
    logical(1)
  )))

  program <- artifact_leaf()
  for (name in credential_names) {
    program$config[[name]] <- sentinels[[name]]
  }
  for (name in benign_names) {
    program$config[[name]] <- paste0("benign-", name)
  }

  artifact <- program_artifact(program)
  rendered <- paste(capture.output(dput(artifact)), collapse = "\n")
  restored <- restore_module_config(artifact)

  expect_false(any(vapply(
    sentinels,
    grepl,
    logical(1),
    x = rendered,
    fixed = TRUE
  )))
  expect_false(any(credential_names %in% names(restored$config)))
  expect_identical(
    restored$config[benign_names],
    as.list(stats::setNames(paste0("benign-", benign_names), benign_names))
  )
})

test_that("runtime fields are excluded across camel and acronym styles", {
  runtime_names <- c(
    "baseUrl",
    "apiArgs",
    "extraHeaders",
    "providerArgs",
    "requestArgs",
    "runtimeHistory",
    "trialHistory",
    "generatedPrompts",
    "allGenerations"
  )
  sentinels <- stats::setNames(
    paste0("ARTIFACT_CAMEL_RUNTIME_SENTINEL_", seq_along(runtime_names)),
    runtime_names
  )
  benign_names <- c(
    "baseUrlPolicy",
    "apiArgumentCount",
    "headerStyle",
    "providerArgumentSchema",
    "requestArgumentCount",
    "runtimeHistorical",
    "trialHistorian",
    "promptStyle",
    "allergenCount"
  )

  expect_true(all(vapply(
    runtime_names,
    dsprrr:::artifact_is_runtime_name,
    logical(1)
  )))
  expect_false(any(vapply(
    benign_names,
    dsprrr:::artifact_is_runtime_name,
    logical(1)
  )))

  program <- artifact_leaf()
  for (name in runtime_names) {
    program$config[[name]] <- sentinels[[name]]
  }
  for (name in benign_names) {
    program$config[[name]] <- paste0("benign-", name)
  }

  artifact <- program_artifact(program)
  rendered <- paste(capture.output(dput(artifact)), collapse = "\n")
  restored <- restore_module_config(artifact)

  expect_false(any(vapply(
    sentinels,
    grepl,
    logical(1),
    x = rendered,
    fixed = TRUE
  )))
  expect_false(any(runtime_names %in% names(restored$config)))
  expect_identical(
    unname(unlist(restored$config[benign_names], use.names = FALSE)),
    paste0("benign-", benign_names)
  )
})

test_that("chat configuration records only provider and model", {
  sentinel <- "CHAT_HISTORY_SENTINEL"
  fake_chat <- structure(
    list(
      api_key = sentinel,
      turns = list(sentinel),
      get_model = function() "safe-model"
    ),
    class = c("SafeProviderChat", "Chat")
  )
  program <- artifact_leaf()
  program$chat <- fake_chat

  artifact <- program_artifact(program)
  restored <- restore_module_config(artifact)
  rendered <- paste(capture.output(dput(artifact)), collapse = "\n")

  expect_identical(
    artifact$graph$nodes[["$"]]$provider_model,
    list(provider = "SafeProviderChat", model = "safe-model")
  )
  expect_identical(grepl(sentinel, rendered, fixed = TRUE), FALSE)
  expect_null(restored$chat)
  expect_identical(
    dsprrr:::artifact_detached_runtime(restored)$chat,
    list(provider = "SafeProviderChat", model = "safe-model")
  )

  runner <- list(
    execute = function(code, context = list()) list(success = TRUE),
    policy = function() {
      list(
        backend = "test",
        trust = "test",
        sandboxed = TRUE
      )
    }
  )
  rlm <- dsprrr:::RLMModule$new(
    signature = signature("question -> answer"),
    runner = runner,
    sub_lm = fake_chat
  )
  rlm_artifact <- program_artifact(rlm, registry = list(runner = runner))
  restored_rlm <- restore_module_config(
    rlm_artifact,
    registry = list(runner = runner)
  )
  expect_null(restored_rlm$sub_lm)
  expect_identical(
    dsprrr:::artifact_detached_runtime(restored_rlm)$sub_lm,
    list(provider = "SafeProviderChat", model = "safe-model")
  )
  restored_artifact <- program_artifact(restored)
  restored_rlm_artifact <- program_artifact(
    restored_rlm,
    registry = list(runner = runner)
  )
  expect_identical(restored_artifact$graph, artifact$graph)
  expect_identical(restored_artifact$integrity, artifact$integrity)
  expect_identical(restored_rlm_artifact$graph, rlm_artifact$graph)
  expect_identical(restored_rlm_artifact$integrity, rlm_artifact$integrity)
})

test_that("ellmer provider metadata is specific and credential-free", {
  make_artifact <- function(chat) {
    program <- artifact_leaf()
    program$chat <- chat
    program_artifact(program)
  }

  openai_secret <- "OPENAI_PROVIDER_CREDENTIAL_SENTINEL"
  anthropic_secret <- "ANTHROPIC_PROVIDER_CREDENTIAL_SENTINEL"
  url_user_secret <- "URL_USERINFO_CREDENTIAL_SENTINEL"
  url_query_secret <- "URL_QUERY_CREDENTIAL_SENTINEL"
  model <- "shared-model"

  openai <- ellmer::chat_openai(
    credentials = function() openai_secret,
    model = model
  )
  anthropic <- ellmer::chat_anthropic(
    credentials = function() anthropic_secret,
    model = model
  )
  other_endpoint <- ellmer::chat_openai(
    credentials = function() openai_secret,
    base_url = "https://gateway.example.test/v1",
    model = model
  )
  unsafe_endpoint <- ellmer::chat_openai(
    credentials = function() openai_secret,
    base_url = paste0(
      "https://user:",
      url_user_secret,
      "@gateway.example.test/v1?token=",
      url_query_secret
    ),
    model = model
  )

  artifacts <- lapply(
    list(openai, anthropic, other_endpoint, unsafe_endpoint),
    make_artifact
  )
  metadata <- lapply(
    artifacts,
    function(artifact) artifact$graph$nodes[["$"]]$provider_model
  )

  expect_false(identical(metadata[[1L]], metadata[[2L]]))
  expect_false(identical(metadata[[1L]], metadata[[3L]]))
  expect_identical(metadata[[1L]]$model, model)

  openai_provider <- jsonlite::fromJSON(metadata[[1L]]$provider)
  anthropic_provider <- jsonlite::fromJSON(metadata[[2L]]$provider)
  other_provider <- jsonlite::fromJSON(metadata[[3L]]$provider)
  unsafe_provider <- jsonlite::fromJSON(metadata[[4L]]$provider)

  expect_identical(
    names(openai_provider),
    c("class", "name", "base_url")
  )
  expect_identical(openai_provider$class, "ellmer::ProviderOpenAI")
  expect_identical(openai_provider$name, "OpenAI")
  expect_identical(
    openai_provider$base_url,
    "https://api.openai.com/v1"
  )
  expect_identical(anthropic_provider$name, "Anthropic")
  expect_identical(
    other_provider$base_url,
    "https://gateway.example.test/v1"
  )
  expect_null(unsafe_provider$base_url)

  rendered <- paste(capture.output(dput(artifacts)), collapse = "\n")
  secrets <- c(
    openai_secret,
    anthropic_secret,
    url_user_secret,
    url_query_secret
  )
  expect_false(any(vapply(
    secrets,
    grepl,
    logical(1),
    x = rendered,
    fixed = TRUE
  )))
  expect_false(any(vapply(
    secrets,
    grepl,
    logical(1),
    x = paste(
      vapply(metadata, `[[`, character(1), "provider"),
      collapse = "\n"
    ),
    fixed = TRUE
  )))
})

test_that("registered function modules preserve behavior", {
  first_fn <- function(text, ...) list(intermediate = paste0(text, "!"))
  second_fn <- function(intermediate, ...) {
    list(answer = paste0("[", intermediate, "]"))
  }
  program <- pipeline(
    first = module_fn("text -> intermediate", first_fn),
    second = module_fn("intermediate -> answer", second_fn)
  )
  registry <- list(first = first_fn, second = second_fn)
  artifact <- program_artifact(program, registry = registry)

  expect_snapshot(
    restore_module_config(artifact),
    error = TRUE
  )

  restored <- restore_module_config(artifact, registry = registry)
  expect_identical(restored$run(text = "ok"), list(answer = "[ok!]"))

  incompatible <- function(value) list(intermediate = value)
  expect_snapshot(
    restore_module_config(
      artifact,
      registry = list(first = incompatible, second = second_fn)
    ),
    error = TRUE
  )
})

test_that("tools require registry IDs and round-trip by identity", {
  search <- function(query) paste("found", query)
  tool <- ellmer::tool(
    search,
    description = "Search",
    arguments = list(query = ellmer::type_string()),
    name = "search"
  )
  program <- module(
    signature("question -> answer"),
    type = "react",
    tools = list(tool)
  )

  expect_snapshot(program_artifact(program), error = TRUE)

  registry <- list(search = tool)
  artifact <- program_artifact(program, registry = registry)
  restored <- restore_module_config(artifact, registry = registry)
  expect_identical(restored$tools[[1]], tool)
})

test_that("embedded runtime values require dual trusted opt-in", {
  reducer <- local({
    suffix <- "!"
    function(outputs, weights = NULL) {
      result <- outputs[[length(outputs)]]
      result$answer <- paste0(result$answer, suffix)
      result
    }
  })
  program <- ensemble(
    list(artifact_leaf(), artifact_leaf()),
    reduce_fn = reducer
  )

  expect_snapshot(program_artifact(program), error = TRUE)
  artifact <- program_artifact(program, trusted = TRUE)
  environment(reducer)$suffix <- "CHANGED"
  expect_snapshot(restore_module_config(artifact), error = TRUE)

  restored <- restore_module_config(artifact, trusted = TRUE)
  expect_identical(
    restored$reduce_fn(list(list(answer = "a"), list(answer = "b"))),
    list(answer = "b!")
  )

  path <- withr::local_tempfile(fileext = ".rds")
  unlink(path)
  environment(reducer)$suffix <- "!"
  save_program(program, path, trusted = TRUE)
  expect_snapshot(load_program(path), error = TRUE)
  loaded <- load_program(path, trusted = TRUE)
  expect_identical(
    loaded$reduce_fn(list(list(answer = "a"), list(answer = "b"))),
    list(answer = "b!")
  )
})

test_that("built-in callables preserve arguments and reject altered environments", {
  modules <- list(artifact_leaf(), artifact_leaf())
  outputs <- list(
    list(answer = "a"),
    list(answer = "b"),
    list(answer = "b")
  )
  reducers <- list(
    majority = reduce_majority(field = "answer", tie_breaker = "first"),
    weighted = reduce_weighted_vote(field = "answer"),
    first = reduce_first()
  )
  restored <- lapply(reducers, function(reducer) {
    program <- ensemble(modules, reduce_fn = reducer)
    restore_module_config(program_artifact(program))$reduce_fn
  })

  expect_identical(restored$majority(outputs)$answer, "b")
  expect_identical(restored$weighted(outputs, c(10, 1, 1))$answer, "a")
  expect_identical(restored$first(outputs)$answer, "a")

  wrapped <- restore_module_config(
    program_artifact(best_of_n(artifact_leaf()))
  )
  expect_identical(wrapped$reward_fn(list(answer = "ok"), list()), 1)
  expect_identical(wrapped$reward_fn(NULL, list()), 0)

  altered <- reduce_majority()
  environment(altered)$table <- function(...) structure(1, names = "wrong")
  unsafe <- ensemble(modules, reduce_fn = altered)
  expect_s3_class(
    rlang::catch_cnd(program_artifact(unsafe)),
    "dsprrr_artifact_unsafe_value"
  )
})

test_that("all built-in module codecs reconstruct", {
  forward_fn <- function(text, ...) list(answer = text)
  reward_fn <- function(prediction, inputs) 1
  condition_fn <- function(output) TRUE
  vectorizer <- function(text) matrix(1, nrow = length(text))
  input_text <- function(inputs) inputs$text
  retriever <- function(query, k) rep(query, k)
  repl_tool <- function(x) x
  runner <- list(
    execute = function(code, context = list()) list(success = TRUE),
    policy = function() {
      list(
        backend = "test",
        trust = "test",
        sandboxed = TRUE
      )
    }
  )
  tool_fn <- function(query) query
  tool <- ellmer::tool(
    tool_fn,
    description = "Echo",
    arguments = list(query = ellmer::type_string()),
    name = "echo"
  )
  registry <- list(
    forward = forward_fn,
    reward = reward_fn,
    condition = condition_fn,
    vectorizer = vectorizer,
    input_text = input_text,
    retriever = retriever,
    repl_tool = repl_tool,
    runner = runner,
    tool = tool
  )
  leaf <- artifact_leaf()
  modules <- list(
    predict = leaf,
    react = module(
      signature("question -> answer"),
      type = "react",
      tools = list(tool)
    ),
    pipeline = pipeline(artifact_leaf(), artifact_leaf("answer", "summary")),
    ensemble = ensemble(list(artifact_leaf(), artifact_leaf())),
    multichain = multi_chain_comparison("question -> answer"),
    best_of_n = best_of_n(artifact_leaf(), reward_fn = reward_fn),
    refine = refine(artifact_leaf(), reward_fn = reward_fn),
    assert = with_assertions(
      artifact_leaf(),
      list(Assertion(condition = condition_fn, message = "valid"))
    ),
    knn = dsprrr:::KNNFewShotModule$new(
      module = artifact_leaf(),
      k = 1L,
      vectorizer = vectorizer,
      input_text = input_text,
      train_embeddings = matrix(1, nrow = 1),
      trainset_demos = list(list(text = "x", answer = "y"))
    ),
    fn = module_fn("text -> answer", forward_fn),
    program_of_thought = dsprrr:::ProgramOfThoughtModule$new(
      signature = signature("question -> answer"),
      runner = runner
    ),
    codeact = dsprrr:::CodeActModule$new(
      signature = signature("question -> answer"),
      tools = list(tool),
      runner = runner
    ),
    rlm = dsprrr:::RLMModule$new(
      signature = signature("question -> answer"),
      runner = runner,
      tools = list(echo = repl_tool)
    ),
    rag = dsprrr:::RAGModule$new(
      signature = signature("question -> answer"),
      retriever = retriever
    )
  )

  restored <- lapply(modules, function(program) {
    restore_module_config(
      program_artifact(program, registry = registry),
      registry = registry
    )
  })

  expect_identical(
    vapply(restored, function(module) class(module)[1], character(1)),
    vapply(modules, function(module) class(module)[1], character(1))
  )
})

test_that("rehashed malformed manifests fail closed before construction", {
  fixtures <- artifact_codec_fixtures()$artifacts
  cases <- list(
    signature_required_na = list("predict", function(x) {
      x$graph$nodes[["$"]]$signature$output_type$required <- NA
      x
    }),
    signature_description_closure = list("predict", function(x) {
      x$graph$nodes[["$"]]$signature$output_type$description <- function() NULL
      x
    }),
    state_compiled_na = list("predict", function(x) {
      x$graph$nodes[["$"]]$state$compiled <- NA
      x
    }),
    optimization_compiled_text = list("predict", function(x) {
      x$graph$nodes[["$"]]$optimization$compiled <- "yes"
      x
    }),
    optimization_compiled_contradiction = list("predict", function(x) {
      x$graph$nodes[["$"]]$optimization$compiled <- TRUE
      x
    }),
    optimization_negative_trials = list("predict", function(x) {
      x$graph$nodes[["$"]]$optimization$n_trials <- -10L
      x
    }),
    optimization_forged_provenance = list("predict", function(x) {
      x$graph$nodes[["$"]]$optimization$provenance <- list(method = "forged")
      x
    }),
    provider_na = list("predict", function(x) {
      x$graph$nodes[["$"]]$provider_model <- list(
        provider = NA_character_,
        model = NULL
      )
      x
    }),
    predict_template_na = list("predict", function(x) {
      x$graph$nodes[["$"]]$fields$template <- NA_character_
      x
    }),
    predict_atomic_demos = list("predict", function(x) {
      x$graph$nodes[["$"]]$fields$demos <- "not-a-list"
      x
    }),
    react_zero_iterations = list("react", function(x) {
      x$graph$nodes[["$"]]$fields$max_iterations <- 0L
      x
    }),
    react_text_iterations = list("react", function(x) {
      x$graph$nodes[["$"]]$fields$max_iterations <- "ten"
      x
    }),
    pipeline_step_count = list("pipeline", function(x) {
      x$graph$nodes[["$"]]$fields$steps <-
        x$graph$nodes[["$"]]$fields$steps[1]
      x
    }),
    pipeline_wrong_child_key = list("pipeline", function(x) {
      names(x$graph$nodes[["$"]]$children) <- "wrong"
      x
    }),
    ensemble_text_weights = list("ensemble", function(x) {
      x$graph$nodes[["$"]]$fields$weights <- "equal"
      x
    }),
    ensemble_wrong_child_key = list("ensemble", function(x) {
      names(x$graph$nodes[["$"]]$children) <- "wrong"
      x
    }),
    multichain_zero_m = list("multichain", function(x) {
      x$graph$nodes[["$"]]$fields$M <- 0L
      x
    }),
    multichain_infinite_temperature = list("multichain", function(x) {
      x$graph$nodes[["$"]]$fields$temperature <- Inf
      x
    }),
    refine_zero_n = list("refine", function(x) {
      x$graph$nodes[["$"]]$fields$N <- 0L
      x
    }),
    refine_nan_threshold = list("refine", function(x) {
      x$graph$nodes[["$"]]$fields$threshold <- NaN
      x
    }),
    refine_negative_fail_count = list("refine", function(x) {
      x$graph$nodes[["$"]]$fields$fail_count <- -1L
      x
    }),
    refine_na_feedback_field = list("refine", function(x) {
      x$graph$nodes[["$"]]$fields$feedback_field <- NA_character_
      x
    }),
    best_of_n_zero_n = list("best_of_n", function(x) {
      x$graph$nodes[["$"]]$fields$N <- 0L
      x
    }),
    assert_negative_retries = list("assert", function(x) {
      x$graph$nodes[["$"]]$fields$max_retries <- -1L
      x
    }),
    assert_unknown_failure_mode = list("assert", function(x) {
      x$graph$nodes[["$"]]$fields$on_failure <- "explode"
      x
    }),
    assert_na_message = list("assert", function(x) {
      x$graph$nodes[["$"]]$fields$assertions[[1]]$message <- NA_character_
      x
    }),
    knn_zero_k = list("knn", function(x) {
      x$graph$nodes[["$"]]$fields$k <- 0L
      x
    }),
    knn_na_merge = list("knn", function(x) {
      x$graph$nodes[["$"]]$fields$merge_demos <- NA
      x
    }),
    pot_zero_iterations = list("program_of_thought", function(x) {
      x$graph$nodes[["$"]]$fields$max_iters <- 0L
      x
    }),
    pot_na_extract = list("program_of_thought", function(x) {
      x$graph$nodes[["$"]]$fields$extract_answer <- NA
      x
    }),
    codeact_zero_iterations = list("codeact", function(x) {
      x$graph$nodes[["$"]]$fields$max_iterations <- 0L
      x
    }),
    rlm_na_verbose = list("rlm", function(x) {
      x$graph$nodes[["$"]]$fields$verbose <- NA
      x
    }),
    rag_zero_k = list("rag", function(x) {
      x$graph$nodes[["$"]]$fields$k <- 0L
      x
    }),
    rag_na_context = list("rag", function(x) {
      x$graph$nodes[["$"]]$fields$context_format <- NA_character_
      x
    }),
    predict_reachable_child = list("predict", function(x) {
      extra <- x$graph$nodes[["$"]]
      extra$id <- "$/extra"
      extra$path <- "$/extra"
      x$graph$nodes[["$"]]$children <- list(
        extra = list(.node = "$/extra")
      )
      x$graph$nodes[["$/extra"]] <- extra
      x$graph$edges <- list(list(
        from = "$",
        to = "$/extra",
        path = "$/extra"
      ))
      x
    })
  )

  for (name in names(cases)) {
    case <- cases[[name]]
    expect_artifact_manifest_rejected(
      case[[2]](fixtures[[case[[1]]]]),
      info = name
    )
  }
})

test_that("JSON-schema values use the closed JSON value domain", {
  schema <- ellmer::type_from_schema(
    text = '{"type":"object","properties":{"answer":{"type":"string"}}}'
  )
  artifact <- program_artifact(module(signature(
    inputs = list(input("text")),
    output_type = schema,
    instructions = "JSON"
  )))
  invalid_json <- list(
    1 + 2i,
    as.raw(1),
    NaN,
    Inf,
    NA_character_,
    structure(list("value"), names = NA_character_)
  )
  for (i in seq_along(invalid_json)) {
    malformed <- artifact
    malformed$graph$nodes[["$"]]$signature$output_type$json <- invalid_json[[i]]
    expect_artifact_manifest_rejected(malformed, info = paste("json", i))
  }
})

test_that("graph records require canonical node and edge order", {
  artifact <- program_artifact(
    pipeline(artifact_leaf(), artifact_leaf("answer", "summary"))
  )
  reversed_edges <- artifact
  reversed_edges$graph$edges <- rev(reversed_edges$graph$edges)
  expect_artifact_manifest_rejected(reversed_edges, "reversed edges")

  reversed_nodes <- artifact
  reversed_nodes$graph$nodes <- rev(reversed_nodes$graph$nodes)
  expect_artifact_manifest_rejected(reversed_nodes, "reversed nodes")
})

test_that("composite signatures are validated without resolving runtimes", {
  reward <- function(prediction, inputs) 1
  program <- best_of_n(artifact_leaf(), reward_fn = reward)
  artifact <- program_artifact(program, registry = list(reward = reward))
  artifact$graph$nodes[["$"]]$signature$instructions <- "forged"
  artifact <- artifact_rehash(artifact)

  condition <- rlang::catch_cnd(restore_module_config(
    artifact,
    registry = list()
  ))

  expect_s3_class(condition, "dsprrr_artifact_malformed")
  expect_false(inherits(condition, "dsprrr_artifact_registry_error"))
})

test_that("ensembles preserve the first signature with compatible input names", {
  first <- module(signature("x -> a"))
  second <- module(signature("x -> b"))
  program <- ensemble(list(first, second))

  restored <- restore_module_config(program_artifact(program))

  expect_identical(
    restored$signature@output_type@properties |> names(),
    "a"
  )
  expect_identical(
    restored$modules[[2]]$signature@output_type@properties |> names(),
    "b"
  )
})

test_that("declarative ellmer content round-trips through demos", {
  ContentJson <- get("ContentJson", envir = asNamespace("ellmer"))
  json_values <- list(
    data = ContentJson(data = list(a = 1), string = NULL),
    string = ContentJson(data = NULL, string = '{"a":1}'),
    both = ContentJson(data = list(a = 1), string = '{"a":2}'),
    vector = ContentJson(data = c(1, 2), string = NULL)
  )
  contents <- c(
    list(
      text = ellmer::ContentText("hello"),
      inline = ellmer::ContentImageInline("image/png", "YWJj"),
      remote = ellmer::ContentImageRemote("https://example.com/image.png"),
      pdf = ellmer::ContentPDF("application/pdf", "YWJj", "example.pdf")
    ),
    json_values
  )
  program <- artifact_leaf()
  program$demos <- list(list(text = contents, answer = "ok"))

  artifact <- program_artifact(program)
  restored <- restore_module_config(artifact)
  restored_contents <- restored$demos[[1]]$text

  expect_identical(
    vapply(restored_contents, function(x) class(x)[1], character(1)),
    vapply(contents, function(x) class(x)[1], character(1))
  )
  for (name in names(json_values)) {
    expect_identical(restored_contents[[name]]@data, json_values[[name]]@data)
    expect_identical(
      restored_contents[[name]]@string,
      json_values[[name]]@string
    )
    expect_identical(
      restored_contents[[name]]@parsed,
      json_values[[name]]@parsed
    )
  }
  expect_identical(
    program_artifact(restored)$integrity,
    artifact$integrity
  )
})

test_that("declarative content rejects unsafe URLs and malformed tags", {
  secret <- "ARTIFACT_REMOTE_URL_SECRET_SENTINEL"
  unsafe_remote_urls <- c(
    paste0("https://example.com/image.png?X-Amz-Signature=", secret),
    paste0("https://example.com/image.png?key=", secret),
    paste0("https://example.com/image.png?width=100&value=", secret),
    paste0("https://example.com/image.png#access_token=", secret),
    paste0("https://example.com/image.png#", secret),
    "https://example.com/image.png?width=100",
    paste0("https://cdn.example.com/s--", secret, "--/image.png"),
    paste0("https://cdn.example.com/s%2D%2D", secret, "%2D%2D/image.png"),
    paste0("https://cdn.example.com/signature/", secret, "/image.png"),
    paste0("https://cdn.example.com/token-", secret, "/image.png")
  )
  for (url in unsafe_remote_urls) {
    program <- artifact_leaf()
    program$demos <- list(list(
      text = ellmer::ContentImageRemote(url),
      answer = "no"
    ))
    condition <- rlang::catch_cnd(program_artifact(program))
    expect_s3_class(condition, "dsprrr_artifact_unsafe_value")
    expect_false(grepl(secret, conditionMessage(condition), fixed = TRUE))
  }

  credential_url <- artifact_leaf()
  credential_url$demos <- list(list(
    text = ellmer::ContentImageRemote(
      "https://user:password@example.com/image.png"
    ),
    answer = "no"
  ))
  expect_s3_class(
    rlang::catch_cnd(program_artifact(credential_url)),
    "dsprrr_artifact_unsafe_value"
  )

  for (url in c(
    "http://example.com/image.png",
    "https:///image.png"
  )) {
    insecure <- artifact_leaf()
    insecure$demos <- list(list(
      text = ellmer::ContentImageRemote(url),
      answer = "no"
    ))
    expect_s3_class(
      rlang::catch_cnd(program_artifact(insecure)),
      "dsprrr_artifact_unsafe_value"
    )
  }

  ContentJson <- get("ContentJson", envir = asNamespace("ellmer"))
  credential_json <- artifact_leaf()
  credential_json$demos <- list(list(
    text = ContentJson(data = list(api_key = "SECRET"), string = NULL),
    answer = "no"
  ))
  expect_s3_class(
    rlang::catch_cnd(program_artifact(credential_json)),
    "dsprrr_artifact_unsafe_value"
  )

  thinking <- artifact_leaf()
  thinking$demos <- list(list(
    text = ellmer::ContentThinking("runtime thought", list()),
    answer = "no"
  ))
  expect_s3_class(
    rlang::catch_cnd(program_artifact(thinking)),
    "dsprrr_artifact_unsafe_value"
  )

  safe <- artifact_leaf()
  safe$demos <- list(list(
    text = ellmer::ContentImageRemote("https://example.com/image.png"),
    answer = "ok"
  ))
  malformed <- program_artifact(safe)
  envelope <- malformed$graph$nodes[["$"]]$fields$demos[[1]]$text
  envelope$.dsprrr$payload$detail <- "secret"
  malformed$graph$nodes[["$"]]$fields$demos[[1]]$text <- envelope
  expect_artifact_manifest_rejected(malformed, "malformed content detail")

  signed <- program_artifact(safe)
  signed$graph$nodes[["$"]]$fields$demos[[1]]$text$.dsprrr$payload$url <-
    "https://example.com/image.png?token=SECRET"
  expect_artifact_manifest_rejected(signed, "tampered signed content URL")
})

test_that("versioned envelopes escape tag-like ordinary user lists", {
  builtin <- list(kind = "builtin", id = "reduce_first", args = list())
  registry <- list(
    kind = "registry",
    id = "value",
    interface_sha256 = paste(rep("a", 64), collapse = "")
  )
  trusted <- list(
    kind = "trusted",
    serialized = serialize("value", NULL),
    sha256 = paste(rep("b", 64), collapse = "")
  )
  payload <- list(
    content = list(.dsprrr_content = list(kind = "text", text = "ordinary")),
    builtin = list(.dsprrr_runtime = builtin),
    registry = list(.dsprrr_runtime = registry),
    trusted = list(.dsprrr_runtime = trusted),
    envelope = list(
      .dsprrr = list(
        version = 1L,
        kind = "runtime",
        payload = builtin
      )
    ),
    nested = list(
      value = list(
        .dsprrr = list(
          version = 1L,
          kind = "content",
          payload = list(kind = "text", text = "ordinary")
        )
      )
    )
  )
  program <- artifact_leaf()
  program$config$payload <- payload

  restored <- restore_module_config(program_artifact(program))

  expect_identical(restored$config$payload, payload)

  malformed <- program_artifact(program)
  malformed$graph$nodes[["$"]]$config$payload$envelope$.dsprrr$version <- 2L
  expect_artifact_manifest_rejected(malformed, "invalid envelope version")

  leak <- program_artifact(artifact_leaf())
  leak$graph$nodes[["$"]]$config$leak <- dsprrr:::artifact_envelope(
    "plain",
    list(
      nested = dsprrr:::artifact_envelope(
        "plain",
        list(api_key = "SECRET", prompt = "GENERATED_PROMPT")
      )
    )
  )
  leak <- artifact_rehash(leak)
  decode_calls <- 0L
  testthat::local_mocked_bindings(
    artifact_decode_runtime = function(...) {
      decode_calls <<- decode_calls + 1L
      cli::cli_abort("runtime decoder must not be reached")
    },
    .package = "dsprrr"
  )
  condition <- rlang::catch_cnd(restore_module_config(leak))
  expect_s3_class(condition, "dsprrr_artifact_unsafe_value")
  expect_identical(decode_calls, 0L)
})

test_that("arbitrary runtime objects honor registry and dual trusted opt-in", {
  RuntimeValue <- S7::new_class(
    "ArtifactRuntimeValue",
    properties = list(value = S7::class_character)
  )
  runtime_value <- RuntimeValue(value = "opaque")
  program <- artifact_leaf()
  program$config$runtime_value <- runtime_value

  expect_s3_class(
    rlang::catch_cnd(program_artifact(program)),
    "dsprrr_artifact_unsafe_value"
  )

  registered <- program_artifact(
    program,
    registry = list(runtime_value = runtime_value)
  )
  restored <- restore_module_config(
    registered,
    registry = list(runtime_value = runtime_value)
  )
  expect_identical(restored$config$runtime_value, runtime_value)

  embedded <- program_artifact(program, trusted = TRUE)
  expect_s3_class(
    rlang::catch_cnd(restore_module_config(embedded)),
    "dsprrr_artifact_unsafe_value"
  )
  trusted_restored <- restore_module_config(embedded, trusted = TRUE)
  expect_identical(trusted_restored$config$runtime_value, runtime_value)
})

test_that("generated optimizer prompts and demo payloads are excluded", {
  sentinel <- "SIMBA_GENERATED_SENTINEL"
  program <- artifact_leaf()
  program$config$optimizer <- list(
    steps = 3L,
    best_score = 0.9,
    n_rules = 1L,
    rules = sentinel,
    demos_added = list(list(text = sentinel, answer = sentinel))
  )

  artifact <- program_artifact(program)
  rendered <- paste(capture.output(dput(artifact)), collapse = "\n")
  exported <- export_module_code(program, include_demos = FALSE)
  optimizer <- artifact$graph$nodes[["$"]]$config$optimizer

  expect_false(grepl(sentinel, rendered, fixed = TRUE))
  expect_false(grepl(sentinel, exported, fixed = TRUE))
  expect_identical(optimizer$steps, 3L)
  expect_identical(optimizer$best_score, 0.9)
  expect_identical(optimizer$n_rules, 1L)
  expect_null(optimizer$rules)
  expect_null(optimizer$demos_added)
})

test_that("local files atomically replace and preserve the old target on failure", {
  path <- withr::local_tempfile(fileext = ".rds")
  unlink(path)
  first <- artifact_leaf()
  first$config$marker <- "first"
  second <- artifact_leaf()
  second$config$marker <- "second"

  expect_identical(save_program(first, path), path)
  expect_identical(load_program(path)$config$marker, "first")
  expect_identical(save_program(second, path), path)
  expect_identical(load_program(path)$config$marker, "second")

  testthat::local_mocked_bindings(
    artifact_atomic_replace = function(...) {
      cli::cli_abort(
        "Injected atomic replacement failure",
        class = "dsprrr_artifact_io_error"
      )
    },
    .package = "dsprrr"
  )
  replacement_error <- rlang::catch_cnd(save_program(first, path))
  expect_s3_class(replacement_error, "dsprrr_artifact_io_error")
  expect_match(conditionMessage(replacement_error), "Injected")
  expect_identical(load_program(path)$config$marker, "second")
  if (.Platform$OS.type == "unix") {
    expect_identical(as.character(as.octmode(file.info(path)$mode)), "600")
  }
})

test_that("atomic replacement is a guarded same-directory filesystem move", {
  directory <- withr::local_tempdir()
  destination <- file.path(directory, "published.txt")
  source <- file.path(directory, "staged.txt")
  writeLines("old", destination)
  writeLines("new", source)

  expect_identical(
    dsprrr:::artifact_atomic_replace(source, destination, "test artifact"),
    destination
  )
  expect_identical(readLines(destination), "new")
  expect_identical(file.exists(source), FALSE)

  writeLines("verified-old", destination)
  writeLines("unpublished", source)
  before <- readBin(destination, "raw", n = file.info(destination)$size)
  testthat::local_mocked_bindings(
    artifact_file_move = function(...) {
      cli::cli_abort("injected filesystem move failure")
    },
    .package = "dsprrr"
  )

  condition <- rlang::catch_cnd(
    dsprrr:::artifact_atomic_replace(source, destination, "test artifact")
  )

  expect_s3_class(condition, "dsprrr_artifact_io_error")
  expect_identical(
    readBin(destination, "raw", n = file.info(destination)$size),
    before
  )
  expect_identical(readLines(source), "unpublished")
})

test_that("atomic replacement rejects cross-directory publication", {
  source_dir <- withr::local_tempdir()
  destination_dir <- withr::local_tempdir()
  source <- file.path(source_dir, "staged.txt")
  destination <- file.path(destination_dir, "published.txt")
  writeLines("new", source)
  writeLines("old", destination)

  condition <- rlang::catch_cnd(
    dsprrr:::artifact_atomic_replace(source, destination, "test artifact")
  )

  expect_s3_class(condition, "dsprrr_artifact_io_error")
  expect_match(conditionMessage(condition), "canonical directory")
  expect_identical(readLines(destination), "old")
  expect_identical(readLines(source), "new")
})

test_that("atomic replacement rejects hard-link aliases without mutation", {
  directory <- withr::local_tempdir()
  source <- file.path(directory, "staged.txt")
  destination <- file.path(directory, "published.txt")
  writeLines("staged-content", source)
  linked <- suppressWarnings(file.link(source, destination))
  skip_if_not(isTRUE(linked), "hard links are unavailable")
  before_source <- readBin(source, "raw", n = file.info(source)$size)
  before_destination <- readBin(
    destination,
    "raw",
    n = file.info(destination)$size
  )
  source_identity <- dsprrr:::artifact_atomic_identity(source)
  destination_identity <- dsprrr:::artifact_atomic_identity(destination)

  condition <- rlang::catch_cnd(
    dsprrr:::artifact_atomic_replace(source, destination, "test artifact")
  )

  expect_s3_class(condition, "dsprrr_artifact_io_error")
  expect_match(conditionMessage(condition), "same existing file")
  expect_identical(
    dsprrr:::artifact_atomic_identity(source),
    source_identity
  )
  expect_identical(
    dsprrr:::artifact_atomic_identity(destination),
    destination_identity
  )
  expect_identical(
    readBin(source, "raw", n = file.info(source)$size),
    before_source
  )
  expect_identical(
    readBin(destination, "raw", n = file.info(destination)$size),
    before_destination
  )
})

test_that("atomic replacement rejects case aliases where supported", {
  directory <- withr::local_tempdir()
  source <- file.path(directory, "staged.txt")
  destination <- file.path(directory, "STAGED.txt")
  writeLines("staged-content", source)
  skip_if_not(
    file.exists(destination),
    "the filesystem is case-sensitive"
  )
  source_identity <- dsprrr:::artifact_atomic_identity(source)
  destination_identity <- dsprrr:::artifact_atomic_identity(destination)
  skip_if_not(
    dsprrr:::artifact_atomic_same_file(
      source_identity,
      destination_identity
    ),
    "case variants do not identify the same file"
  )
  before <- readBin(source, "raw", n = file.info(source)$size)

  condition <- rlang::catch_cnd(
    dsprrr:::artifact_atomic_replace(source, destination, "test artifact")
  )

  expect_s3_class(condition, "dsprrr_artifact_io_error")
  expect_match(conditionMessage(condition), "same existing file")
  expect_identical(
    dsprrr:::artifact_atomic_identity(source),
    source_identity
  )
  expect_identical(
    dsprrr:::artifact_atomic_identity(destination),
    destination_identity
  )
  expect_identical(
    readBin(source, "raw", n = file.info(source)$size),
    before
  )
  expect_identical(
    readBin(destination, "raw", n = file.info(destination)$size),
    before
  )
})

test_that("artifact staging is private before any RDS content is written", {
  skip_if(.Platform$OS.type != "unix")
  writer <- dsprrr:::artifact_write_rds
  observed_mode <- NULL
  testthat::local_mocked_bindings(
    artifact_write_rds = function(value, path) {
      observed_mode <<- as.character(as.octmode(file.info(path)$mode))
      writer(value, path)
    },
    .package = "dsprrr"
  )
  path <- withr::local_tempfile(fileext = ".rds")
  unlink(path)

  save_program(artifact_leaf(), path)

  expect_identical(observed_mode, "600")
})

test_that("code-export staging is private before content is written", {
  skip_if(.Platform$OS.type != "unix")
  writer <- dsprrr:::artifact_write_lines
  observed_mode <- NULL
  testthat::local_mocked_bindings(
    artifact_write_lines = function(lines, path) {
      observed_mode <<- as.character(as.octmode(file.info(path)$mode))
      writer(lines, path)
    },
    .package = "dsprrr"
  )
  path <- withr::local_tempfile(fileext = ".R")
  unlink(path)

  export_module_code(artifact_leaf(), file = path)

  expect_identical(observed_mode, "600")
})

test_that("malformed, unsupported, and corrupt artifacts fail with typed errors", {
  artifact <- program_artifact(artifact_leaf())

  malformed <- artifact
  malformed$graph$nodes <- list()
  malformed$integrity <- dsprrr:::artifact_integrity(malformed)
  expect_snapshot(
    restore_module_config(malformed),
    error = TRUE
  )
  expect_s3_class(
    rlang::catch_cnd(restore_module_config(malformed)),
    "dsprrr_artifact_malformed"
  )

  unsupported <- artifact
  unsupported$format_version <- 999L
  expect_snapshot(
    restore_module_config(unsupported),
    error = TRUE
  )
  expect_s3_class(
    rlang::catch_cnd(restore_module_config(unsupported)),
    "dsprrr_artifact_unsupported_version"
  )

  corrupt <- artifact
  corrupt$graph$nodes[["$"]]$fields$template <- "tampered"
  expect_snapshot(
    restore_module_config(corrupt),
    error = TRUE
  )
  expect_s3_class(
    rlang::catch_cnd(restore_module_config(corrupt)),
    "dsprrr_artifact_integrity_error"
  )

  injected_config <- artifact
  injected_config$graph$nodes[["$"]]$config$api_key <- "INJECTED"
  injected_config$integrity <- dsprrr:::artifact_integrity(injected_config)
  expect_s3_class(
    rlang::catch_cnd(restore_module_config(injected_config)),
    "dsprrr_artifact_unsafe_value"
  )

  injected_state <- artifact
  injected_state$graph$nodes[["$"]]$state$traces <- list(prompt = "INJECTED")
  injected_state$integrity <- dsprrr:::artifact_integrity(injected_state)
  expect_s3_class(
    rlang::catch_cnd(restore_module_config(injected_state)),
    "dsprrr_artifact_malformed"
  )

  injected_demo <- artifact
  injected_demo$graph$nodes[["$"]]$fields$demos <- list(
    list(text = "safe", password = "INJECTED")
  )
  injected_demo$integrity <- dsprrr:::artifact_integrity(injected_demo)
  expect_s3_class(
    rlang::catch_cnd(restore_module_config(injected_demo)),
    "dsprrr_artifact_unsafe_value"
  )

  duplicate <- append(
    artifact,
    list(metadata = list(client_secret = "INJECTED"))
  )
  class(duplicate) <- class(artifact)
  expect_s3_class(
    rlang::catch_cnd(restore_module_config(duplicate)),
    "dsprrr_artifact_malformed"
  )

  metadata_leak <- artifact
  metadata_leak$metadata$client_secret <- "DO_NOT_PERSIST"
  metadata_leak$integrity <- dsprrr:::artifact_integrity(metadata_leak)
  expect_snapshot(
    restore_module_config(metadata_leak),
    error = TRUE
  )

  unreachable <- artifact
  extra <- unreachable$graph$nodes[["$"]]
  extra$id <- "$/unused"
  extra$path <- "$/unused"
  unreachable$graph$nodes[["$/unused"]] <- extra
  unreachable$integrity <- dsprrr:::artifact_integrity(unreachable)
  expect_snapshot(
    restore_module_config(unreachable),
    error = TRUE
  )

  dependency <- artifact
  dependency$metadata$packages$ellmer <- "999.0.0"
  dependency$integrity <- dsprrr:::artifact_integrity(dependency)
  expect_snapshot(
    restore_module_config(dependency),
    error = TRUE
  )

  path <- withr::local_tempfile(lines = "not an RDS artifact")
  expect_snapshot(load_program(path), error = TRUE)
})

test_that("only the current artifact schema reaches constructors", {
  legacy <- list(
    format_version = 2L,
    module_kind = "predict",
    signature = list(),
    config = list(),
    state = list(),
    fields = list(),
    metadata = list()
  )
  constructor_called <- FALSE
  testthat::local_mocked_bindings(
    artifact_build_node = function(...) {
      constructor_called <<- TRUE
      cli::cli_abort("unexpected artifact construction")
    },
    .package = "dsprrr"
  )

  condition <- rlang::catch_cnd(restore_module_config(legacy))
  expect_s3_class(condition, "dsprrr_artifact_malformed")
  expect_false(constructor_called)

  probe <- new.env(parent = emptyenv())
  probe$called <- FALSE
  method_name <- "$.artifact_evil"
  had_method <- exists(method_name, envir = globalenv(), inherits = FALSE)
  old_method <- if (had_method) get(method_name, envir = globalenv()) else NULL
  assign(
    method_name,
    function(x, name) {
      probe$called <- TRUE
      cli::cli_abort("unexpected S3 dispatch")
    },
    envir = globalenv()
  )
  withr::defer({
    if (had_method) {
      assign(method_name, old_method, envir = globalenv())
    } else {
      rm(list = method_name, envir = globalenv())
    }
  })
  class(legacy) <- c("artifact_evil", "list")

  condition <- rlang::catch_cnd(restore_module_config(legacy))
  expect_s3_class(condition, "dsprrr_artifact_malformed")
  expect_false(probe$called)
  expect_false(constructor_called)
})

test_that("cycles and unsupported custom classes fail explicitly", {
  cycle <- ArtifactCycleProgram$new()
  cycle$children$self <- cycle
  expect_snapshot(program_artifact(cycle), error = TRUE)

  custom <- ArtifactCycleProgram$new()
  expect_snapshot(program_artifact(custom), error = TRUE)
})

test_that("tampered composite signatures fail validation", {
  program <- pipeline(artifact_leaf(), artifact_leaf("answer", "summary"))
  artifact <- program_artifact(program)

  tampered <- artifact
  tampered$graph$nodes[[
    "$"
  ]]$signature <- dsprrr:::artifact_serialize_signature(
    signature("different -> output"),
    registry = list(),
    trusted = FALSE,
    exclusions = local({
      result <- new.env(parent = emptyenv())
      result$records <- list()
      result
    }),
    path = "graph.nodes.$.signature"
  )
  tampered$integrity <- dsprrr:::artifact_integrity(tampered)
  expect_snapshot(restore_module_config(tampered), error = TRUE)
})

test_that("pins transports the exact graph manifest", {
  skip_if_not_installed("pins")
  shared <- artifact_leaf()
  program <- ensemble(list(left = shared, right = shared))
  board <- pins::board_temp()

  pin_module_config(board, "program", program)
  artifact <- pins::pin_read(board, "program")
  restored <- restore_module_config(artifact)

  expect_identical(artifact$format, "dsprrr-program")
  expect_identical(artifact$format_version, 4L)
  expect_identical(
    artifact$integrity,
    dsprrr:::artifact_integrity(artifact)
  )
  expect_identical(restored$modules$left, restored$modules$right)
})

test_that("module code export restores from the complete manifest", {
  program <- artifact_leaf()
  program$config$complex <- "quote \" newline\n slash \\"
  program$demos <- list(list(text = "demo", answer = "kept"))
  program$state$compiled <- TRUE
  code <- export_module_code(program, name = "restored")
  environment <- new.env(parent = globalenv())
  sentinel_binding <- new.env(parent = emptyenv())
  environment$.dsprrr_artifact <- sentinel_binding

  expect_no_error(eval(parse(text = code), envir = environment))
  expect_identical(environment$.dsprrr_artifact, sentinel_binding)
  expect_identical(environment$restored$config$complex, program$config$complex)
  expect_identical(environment$restored$demos, program$demos)
  expect_identical(environment$restored$is_compiled(), TRUE)

  without_demos <- export_module_code(
    program,
    name = "restored",
    include_demos = FALSE
  )
  environment <- new.env(parent = globalenv())
  expect_no_error(eval(parse(text = without_demos), envir = environment))
  expect_length(environment$restored$demos, 0L)

  path <- withr::local_tempfile(fileext = ".R")
  unlink(path)
  expect_message(export_module_code(program, file = path), "written to")
  expect_no_error(parse(path))
  expect_message(
    export_module_code(program, name = "replacement", file = path),
    "written to"
  )
  expect_true(any(grepl(
    "replacement <- local({",
    readLines(path),
    fixed = TRUE
  )))

  reserved <- export_module_code(program, name = ".dsprrr_artifact")
  isolated <- new.env(parent = baseenv())
  expect_no_error(eval(parse(text = reserved), envir = isolated))
  expect_s3_class(isolated$.dsprrr_artifact, "Module")
  expect_match(code, "dsprrr::restore_module_config", fixed = TRUE)

  forward <- function(text, ...) list(answer = text)
  callable <- module_fn("text -> answer", forward)
  expect_snapshot(
    export_module_code(callable, registry = list(forward = forward)),
    error = TRUE
  )
})
