test_that("history_abort escapes braces in untrusted error text", {
  err <- expect_error(
    dsprrr:::history_abort("Invalid field `{stop('boom')}` in JSON"),
    class = "dsprrr_history_error"
  )
  expect_match(conditionMessage(err), "\\{stop\\('boom'\\)\\}")
})

test_that("NA_character_ is rejected by string validators using nzchar()", {
  expect_error(
    stream_listener(NA_character_, identity),
    "single non-empty string"
  )
  expect_error(
    dsprrr:::write_history_transcript(list(), file = NA_character_),
    "file path or writable connection"
  )
  res <- dsprrr:::prepare_cache_directory(NA_character_)
  expect_false(res$ok)

  expect_error(
    dsprrr:::Assertion(condition = is.character, message = NA_character_),
    "single character string"
  )
  expect_error(
    dsprrr:::Assertion(condition = is.character, field = NA_character_),
    "single character string or NULL"
  )
  expect_error(
    dsprrr:::Assertion(condition = is.character, type = NA_character_),
    "single character string"
  )
  expect_error(
    module(signature("x -> y"), template = NA_character_),
    "single character string"
  )
})

test_that("session_cost handles NA_character_ models without corrupting by_model", {
  clear_prompt_history()
  on.exit(clear_prompt_history(), add = TRUE)

  env <- dsprrr:::.dsprrr_env
  env$prompt_history <- list(
    list(
      timestamp = Sys.time(),
      source = "PredictModule",
      model = NA_character_,
      tokens_in = 10L,
      tokens_out = 5L,
      cost = 0.01,
      duration_s = 0.1
    ),
    list(
      timestamp = Sys.time(),
      source = "PredictModule",
      model = "gpt-test",
      tokens_in = 20L,
      tokens_out = 8L,
      cost = 0.02,
      duration_s = 0.2
    )
  )

  cost <- session_cost()
  expect_equal(nrow(cost$by_model), 2L)
  unknown_row <- cost$by_model[cost$by_model$model == "unknown", ]
  expect_equal(unknown_row$n_calls, 1L)
  expect_equal(unknown_row$tokens_in, 10L)
  expect_equal(unknown_row$tokens_out, 5L)
  expect_equal(unknown_row$cost, 0.01)
})

test_that("dsprrr_sitrep respects prompt_history_max option and reports invalid default_chat", {
  withr::local_options(list(
    dsprrr.prompt_history_max = 42L,
    dsprrr.default_chat = "not-a-chat"
  ))

  status <- suppressMessages(dsprrr_sitrep())
  expect_equal(status$prompt_history_max, 42L)
  expect_false(status$has_default_chat)
  expect_true(rlang::is_string(status$chat_error))
  expect_true(nzchar(status$chat_error))
})

test_that("inspect_history(n = 0) returns empty tibble and format_output_for_history returns scalar", {
  clear_prompt_history()
  on.exit(clear_prompt_history(), add = TRUE)

  env <- dsprrr:::.dsprrr_env
  env$prompt_history <- list(
    list(
      timestamp = Sys.time(),
      source = "PredictModule",
      model = "gpt-test",
      tokens_in = 1L,
      tokens_out = 1L,
      cost = 0,
      duration_s = 0.1,
      prompt = "hi",
      response = "hello"
    )
  )

  empty <- inspect_history(n = 0)
  expect_s3_class(empty, "tbl_df")
  expect_equal(nrow(empty), 0L)

  formatted_vec <- dsprrr:::format_output_for_history(c("a", "b", "c"))
  expect_identical(formatted_vec, "a, b, c")
  expect_identical(
    dsprrr:::format_output_for_history(character()),
    NA_character_
  )
})

test_that("ContentPDF fingerprint and replay preserve url property", {
  pdf_a <- ellmer::ContentPDF(
    type = "application/pdf",
    data = "AAAA",
    filename = "doc.pdf",
    url = "https://example.com/a.pdf"
  )
  pdf_b <- ellmer::ContentPDF(
    type = "application/pdf",
    data = "AAAA",
    filename = "doc.pdf",
    url = "https://example.com/b.pdf"
  )

  fp_a <- dsprrr:::cache_content_fingerprint(pdf_a)
  fp_b <- dsprrr:::cache_content_fingerprint(pdf_b)
  expect_false(identical(fp_a, fp_b))

  replayed <- dsprrr:::cache_replay_content(pdf_a)
  expect_identical(replayed@url, "https://example.com/a.pdf")
})

test_that("signature parser accepts DSPy numeric and string bounds without crashing", {
  sig_num <- signature("text -> score: float(0, 1)")
  expect_s3_class(sig_num, "dsprrr::Signature")
  expect_true(inherits(sig_num@output_type, "ellmer::TypeObject"))
  expect_true(inherits(
    sig_num@output_type@properties$score,
    "ellmer::TypeBasic"
  ))

  sig_str <- signature("text -> summary: string[10, 100]")
  expect_s3_class(sig_str, "dsprrr::Signature")
  expect_true(inherits(
    sig_str@output_type@properties$summary,
    "ellmer::TypeBasic"
  ))
})

test_that("Module$print handles list-valued config entries without error", {
  mod <- module(
    signature("x -> y"),
    config = list(params = list(temperature = 0.2), tag = "v1")
  )
  expect_no_error(suppressMessages(print(mod)))
})

test_that("PipelineModule$deepcopy, FnModule$reset_copy, and KNNFewShotModule$reset_copy preserve types and config", {
  fn_mod <- module_fn("x -> y", function(x) list(y = toupper(x)))
  fn_mod$config$tag <- "keep"
  fn_reset <- fn_mod$reset_copy()
  expect_s3_class(fn_reset, "FnModule")
  expect_equal(fn_reset$config, list())

  inner <- module(signature("x -> y"), config = list(tag = "inner"))
  wrapped <- best_of_n(inner, N = 2L, reward_fn = function(x, ...) 1)
  wrapped$config$custom <- "preserve-me"
  pipe_mod <- pipeline(wrapped)
  pipe_copy <- pipe_mod$deepcopy()
  expect_identical(pipe_copy$steps[[1]]@module$config$custom, "preserve-me")

  train_emb <- matrix(c(1, 0, 0, 1), nrow = 2, byrow = TRUE)
  knn_mod <- dsprrr:::KNNFewShotModule$new(
    module = inner,
    k = 1L,
    vectorizer = function(x) {
      matrix(c(1, 0), nrow = length(x), ncol = 2, byrow = TRUE)
    },
    input_text = function(row) as.character(row$x),
    train_embeddings = train_emb,
    trainset_demos = list(
      list(inputs = list(x = "a"), output = list(y = "A")),
      list(inputs = list(x = "b"), output = list(y = "B"))
    )
  )
  knn_reset <- knn_mod$reset_copy()
  expect_s3_class(knn_reset, "KNNFewShotModule")
  expect_identical(knn_reset$k, 1L)
  expect_no_error(knn_mod$reset(hard = TRUE))
})

test_that("assert_length works with non-integer numeric bounds", {
  a <- assert_length("answer", min = 1.5, max = 10.5)
  expect_match(a@message, "1.5 and 10.5")
  expect_true(a@condition(list(answer = "hello")))
})

test_that("BootstrapFewShotWithRandomSearch and MIPROv2 accept numeric double seeds", {
  sig <- signature("q -> a")
  mod <- module(sig)
  train <- tibble::tibble(q = c("1+1", "2+2"), a = c("2", "4"))
  mock_llm <- new_test_chat(chat_structured = function(...) list(a = "2"))

  tp_rs <- BootstrapFewShotWithRandomSearch(
    metric = metric_exact_match(field = "a"),
    num_candidate_programs = 2L,
    max_bootstrapped_demos = 0L,
    max_labeled_demos = 1L,
    seed = 42
  )
  expect_no_error(compile(
    mod,
    tp_rs,
    train,
    valset = train,
    .llm = mock_llm,
    .cache = FALSE
  ))
})

test_that("GridSearchTeleprompter handles empty trainset gracefully", {
  mod <- module(signature("q -> a"))
  tp <- GridSearchTeleprompter(
    variants = tibble::tibble(id = "v1", instructions_suffix = "Be brief"),
    metric = metric_exact_match(field = "a")
  )
  empty_train <- tibble::tibble(q = character(), a = character())
  expect_warning(
    res <- compile(mod, tp, empty_train),
    "Empty trainset"
  )
  expect_s3_class(res, "Module")
})

test_that("evaluate() returns classed dsprrr_evaluation on empty data", {
  mod <- module_fn("q -> a", function(q) list(a = q))
  empty_df <- tibble::tibble(q = character(), a = character())
  expect_warning(
    res <- evaluate(mod, empty_df, metric = metric_exact_match(field = "a")),
    "Empty data"
  )
  expect_s3_class(res, "dsprrr_evaluation")
})

test_that("module_trials uses non-NA trial count for std_error", {
  mod <- module(signature("q -> a"))
  mod$state$trials <- tibble::tibble(
    trial_id = 1:4,
    score = c(0.2, 0.8, NA_real_, NA_real_),
    parameters = list(list(a = 1), list(a = 2), list(a = 3), list(a = 4))
  )
  summary <- module_trials(mod)
  expected_se <- stats::sd(c(0.2, 0.8)) / sqrt(2)
  expect_equal(summary$std_error, expected_se)
})

test_that("sample_dataset supports n > nrow(dataset) when replace = TRUE", {
  df <- tibble::tibble(x = 1:3)
  sampled <- sample_dataset(df, n = 10L, replace = TRUE, seed = 1L)
  expect_equal(nrow(sampled), 10L)
})

test_that("module_metrics computes per-trial stats and yardstick metrics (classification & regression)", {
  skip_if_not_installed("yardstick")

  # Classification with bare symbols and character names
  mod_cls <- module(signature("text -> sentiment"))
  mock_cls <- new_test_chat(
    chat_structured = function(prompt, ...) {
      list(sentiment = if (grepl("good", prompt)) "pos" else "neg")
    }
  )
  dev_cls <- tibble::tibble(
    text = c("good day", "bad day"),
    target = c("pos", "neg")
  )
  optimize_grid(
    mod_cls,
    data = dev_cls,
    metric = function(pred, row) as.numeric(pred$sentiment == row$target),
    parameters = list(instructions_suffix = c("v1", "v2")),
    .llm = mock_cls,
    control = list(progress = FALSE),
    .cache = FALSE
  )

  m_chr <- module_metrics(
    mod_cls,
    metrics = list(yardstick::accuracy),
    truth = "target",
    estimate = "sentiment"
  )
  expect_equal(m_chr$n_evaluated, c(2L, 2L))
  expect_equal(m_chr$median_score, c(1, 1))
  expect_s3_class(m_chr$yardstick[[1]], "tbl_df")
  expect_equal(m_chr$yardstick[[1]]$.estimate, 1)

  # Bare symbols also work
  m_sym <- module_metrics(
    mod_cls,
    metrics = yardstick::accuracy,
    truth = target,
    estimate = sentiment
  )
  expect_s3_class(m_sym$yardstick[[1]], "tbl_df")
  expect_equal(m_sym$yardstick[[1]]$.estimate, 1)

  # Numeric regression metrics stay numeric (not coerced to factor)
  mod_reg <- module(signature("x -> score: number"))
  mock_reg <- new_test_chat(
    chat_structured = function(...) list(score = 2.0)
  )
  dev_reg <- tibble::tibble(x = c("a", "b"), target = c(1.0, 3.0))
  optimize_grid(
    mod_reg,
    data = dev_reg,
    metric = function(pred, row) -abs(pred$score - row$target),
    parameters = list(instructions_suffix = c("v1")),
    .llm = mock_reg,
    control = list(progress = FALSE),
    .cache = FALSE
  )
  m_reg <- module_metrics(
    mod_reg,
    metrics = yardstick::rmse,
    truth = target,
    estimate = score
  )
  expect_s3_class(m_reg$yardstick[[1]], "tbl_df")
  expect_equal(m_reg$yardstick[[1]]$.estimate, 1.0)
})

test_that("as_vitals_cost handles NA total cost and summarize_traces_df includes token_breakdown when empty", {
  na_cost <- structure(
    list(costs = tibble::tibble(row = 1L, cost = NA_real_), total = NA_real_),
    class = "dsprrr_cost_summary"
  )
  res <- expect_no_error(as_vitals_cost(na_cost))
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1L)
  expect_equal(res$price, "$0.00")

  empty_summary <- summarize_traces_df(tibble::tibble(
    input_tokens = integer(),
    output_tokens = integer(),
    total_tokens = integer(),
    latency_ms = numeric(),
    cost = numeric()
  ))
  expect_true("token_breakdown" %in% names(empty_summary))
  expect_equal(empty_summary$token_breakdown$input, 0L)
})

test_that("parsnip print.llm_predict detects namespaced tune() and fit_llm_predict handles quotes in factor levels", {
  skip_if_not_installed("parsnip")

  spec <- llm_predict(
    signature = "text -> sentiment",
    temperature = parsnip::tune()
  )
  printed <- cli::cli_fmt(print(spec))
  expect_true(any(grepl("Tuned:.*temperature", printed)))

  fitted <- fit_llm_predict(
    x = data.frame(text = c("a", "b")),
    y = factor(c("it's good", "don't buy"))
  )
  expect_s3_class(fitted, "Module")
  expect_equal(
    fitted$signature@output_type@properties$output@values,
    c("don't buy", "it's good")
  )
})

test_that("format_search_results handles data frames with list columns", {
  df <- tibble::tibble(
    id = 1:2,
    meta = list(list(a = 1), list(b = 2))
  )
  out <- expect_no_error(dsprrr:::format_search_results(df))
  expect_match(out, "\\[Result 1\\]")
  expect_match(out, "\\[Result 2\\]")
})
