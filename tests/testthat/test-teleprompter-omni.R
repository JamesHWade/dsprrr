OmniMarkingTeleprompter <- S7::new_class(
  "OmniMarkingTeleprompter",
  parent = Teleprompter,
  properties = list(
    marker = S7::new_property(S7::class_character),
    append = S7::new_property(S7::class_logical, default = FALSE)
  )
)

S7::method(compile, list(S7::class_any, OmniMarkingTeleprompter)) <- function(
  program,
  teleprompter,
  trainset,
  ...
) {
  compiled <- dsprrr:::copy_module(program)
  input_marker <- compiled$config$marker
  compiled$config$input_marker <- input_marker
  compiled$config$marker <- if (teleprompter@append) {
    paste0(input_marker, teleprompter@marker)
  } else {
    teleprompter@marker
  }
  compiled$config$compiled <- TRUE
  compiled$config$teleprompter <- "OmniMarkingTeleprompter"
  compiled$state$compiled <- TRUE
  compiled
}

OmniFailingTeleprompter <- S7::new_class(
  "OmniFailingTeleprompter",
  parent = Teleprompter
)

S7::method(compile, list(S7::class_any, OmniFailingTeleprompter)) <- function(
  program,
  teleprompter,
  trainset,
  ...
) {
  cli::cli_abort("intentional Omni explorer failure")
}

OmniWorkerLlmTeleprompter <- S7::new_class(
  "OmniWorkerLlmTeleprompter",
  parent = Teleprompter,
  properties = list(
    marker = S7::new_property(S7::class_character)
  )
)

S7::method(compile, list(S7::class_any, OmniWorkerLlmTeleprompter)) <- function(
  program,
  teleprompter,
  trainset,
  .llm = NULL,
  ...
) {
  if (!inherits(.llm, "Chat")) {
    cli::cli_abort("Omni worker did not create a local Chat")
  }
  compiled <- dsprrr:::copy_module(program)
  compiled$config$input_marker <- compiled$config$marker
  compiled$config$marker <- teleprompter@marker
  compiled$config$compiled <- TRUE
  compiled$config$teleprompter <- "OmniWorkerLlmTeleprompter"
  compiled$state$compiled <- TRUE
  compiled
}

make_omni_mock_module <- function(marker = "seed") {
  OmniMockModule <- R6::R6Class(
    "OmniMockModule",
    inherit = dsprrr:::Module,
    public = list(
      initialize = function(marker = "seed") {
        sig <- Signature(
          inputs = list(input("x", "string")),
          output_type = ellmer::type_string(),
          instructions = "Return the configured marker"
        )
        super$initialize(sig, config = list(marker = marker))
      },
      forward = function(batch, .llm = NULL, trace = TRUE, ...) {
        tibble::tibble(
          output = list(self$config$marker),
          chat = list(NULL),
          metadata = list(list(marker = self$config$marker))
        )
      },
      deepcopy = function() {
        new_module <- make_omni_mock_module(self$config$marker)
        new_module$config <- lapply(self$config, function(x) x)
        new_module$state <- lapply(self$state, function(x) x)
        new_module
      }
    )
  )

  OmniMockModule$new(marker = marker)
}

omni_metric <- function(prediction, expected_row) {
  scores <- c(seed = 0, a = 0.2, b = 0.8, `b-c` = 1, bad = 0.1)
  unname(scores[[as.character(prediction)]] %||% 0)
}

make_omni <- function(continuation = NULL, parallel = FALSE, seed = NULL) {
  if (is.null(continuation)) {
    continuation <- OmniMarkingTeleprompter(marker = "-c", append = TRUE)
  }
  Omni(
    metric = omni_metric,
    explorers = list(
      a = OmniMarkingTeleprompter(marker = "a"),
      b = OmniMarkingTeleprompter(marker = "b")
    ),
    continuation = continuation,
    parallel = parallel,
    seed = seed,
    verbose = FALSE
  )
}

omni_trainset <- data.frame(
  x = c("train-a", "train-b"),
  target = c("unused", "unused")
)
omni_valset <- data.frame(x = "validation", target = "unused")

test_that("Omni validates its optimizer surface", {
  expect_snapshot(
    error = TRUE,
    Omni(
      metric = omni_metric,
      explorers = list(a = OmniMarkingTeleprompter(marker = "a")),
      continuation = OmniMarkingTeleprompter(marker = "-c", append = TRUE)
    )
  )

  expect_snapshot(
    error = TRUE,
    Omni(
      metric = omni_metric,
      explorers = list(
        OmniMarkingTeleprompter(marker = "a"),
        OmniMarkingTeleprompter(marker = "b")
      ),
      continuation = OmniMarkingTeleprompter(marker = "-c", append = TRUE)
    )
  )

  expect_snapshot(
    error = TRUE,
    make_omni(seed = 1.5)
  )

  expect_snapshot(
    error = TRUE,
    make_omni(seed = .Machine$integer.max + 1)
  )
})

test_that("Omni explores from one seed and continues from the winner", {
  compiled <- compile(
    make_omni_mock_module(),
    make_omni(),
    omni_trainset,
    valset = omni_valset
  )

  expect_equal(compiled$config$marker, "b-c")
  expect_equal(compiled$config$teleprompter, "Omni")
  result <- optimization_result(compiled)
  details <- result$extensions$omni
  expect_equal(details$exploration_winner, "b")
  expect_equal(details$best_phase, "continue")
  expect_equal(
    details$best_optimizer,
    "dsprrr::OmniMarkingTeleprompter"
  )

  candidates <- details$candidate_programs
  expect_equal(
    candidates$optimizer,
    c("baseline", "a", "b", "dsprrr::OmniMarkingTeleprompter")
  )
  expect_equal(candidates$score, c(0, 0.2, 0.8, 1))
  expect_equal(which(candidates$selected), 4L)
  expect_equal(candidates$program_config[[2]]$input_marker, "seed")
  expect_equal(candidates$program_config[[3]]$input_marker, "seed")
  expect_equal(candidates$program_config[[4]]$input_marker, "b")
})

test_that("Omni preserves the best program when continuation regresses", {
  tp <- make_omni(
    continuation = OmniMarkingTeleprompter(marker = "bad")
  )
  compiled <- compile(
    make_omni_mock_module(),
    tp,
    omni_trainset,
    valset = omni_valset
  )

  expect_equal(compiled$config$marker, "b")
  details <- optimization_result(compiled)$extensions$omni
  expect_equal(details$best_phase, "explore")
  expect_equal(details$best_optimizer, "b")
})

test_that("Omni isolates explorer failures", {
  tp <- Omni(
    metric = omni_metric,
    explorers = list(
      broken = OmniFailingTeleprompter(),
      b = OmniMarkingTeleprompter(marker = "b")
    ),
    continuation = OmniMarkingTeleprompter(marker = "-c", append = TRUE),
    verbose = FALSE
  )

  compiled <- NULL
  expect_snapshot(
    compiled <- compile(
      make_omni_mock_module(),
      tp,
      omni_trainset,
      valset = omni_valset
    )
  )

  details <- optimization_result(compiled)$extensions$omni
  expect_equal(compiled$config$marker, "b-c")
  expect_equal(
    details$candidate_programs$error[[2]],
    "intentional Omni explorer failure"
  )
  expect_equal(
    details$flag_compilation_error_occurred,
    TRUE
  )
})

test_that("Omni requires comparison data", {
  expect_snapshot(
    error = TRUE,
    compile(
      make_omni_mock_module(),
      make_omni(),
      data.frame(x = "only-row", target = "unused")
    )
  )
})

test_that("Omni validates per-optimizer compile arguments", {
  expect_snapshot(
    error = TRUE,
    compile(
      make_omni_mock_module(),
      make_omni(),
      omni_trainset,
      valset = omni_valset,
      explorer_compile_args = list(
        missing = list(extra = TRUE)
      )
    )
  )

  expect_snapshot(
    error = TRUE,
    compile(
      make_omni_mock_module(),
      make_omni(),
      omni_trainset,
      valset = omni_valset,
      continuation_compile_args = list(trainset = omni_trainset)
    )
  )

  expect_snapshot(
    error = TRUE,
    compile(
      make_omni_mock_module(),
      make_omni(),
      omni_trainset,
      valset = omni_valset,
      continuation_compile_args = list(TRUE)
    )
  )
})

test_that("Omni supports mirai exploration without a shared chat object", {
  withr::local_options(dsprrr.omni_parallel_sync = TRUE)
  withr::local_envvar(c(
    OPENAI_API_KEY = "test-key",
    ANTHROPIC_API_KEY = NA,
    GOOGLE_API_KEY = NA
  ))

  tp <- Omni(
    metric = omni_metric,
    explorers = list(
      a = OmniWorkerLlmTeleprompter(marker = "a"),
      b = OmniWorkerLlmTeleprompter(marker = "b")
    ),
    continuation = OmniMarkingTeleprompter(marker = "-c", append = TRUE),
    parallel = TRUE,
    verbose = FALSE
  )
  compiled <- compile(
    make_omni_mock_module(),
    tp,
    omni_trainset,
    valset = omni_valset
  )

  expect_equal(compiled$config$marker, "b-c")
  expect_equal(optimization_result(compiled)$extensions$omni$parallel, TRUE)
})

test_that("Omni requires worker-visible credentials for parallel exploration", {
  withr::local_envvar(c(
    OPENAI_API_KEY = NA,
    ANTHROPIC_API_KEY = NA,
    GOOGLE_API_KEY = NA
  ))

  expect_snapshot(
    error = TRUE,
    compile(
      make_omni_mock_module(),
      make_omni(parallel = TRUE),
      omni_trainset,
      valset = omni_valset
    )
  )
})

test_that("Omni rejects non-Chat .llm before parallel policy", {
  expect_snapshot(
    error = TRUE,
    compile(
      make_omni_mock_module(),
      make_omni(parallel = TRUE),
      omni_trainset,
      valset = omni_valset,
      .llm = list(provider = "not-serializable")
    )
  )
})
