OmniMarkingTeleprompter <- S7::new_class(
  "OmniMarkingTeleprompter",
  parent = Teleprompter,
  properties = list(
    marker = S7::new_property(S7::class_character),
    append = S7::new_property(S7::class_logical, default = FALSE)
  )
)

S7::method(compile, list(OmniMarkingTeleprompter, S7::class_any)) <- function(
  teleprompter,
  program,
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

S7::method(compile, list(OmniFailingTeleprompter, S7::class_any)) <- function(
  teleprompter,
  program,
  trainset,
  ...
) {
  cli::cli_abort("intentional Omni explorer failure")
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

make_omni <- function(continuation = NULL, parallel = FALSE) {
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
})

test_that("Omni explores from one seed and continues from the winner", {
  compiled <- compile(
    make_omni(),
    make_omni_mock_module(),
    omni_trainset,
    valset = omni_valset
  )

  expect_equal(compiled$config$marker, "b-c")
  expect_equal(compiled$config$teleprompter, "Omni")
  expect_equal(compiled$config$optimizer$exploration_winner, "b")
  expect_equal(compiled$config$optimizer$best_phase, "continue")
  expect_equal(
    compiled$config$optimizer$best_optimizer,
    "dsprrr::OmniMarkingTeleprompter"
  )

  candidates <- compiled$config$optimizer$candidate_programs
  expect_equal(
    candidates$optimizer,
    c("baseline", "a", "b", "dsprrr::OmniMarkingTeleprompter")
  )
  expect_equal(candidates$score, c(0, 0.2, 0.8, 1))
  expect_equal(which(candidates$selected), 4L)
  expect_equal(candidates$program[[2]]$config$input_marker, "seed")
  expect_equal(candidates$program[[3]]$config$input_marker, "seed")
  expect_equal(candidates$program[[4]]$config$input_marker, "b")
})

test_that("Omni preserves the best program when continuation regresses", {
  tp <- make_omni(
    continuation = OmniMarkingTeleprompter(marker = "bad")
  )
  compiled <- compile(
    tp,
    make_omni_mock_module(),
    omni_trainset,
    valset = omni_valset
  )

  expect_equal(compiled$config$marker, "b")
  expect_equal(compiled$config$optimizer$best_phase, "explore")
  expect_equal(compiled$config$optimizer$best_optimizer, "b")
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
      tp,
      make_omni_mock_module(),
      omni_trainset,
      valset = omni_valset
    )
  )

  expect_equal(compiled$config$marker, "b-c")
  expect_equal(
    compiled$config$optimizer$candidate_programs$error[[2]],
    "intentional Omni explorer failure"
  )
  expect_equal(
    compiled$config$optimizer$flag_compilation_error_occurred,
    TRUE
  )
})

test_that("Omni requires comparison data", {
  expect_snapshot(
    error = TRUE,
    compile(
      make_omni(),
      make_omni_mock_module(),
      data.frame(x = "only-row", target = "unused")
    )
  )
})

test_that("Omni validates per-optimizer compile arguments", {
  expect_snapshot(
    error = TRUE,
    compile(
      make_omni(),
      make_omni_mock_module(),
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
      make_omni(),
      make_omni_mock_module(),
      omni_trainset,
      valset = omni_valset,
      continuation_compile_args = list(trainset = omni_trainset)
    )
  )

  expect_snapshot(
    error = TRUE,
    compile(
      make_omni(),
      make_omni_mock_module(),
      omni_trainset,
      valset = omni_valset,
      continuation_compile_args = list(TRUE)
    )
  )
})

test_that("Omni supports mirai exploration without a shared chat object", {
  withr::local_options(dsprrr.omni_parallel_sync = TRUE)

  compiled <- compile(
    make_omni(parallel = TRUE),
    make_omni_mock_module(),
    omni_trainset,
    valset = omni_valset
  )

  expect_equal(compiled$config$marker, "b-c")
  expect_equal(compiled$config$optimizer$parallel, TRUE)
})

test_that("Omni rejects unsafe parallel chat serialization", {
  expect_snapshot(
    error = TRUE,
    compile(
      make_omni(parallel = TRUE),
      make_omni_mock_module(),
      omni_trainset,
      valset = omni_valset,
      .llm = list(provider = "not-serializable")
    )
  )
})
