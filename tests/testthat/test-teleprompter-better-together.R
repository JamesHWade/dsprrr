BTMarkingTeleprompter <- S7::new_class(
  "BTMarkingTeleprompter",
  parent = Teleprompter,
  properties = list(
    marker = S7::new_property(S7::class_character, default = "marked")
  )
)

S7::method(compile, list(BTMarkingTeleprompter, S7::class_any)) <- function(
  teleprompter,
  program,
  trainset,
  ...
) {
  compiled <- dsprrr:::copy_module(program)
  compiled$config$marker <- teleprompter@marker
  compiled$config$compiled <- TRUE
  compiled$config$teleprompter <- paste0("BTMarking:", teleprompter@marker)
  compiled$state$compiled <- TRUE
  compiled
}

make_bt_mock_module <- function(marker = "baseline") {
  BTMockModule <- R6::R6Class(
    "BTMockModule",
    inherit = dsprrr:::Module,
    public = list(
      initialize = function(marker = "baseline") {
        sig <- Signature(
          inputs = list(input("x", "string")),
          output_type = ellmer::type_string(),
          instructions = "Return the marker"
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
        new_module <- make_bt_mock_module(self$config$marker)
        new_module$config <- lapply(self$config, function(x) x)
        new_module$state <- lapply(self$state, function(x) x)
        new_module
      }
    )
  )

  BTMockModule$new(marker = marker)
}

bt_metric <- function(prediction, expected_row) {
  as.numeric(identical(as.character(prediction), expected_row$target[[1]]))
}

test_that("BetterTogether accepts named optimizers and validates strategy", {
  tp <- BetterTogether(
    metric = bt_metric,
    p = BTMarkingTeleprompter(marker = "p"),
    w = BTMarkingTeleprompter(marker = "w")
  )

  expect_s3_class(tp, "dsprrr::BetterTogether")
  expect_named(tp@optimizers, c("p", "w"))

  expect_error(
    compile(
      tp,
      make_bt_mock_module(),
      data.frame(x = "a", target = "p"),
      strategy = "p -> missing",
      valset_ratio = 0
    ),
    "unknown optimizer keys"
  )
})

test_that("BetterTogether returns best validation candidate when valset exists", {
  tp <- BetterTogether(
    metric = bt_metric,
    p = BTMarkingTeleprompter(marker = "p"),
    w = BTMarkingTeleprompter(marker = "w"),
    verbose = FALSE
  )

  mod <- make_bt_mock_module()
  trainset <- data.frame(x = c("a", "b"), target = c("p", "p"))
  valset <- data.frame(x = "c", target = "p")

  compiled <- compile(
    tp,
    mod,
    trainset,
    valset = valset,
    strategy = "p -> w",
    shuffle_trainset_between_steps = FALSE
  )

  expect_equal(compiled$config$marker, "p")
  expect_equal(compiled$config$teleprompter, "BetterTogether")
  expect_equal(compiled$config$best_strategy, "p")
  expect_false(compiled$config$optimizer$flag_compilation_error_occurred)

  candidates <- compiled$config$optimizer$candidate_programs
  expect_s3_class(candidates, "tbl_df")
  expect_equal(candidates$strategy[[1]], "p")
  expect_equal(candidates$score[[1]], 1)
})

test_that("BetterTogether returns latest candidate without validation", {
  tp <- BetterTogether(
    metric = bt_metric,
    optimizers = list(
      p = BTMarkingTeleprompter(marker = "p"),
      w = BTMarkingTeleprompter(marker = "w")
    ),
    verbose = FALSE
  )

  compiled <- compile(
    tp,
    make_bt_mock_module(),
    data.frame(x = c("a", "b"), target = c("p", "w")),
    strategy = "p -> w",
    valset_ratio = 0,
    shuffle_trainset_between_steps = FALSE
  )

  expect_equal(compiled$config$marker, "w")
  expect_equal(compiled$config$best_strategy, "p -> w")
  expect_true(all(is.na(compiled$config$optimizer$candidate_programs$score)))
})

test_that("BetterTogether marks compilation errors and returns prior candidate", {
  BTFailingTeleprompter <- S7::new_class(
    "BTFailingTeleprompter",
    parent = Teleprompter
  )

  S7::method(compile, list(BTFailingTeleprompter, S7::class_any)) <- function(
    teleprompter,
    program,
    trainset,
    ...
  ) {
    cli::cli_abort("intentional failure")
  }

  tp <- BetterTogether(
    metric = bt_metric,
    p = BTMarkingTeleprompter(marker = "p"),
    f = BTFailingTeleprompter(),
    verbose = FALSE
  )

  compiled <- NULL
  expect_warning(
    compiled <- compile(
      tp,
      make_bt_mock_module(),
      data.frame(x = c("a", "b"), target = c("p", "p")),
      valset = data.frame(x = "c", target = "p"),
      strategy = "p -> f"
    ),
    "step failed"
  )

  expect_equal(compiled$config$marker, "p")
  expect_true(compiled$config$optimizer$flag_compilation_error_occurred)
})
