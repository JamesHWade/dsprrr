# Tests for GEPA teleprompter

test_that("GEPA can be created with defaults", {
  tp <- GEPA()
  expect_s3_class(tp, "dsprrr::GEPA")
  expect_s3_class(tp, "dsprrr::Teleprompter")
  expect_equal(tp@population_size, 20L)
  expect_equal(tp@generations, 10L)
  expect_equal(tp@mutation_rate, 0.1)
  expect_equal(tp@crossover_rate, 0.7)
  expect_equal(tp@selection, "pareto")
  expect_true(tp@verbose)
  expect_true(tp@track_stats)
  expect_null(tp@metrics)
})

test_that("GEPA validates properties", {
  expect_error(GEPA(population_size = 1L), "at least 2")
  expect_error(GEPA(generations = 0L), "at least 1")
  expect_error(GEPA(mutation_rate = -0.1), "between 0 and 1")
  expect_error(GEPA(crossover_rate = 1.2), "between 0 and 1")
  expect_error(GEPA(selection = "unknown"), "selection must")
  expect_error(GEPA(metrics = list("not a function")), "functions")
})

test_that("Pareto utilities identify frontier", {
  scores <- matrix(
    c(
      1, 1,
      0.5, 0.9,
      0.9, 0.5,
      0.2, 0.2
    ),
    ncol = 2,
    byrow = TRUE
  )

  frontier <- dsprrr:::pareto_frontier(scores)
  expect_setequal(frontier, c(1, 2, 3))

  ranks <- dsprrr:::pareto_ranks(scores)
  expect_equal(ranks[1], 1L)
  expect_equal(ranks[4], 2L)
})

test_that("GEPA compiles with reflection-based mutation", {
  MockGepaModule <- R6::R6Class(
    "MockGepaModule",
    inherit = dsprrr:::PredictModule,
    public = list(
      initialize = function(
        signature,
        template = "",
        demos = list(),
        config = list()
      ) {
        super$initialize(
          signature,
          template = template,
          demos = demos,
          config = config
        )
      },
      forward = function(batch, .llm = NULL, trace = TRUE, ...) {
        instruction <- self$signature@instructions
        output_val <- if (grepl("accurate", instruction, ignore.case = TRUE)) {
          "yes"
        } else {
          "no"
        }
        tibble::tibble(
          output = list(output_val),
          chat = list(NULL),
          metadata = list(list())
        )
      }
    )
  )

  sig <- Signature(
    inputs = list(input(name = "question", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Be concise."
  )
  mod <- MockGepaModule$new(signature = sig, template = "{question}")

  trainset <- data.frame(
    question = c("q1", "q2"),
    answer = c("yes", "yes"),
    stringsAsFactors = FALSE
  )

  metric_fn <- function(pred, expected) {
    as.numeric(pred == expected$answer)
  }

  mock_llm <- structure(
    list(
      chat_structured = function(prompt, type, ...) {
        list(instructions = "Be accurate and explicit.")
      }
    ),
    class = "Chat"
  )

  tp <- GEPA(
    metrics = list(quality = metric_fn),
    population_size = 4L,
    generations = 1L,
    mutation_rate = 1,
    selection = "current_best",
    seed = 1L,
    verbose = FALSE
  )

  compiled <- compile(tp, mod, trainset, .llm = mock_llm)

  expect_true(compiled$config$compiled)
  expect_equal(compiled$config$teleprompter, "GEPA")
  expect_true(grepl("accurate", compiled$signature@instructions, ignore.case = TRUE))
  expect_true(length(compiled$config$optimizer$pareto_frontier) >= 1)
})
