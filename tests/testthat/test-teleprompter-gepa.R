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
  # Scores where rows 1, 2, 3 are non-dominated (Pareto frontier)
  # Row 1: (1.0, 0.2) - best on metric 1
  # Row 2: (0.5, 0.8) - balanced
  # Row 3: (0.2, 1.0) - best on metric 2
  # Row 4: (0.2, 0.2) - dominated by all others
  scores <- matrix(
    c(
      1.0,
      0.2,
      0.5,
      0.8,
      0.2,
      1.0,
      0.2,
      0.2
    ),
    ncol = 2,
    byrow = TRUE
  )

  frontier <- dsprrr:::pareto_frontier(scores)
  expect_setequal(frontier, c(1, 2, 3))

  ranks <- dsprrr:::pareto_ranks(scores)
  expect_equal(ranks[1], 1L)
  expect_equal(ranks[2], 1L)
  expect_equal(ranks[3], 1L)
  expect_equal(ranks[4], 2L)
})

test_that("GEPA compiles with reflection-based mutation", {
  sig <- Signature(
    inputs = list(input(name = "question", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Be concise."
  )
  mod <- module(signature = sig, type = "predict")

  trainset <- data.frame(
    question = c("q1", "q2"),
    answer = c("yes", "yes"),
    stringsAsFactors = FALSE
  )

  # Metric that checks if prediction matches expected
  metric_fn <- function(pred, expected) {
    as.numeric(pred == expected$answer)
  }

  # Track mutation calls to verify LLM is being used
  mutation_calls <- 0L

  mock_llm <- structure(
    list(
      chat_structured = function(prompt, type, ...) {
        # Check if this is a mutation call (reflection prompt) or regular inference
        if (
          grepl("improving system instructions", prompt, ignore.case = TRUE)
        ) {
          mutation_calls <<- mutation_calls + 1L
          list(instructions = "Be accurate and explicit.")
        } else {
          # Regular inference - return correct answer only if prompt contains
          # "accurate" (from mutated instructions)
          if (grepl("accurate", prompt, ignore.case = TRUE)) {
            list(answer = "yes")
          } else {
            list(answer = "no") # Wrong answer for original instructions
          }
        }
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

  # Verify mutations were called (at least for initial population generation)
  expect_true(mutation_calls >= 3) # population_size - 1 for initial population

  # The compiled module should have mutated instructions (members with "accurate"
  # scored higher because the mock LLM only returns correct answers for them)
  expect_true(grepl(
    "accurate",
    compiled$signature@instructions,
    ignore.case = TRUE
  ))
  expect_true(length(compiled$config$optimizer$pareto_frontier) >= 1)
})
