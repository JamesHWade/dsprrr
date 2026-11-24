test_that("optimizer progress checkpoints follow row and trial settlement", {
  data <- data.frame(x = c("one", "two"), y = c("one", "two"))
  program <- module(signature("x -> y"))
  forwarded <- list()
  local_mocked_bindings(
    eval_program = function(program, dataset, metric, ...) {
      forwarded[[length(forwarded) + 1L]] <<- list(...)
      EvalResult(
        examples = tibble::tibble(
          row_id = 1L,
          score = 1,
          error = NA_character_,
          feedback = NA_character_,
          predicted = list(dataset$y[[1L]]),
          program_trace = list(list(output = dataset$y[[1L]]))
        ),
        mean_score = 1,
        n_evaluated = 1L,
        metric_calls = 1L
      )
    }
  )
  budget <- new_optimizer_budget(optimizer_control(max_metric_calls = 2L))
  progress <- list()

  result <- optimizer_eval_program(
    program,
    data,
    function(...) 1,
    budget = budget,
    stage = "search",
    unit_id = "candidate",
    on_progress = function(records, row_index, ledger) {
      progress[[length(progress) + 1L]] <<- list(
        row = row_index,
        stored_rows = vapply(records, `[[`, integer(1), "row_index"),
        checkpoint_fields = lapply(records, names),
        row_settled = optimizer_budget_unit_completed(
          ledger,
          paste0("candidate:row:", row_index)
        ),
        trial_settled = optimizer_budget_unit_completed(ledger, "candidate")
      )
    },
    row_index = "forwarded"
  )

  expect_identical(result@examples$score, c(1, 1))
  expect_identical(
    vapply(forwarded, `[[`, character(1), "row_index"),
    rep("forwarded", 2L)
  )
  expect_identical(vapply(forwarded, `[[`, integer(1), ".trace_row_ids"), 1:2)
  expect_identical(vapply(progress, `[[`, integer(1), "row"), c(1L, 2L, 2L))
  expect_identical(lapply(progress, `[[`, "stored_rows"), list(1L, 1:2, 1:2))
  expect_identical(
    vapply(progress, `[[`, logical(1), "row_settled"),
    rep(TRUE, 3L)
  )
  expect_identical(
    vapply(progress, `[[`, logical(1), "trial_settled"),
    c(FALSE, FALSE, TRUE)
  )
  expect_length(
    intersect(
      unlist(progress[[3L]]$checkpoint_fields),
      c("predicted", "program_trace")
    ),
    0L
  )
})
