# Tests for RCodeRunner

test_that("r_code_runner creates RCodeRunner object", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5)

  expect_s3_class(runner, "RCodeRunner")
  expect_equal(runner$timeout, 5)
  expect_equal(runner$max_output_chars, 100000L)
  expect_false(runner$persistent)
})

test_that("RCodeRunner exposes its trust boundary", {
  skip_if_not_installed("callr")

  policy <- r_code_runner()$policy()

  expect_identical(policy$backend, "callr")
  expect_identical(policy$trust, "trusted-input-only")
  expect_false(policy$sandboxed)
  expect_true(policy$process_isolation)
  expect_identical(policy$filesystem_access, "host-user")
  expect_identical(policy$network_access, "host-user")
  expect_identical(policy$pattern_scan, "defense-in-depth")
  expect_false(policy$persistent)
})

test_that("code runner protocol supports external sandbox backends", {
  runner <- list(
    execute = function(code, context = list()) {
      list(success = TRUE, result = code, context = context)
    },
    policy = function() {
      list(
        backend = "test-container",
        trust = "untrusted-input",
        sandboxed = TRUE
      )
    }
  )

  expect_invisible(dsprrr:::validate_code_runner(runner))
  expect_s3_class(
    program_of_thought("question -> answer", runner = runner),
    "ProgramOfThoughtModule"
  )
  expect_s3_class(
    code_act("question -> answer", runner = runner),
    "CodeActModule"
  )
  expect_s3_class(
    rlm_module("question -> answer", runner = runner),
    "RLMModule"
  )
})

test_that("execute_code_runner forwards supported control metadata", {
  observed <- NULL
  runner <- list(
    execute = function(
      code,
      context = list(),
      .control_nonce = NULL,
      .control_protocol = NULL,
      .control_max_bytes = NULL
    ) {
      observed <<- list(
        nonce = .control_nonce,
        protocol = .control_protocol,
        max_bytes = .control_max_bytes
      )
      list(success = TRUE, result = code)
    },
    policy = function() {
      list(backend = "test", trust = "test", sandboxed = TRUE)
    }
  )

  result <- dsprrr:::execute_code_runner(
    runner,
    "1 + 1",
    .control_nonce = "step-1",
    .control_protocol = "flex",
    .control_max_bytes = 2048L
  )

  expect_identical(result$result, "1 + 1")
  expect_identical(observed$nonce, "step-1")
  expect_identical(observed$protocol, "flex")
  expect_identical(observed$max_bytes, 2048L)
})

test_that("code runner protocol validates policy metadata", {
  incomplete <- list(
    execute = function(code, context = list()) NULL,
    policy = function() list(backend = "custom")
  )
  invalid <- list(
    execute = function(code, context = list()) NULL,
    policy = function() {
      list(backend = "custom", trust = "trusted", sandboxed = "no")
    }
  )

  expect_error(
    dsprrr:::validate_code_runner(incomplete),
    "Missing required fields"
  )
  expect_error(
    dsprrr:::validate_code_runner(invalid),
    "returned invalid metadata"
  )
})

test_that("RCodeRunner executes simple R code", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  result <- runner$execute("1 + 1")

  expect_true(result$success)
  expect_equal(result$result, 2)
  expect_null(result$error)
  expect_gte(result$duration_ms, 0)
})

test_that("RCodeRunner executes multi-line code", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  code <- "
    x <- 10
    y <- 20
    x + y
  "
  result <- runner$execute(code)

  expect_true(result$success)
  expect_equal(result$result, 30)
})

test_that("RCodeRunner captures stdout and stderr without trailing newlines", {
  skip_if_not_installed("callr")

  result <- r_code_runner(timeout = 10)$execute(
    "cat('hello'); cat('problem', file = stderr()); 42"
  )

  expect_true(result$success)
  expect_equal(result$result, 42)
  expect_identical(result$stdout, "hello")
  expect_identical(result$stderr, "problem")
})

test_that("RCodeRunner captures messages", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  result <- runner$execute("message('test message'); 123")

  expect_true(result$success)
  expect_equal(result$result, 123)
  expect_match(result$messages, "test message")
})

test_that("RCodeRunner captures warnings", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  result <- runner$execute("warning('test warning'); 456")

  expect_true(result$success)
  expect_equal(result$result, 456)
  expect_match(result$warnings, "test warning")
})

test_that("RCodeRunner handles errors gracefully", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  result <- runner$execute("stop('intentional error')")

  expect_false(result$success)
  expect_null(result$result)
  expect_match(result$error, "intentional error")
  expect_identical(result$error_type, "execution")
  expect_true(result$retryable)
})

test_that("RCodeRunner handles syntax errors", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  result <- runner$execute("this is not valid R code {{{")

  expect_false(result$success)
  expect_null(result$result)
  expect_true(nchar(result$error) > 0)
})

test_that("RCodeRunner terminalizes malformed or crashed subprocesses", {
  skip_if_not_installed("callr")

  crash_code <- paste0(
    "base::get(\"q\", baseenv())(",
    "save = \"no\", status = 1, runLast = FALSE)"
  )
  runner <- r_code_runner(timeout = 10)
  result <- runner$execute(crash_code)

  expect_false(result$success)
  expect_identical(result$error_type, "interpreter")
  expect_false(result$retryable)
  expect_true(runner$terminal)
  expect_error(
    runner$execute("1 + 1"),
    class = "dsprrr_interpreter_terminal_error"
  )

  empty_code <- paste0(
    "base::get(\"q\", baseenv())(",
    "save = \"no\", status = 0, runLast = FALSE)"
  )
  empty_runner <- r_code_runner(timeout = 10)
  empty_result <- empty_runner$execute(empty_code)
  expect_false(empty_result$success)
  expect_identical(empty_result$error_type, "interpreter")
  expect_match(empty_result$error, "structured execution result")
  expect_true(empty_runner$terminal)
})

test_that("RCodeRunner passes context correctly", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  result <- runner$execute(
    "mean(.context$data$mpg)",
    context = list(data = mtcars)
  )

  expect_true(result$success)
  expect_equal(result$result, mean(mtcars$mpg))
})

test_that("RCodeRunner context with multiple objects", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  result <- runner$execute(
    ".context$x + .context$y",
    context = list(x = 10, y = 32)
  )

  expect_true(result$success)
  expect_equal(result$result, 42)
})

test_that("RCodeRunner enforces timeout", {
  skip_if_not_installed("callr")
  skip_on_cran() # Timeout tests can be flaky on CRAN

  runner <- r_code_runner(timeout = 1)
  result <- runner$execute("Sys.sleep(10); 'done'")

  expect_false(result$success)
  expect_null(result$result)
  expect_match(result$error, "timed out", ignore.case = TRUE)
})

test_that("RCodeRunner blocks dangerous system() calls", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  result <- runner$execute("system('ls')")

  expect_false(result$success)
  expect_match(result$error, "system\\(\\) calls are not allowed")
})

test_that("RCodeRunner blocks dangerous system2() calls", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  result <- runner$execute("system2('ls', '.')")

  expect_false(result$success)
  expect_match(result$error, "system2\\(\\) calls are not allowed")
})

test_that("RCodeRunner blocks unlink()", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  result <- runner$execute("unlink('/tmp/test')")

  expect_false(result$success)
  expect_match(result$error, "unlink\\(\\) is not allowed")
})

test_that("RCodeRunner blocks quit()", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  result <- runner$execute("quit()")

  expect_false(result$success)
  expect_match(result$error, "quit\\(\\) is not allowed")
})

test_that("RCodeRunner blocks Sys.setenv", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  result <- runner$execute("Sys.setenv(FOO = 'bar')")

  expect_false(result$success)
  expect_match(result$error, "Modifying environment variables")
})

test_that("RCodeRunner blocks download.file", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  result <- runner$execute("download.file('http://example.com', 'x')")

  expect_false(result$success)
  expect_match(result$error, "download.file")
})

test_that("RCodeRunner blocks url() connections", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  result <- runner$execute("url('http://example.com')")

  expect_false(result$success)
  expect_match(result$error, "url\\(\\) connections")
})

test_that("RCodeRunner blocks base::system() bypass", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  result <- runner$execute("base::system('ls')")

  expect_false(result$success)
  expect_match(result$error, "system\\(\\) calls are not allowed")
})

test_that("RCodeRunner blocks do.call(system) bypass", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  result <- runner$execute("do.call(system, list('ls'))")

  expect_false(result$success)
  expect_match(result$error, "do.call\\(system\\)")
})

test_that("RCodeRunner truncates large output in messages", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, max_output_chars = 100)
  # Use message() which is reliably captured
  result <- runner$execute("message(paste(rep('x', 500), collapse = '')); 1")

  expect_true(result$success)
  expect_match(result$messages, "TRUNCATED")
  expect_lte(nchar(result$messages), 150) # 100 + truncation message
})

test_that("RCodeRunner bounds captured stdout before returning it", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, max_output_chars = 100)
  result <- runner$execute("cat(base::strrep('x', 500L)); 1")

  expect_true(result$success)
  expect_match(result$stdout, "TRUNCATED", fixed = TRUE)
  expect_lte(nchar(result$stdout), 160)
})

test_that("RCodeRunner respects custom timeout", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 60)
  expect_equal(runner$timeout, 60)
})

test_that("RCodeRunner respects custom max_output_chars", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(max_output_chars = 5000)
  expect_equal(runner$max_output_chars, 5000L)
})

test_that("RCodeRunner respects custom allowed_packages", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(allowed_packages = c("base", "stats", "dplyr"))
  expect_equal(runner$allowed_packages, c("base", "stats", "dplyr"))
})

test_that("RCodeRunner blocks library() for disallowed packages", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(
    timeout = 10,
    allowed_packages = c("base", "stats")
  )
  result <- runner$execute("library(dplyr)")

  expect_false(result$success)
  expect_match(result$error, "not in allowed_packages")
  expect_match(result$error, "dplyr")
})

test_that("RCodeRunner blocks require() for disallowed packages", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(
    timeout = 10,
    allowed_packages = c("base", "stats")
  )
  result <- runner$execute("require(ggplot2)")

  expect_false(result$success)
  expect_match(result$error, "not in allowed_packages")
  expect_match(result$error, "ggplot2")
})

test_that("RCodeRunner allows library() for allowed packages", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(
    timeout = 10,
    allowed_packages = c("base", "stats", "utils")
  )
  # stats is always available, so this should succeed
  result <- runner$execute("library(stats); mean(1:5)")

  expect_true(result$success)
  expect_equal(result$result, 3)
})

test_that("RCodeRunner runs prelude code", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(
    timeout = 10,
    prelude = c("my_constant <- 42")
  )
  result <- runner$execute("my_constant * 2")

  expect_true(result$success)
  expect_equal(result$result, 84)
})

test_that("RCodeRunner print method works", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5)

  output <- capture.output(expect_invisible(print(runner)), type = "message")
  expect_match(paste(output, collapse = "\n"), "trusted-input-only")
  expect_match(paste(output, collapse = "\n"), "not sandboxed")
  expect_s3_class(runner, "RCodeRunner")
})

test_that("RCodeRunner validates code input", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)

  expect_error(
    runner$execute(123),
    "code must be a single character string"
  )

  expect_error(
    runner$execute(c("a", "b")),
    "code must be a single character string"
  )
})

test_that("RCodeRunner handles empty code", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  result <- runner$execute("")

  # Empty code should execute successfully with NULL result

  expect_true(result$success)
  expect_null(result$result)
})

test_that("RCodeRunner returns structured result with all fields", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  result <- runner$execute("42")

  expect_true("success" %in% names(result))
  expect_true("result" %in% names(result))
  expect_true("stdout" %in% names(result))
  expect_true("stderr" %in% names(result))
  expect_true("messages" %in% names(result))
  expect_true("warnings" %in% names(result))
  expect_true("error" %in% names(result))
  expect_true("duration_ms" %in% names(result))
})

test_that("RCodeRunner can use base R functions", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  result <- runner$execute("sqrt(16) + log(1)")

  expect_true(result$success)
  expect_equal(result$result, 4)
})

test_that("RCodeRunner can use stats functions", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  result <- runner$execute("mean(c(1, 2, 3, 4, 5))")

  expect_true(result$success)
  expect_equal(result$result, 3)
})

test_that("RCodeRunner can create and manipulate data frames", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  code <- "
    df <- data.frame(x = 1:3, y = c('a', 'b', 'c'))
    nrow(df)
  "
  result <- runner$execute(code)

  expect_true(result$success)
  expect_equal(result$result, 3)
})

test_that("RCodeRunner isolation - variables don't persist", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)

  # First execution sets a variable

  result1 <- runner$execute("test_var <- 999; test_var")
  expect_true(result1$success)
  expect_equal(result1$result, 999)

  # Second execution shouldn't see that variable
  result2 <- runner$execute("exists('test_var')")
  expect_true(result2$success)
  expect_false(result2$result)
})

test_that("persistent RCodeRunner preserves execution state", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$close())

  expect_true(runner$policy()$persistent)
  first_pid <- runner$execute("Sys.getpid()")$result
  first <- runner$execute("counter <- 41L; counter")
  second <- runner$execute("counter <- counter + 1L; counter")
  second_pid <- runner$execute("Sys.getpid()")$result

  expect_identical(second_pid, first_pid)
  expect_true(first$success)
  expect_identical(first$result, 41L)
  expect_true(second$success)
  expect_identical(second$result, 42L)

  guest_error <- runner$execute("counter <- counter + 1L; stop('repair me')")
  recovered <- runner$execute("counter")
  expect_false(guest_error$success)
  expect_identical(guest_error$error_type, "execution")
  expect_true(guest_error$retryable)
  expect_true(recovered$success)
  expect_identical(recovered$result, 43L)
})

test_that("persistent RCodeRunner preserves structured output capture", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$close())
  result <- runner$execute(
    "cat('out'); cat('err', file = stderr()); message('note'); warning('careful'); 42L"
  )

  expect_true(result$success)
  expect_identical(result$result, 42L)
  expect_identical(result$stdout, "out")
  expect_identical(result$stderr, "err")
  expect_match(result$messages, "note")
  expect_match(result$warnings, "careful")
})

test_that("persistent RCodeRunner stages base context and overlays dynamic fields", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$close())
  marker <- new.env(parent = emptyenv())
  marker$count <- 0L
  large_data <- data.frame(
    id = seq_len(25000L),
    group = factor(rep(c("a", "b"), length.out = 25000L))
  )

  prepared <- withVisible(runner$prepare_context(list(
    data = large_data,
    label = "base",
    marker = marker
  )))
  expect_true(prepared$value)
  expect_false(prepared$visible)
  first <- runner$execute(
    paste(
      ".context$marker$count <- .context$marker$count + 1L",
      "list(",
      "  is_data_frame = is.data.frame(.context$data),",
      "  rows = nrow(.context$data),",
      "  groups = levels(.context$data$group),",
      "  label = .context$label,",
      "  step = .context$step,",
      "  marker_count = .context$marker$count",
      ")",
      sep = "\n"
    ),
    context = list(label = "dynamic", step = 7L)
  )
  second <- runner$execute(
    "list(label = .context$label, has_step = 'step' %in% names(.context), marker_count = .context$marker$count)"
  )

  expect_true(first$success)
  expect_true(first$result$is_data_frame)
  expect_identical(first$result$rows, 25000L)
  expect_identical(first$result$groups, c("a", "b"))
  expect_identical(first$result$label, "dynamic")
  expect_identical(first$result$step, 7L)
  expect_identical(first$result$marker_count, 1L)
  expect_true(second$success)
  expect_identical(second$result$label, "base")
  expect_false(second$result$has_step)
  expect_identical(second$result$marker_count, 1L)
  expect_identical(marker$count, 0L)
})

test_that("persistent RCodeRunner reset clears state and staged context", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(
    timeout = 10,
    prelude = "from_prelude <- 11L",
    persistent = TRUE
  )
  withr::defer(runner$close())
  runner$prepare_context(list(base_value = 7L))
  before <- runner$execute(
    "transient <- 9L; c(from_prelude, .context$base_value, transient)"
  )

  expect_true(before$success)
  expect_identical(before$result, c(11L, 7L, 9L))
  expect_invisible(runner$reset())

  after <- runner$execute(
    "list(has_transient = exists('transient', inherits = FALSE), from_prelude = from_prelude, context_length = length(.context))"
  )
  expect_true(after$success)
  expect_false(after$result$has_transient)
  expect_identical(after$result$from_prelude, 11L)
  expect_identical(after$result$context_length, 0L)
})

test_that("persistent RCodeRunner timeout terminalizes and stops its session", {
  skip_if_not_installed("callr")
  skip_on_cran()

  runner <- r_code_runner(timeout = 1, persistent = TRUE)
  withr::defer(runner$close())
  runner$start()
  session <- runner$.__enclos_env__$private$session
  expect_true(session$is_alive())

  result <- runner$execute("Sys.sleep(10); 'done'")

  expect_false(result$success)
  expect_identical(result$error_type, "interpreter")
  expect_false(result$retryable)
  expect_match(result$error, "timed out", ignore.case = TRUE)
  expect_true(runner$terminal)
  expect_false(session$is_alive())
  expect_error(
    runner$execute("1 + 1"),
    class = "dsprrr_interpreter_terminal_error"
  )
  expect_invisible(runner$close())
  expect_invisible(runner$close())
})

test_that("persistent RCodeRunner terminalizes when its subprocess crashes", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$close())
  runner$start()
  session <- runner$.__enclos_env__$private$session
  crash_code <- paste0(
    "base::get(\"q\", baseenv())(",
    "save = \"no\", status = 1, runLast = FALSE)"
  )

  result <- runner$execute(crash_code)

  expect_false(result$success)
  expect_identical(result$error_type, "interpreter")
  expect_false(result$retryable)
  expect_true(runner$terminal)
  expect_false(session$is_alive())
})

test_that("persistent RCodeRunner close tears down its session", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  runner$start()
  session <- runner$.__enclos_env__$private$session
  expect_true(session$is_alive())

  expect_invisible(runner$close())
  expect_true(runner$closed)
  expect_false(session$is_alive())
  expect_invisible(runner$close())
  expect_error(
    runner$execute("1 + 1"),
    class = "dsprrr_interpreter_closed_error"
  )
})

test_that("prepare_context requires persistent mode and named fields", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  expect_error(
    runner$prepare_context(list(value = 1)),
    class = "dsprrr_r_code_runner_context_error"
  )

  persistent_runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(persistent_runner$close())
  expect_error(
    persistent_runner$prepare_context(list(1)),
    class = "dsprrr_r_code_runner_context_error"
  )
})
