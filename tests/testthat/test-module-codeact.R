# Tests for CodeActModule

# Helper: Create a mock LLM for testing
create_mock_codeact_llm <- function(responses = list()) {
  call_count <- 0
  tools <- list()
  turns <- list()

  list(
    clone = function() {
      create_mock_codeact_llm(responses)
    },
    register_tool = function(tool) {
      tools[[length(tools) + 1]] <<- tool
    },
    chat = function(prompt, ...) {
      call_count <<- call_count + 1
      turns[[call_count]] <<- list(role = "assistant", content = prompt)

      if (call_count <= length(responses)) {
        responses[[call_count]]
      } else {
        "The answer is 42"
      }
    },
    get_turns = function() {
      turns
    }
  )
}

# ============================================================================
# Factory Function Tests
# ============================================================================

test_that("code_act requires runner", {
  expect_error(
    code_act("question -> answer"),
    "CodeAct requires an explicit runner"
  )
})

test_that("code_act validates runner type", {
  expect_error(
    code_act("question -> answer", runner = "not a runner"),
    "runner must implement the dsprrr code-runner protocol"
  )
})

test_that("code_act creates CodeActModule", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5)
  agent <- code_act("question -> answer", runner = runner)

  expect_s3_class(agent, "CodeActModule")
  expect_s3_class(agent, "Module")
})

test_that("code_act accepts string signature", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5)
  agent <- code_act("context, question -> answer", runner = runner)

  expect_equal(length(agent$signature@inputs), 2)
})

test_that("code_act accepts Signature object", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5)
  sig <- signature("question -> answer")
  agent <- code_act(sig, runner = runner)

  expect_s3_class(agent, "CodeActModule")
})

test_that("code_act respects max_iterations", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5)
  agent <- code_act(
    "question -> answer",
    runner = runner,
    max_iterations = 20
  )

  expect_equal(agent$max_iterations, 20L)
})

test_that("CodeAct validates iteration limits at both construction layers", {
  skip_if_not_installed("callr")
  runner <- r_code_runner(timeout = 5)
  invalid <- list(0, 1.5, NA_real_, Inf, c(1, 2), .Machine$integer.max + 1)

  for (value in invalid) {
    expect_error(
      code_act(
        "question -> answer",
        runner = runner,
        max_iterations = value
      ),
      class = "dsprrr_codeact_bounds_error",
      info = paste(value, collapse = ", ")
    )
  }
  expect_error(
    CodeActModule$new(
      signature = signature("question -> answer"),
      runner = runner,
      max_iterations = 0
    ),
    class = "dsprrr_codeact_bounds_error"
  )
})

test_that("CodeAct detects tool requests in assistant turns", {
  skip_if_not_installed("callr")
  runner <- r_code_runner(timeout = 5)
  agent <- code_act("question -> answer", runner = runner)
  tool_request <- structure(list(), class = "ellmer::ContentToolRequest")

  expect_true(agent$.__enclos_env__$private$has_pending_tools(list(
    role = "assistant",
    contents = list(tool_request)
  )))
  expect_false(agent$.__enclos_env__$private$has_pending_tools(list(
    role = "assistant",
    contents = list("finished")
  )))
})

test_that("CodeAct enforces tool calls inside one ellmer chat", {
  skip_if_not_installed("callr")
  runner <- r_code_runner(timeout = 5)
  agent <- code_act(
    "question -> answer",
    runner = runner,
    max_iterations = 2L
  )
  callback <- NULL
  mock_llm <- NULL
  mock_llm <- list(
    clone = function() mock_llm,
    register_tool = function(tool) invisible(NULL),
    on_tool_request = function(fn) {
      callback <<- fn
      function() callback <<- NULL
    },
    chat = function(prompt, ...) {
      callback(list(name = "execute_r_code"))
      callback(list(name = "execute_r_code"))
      callback(list(name = "execute_r_code"))
      "unreachable"
    },
    get_turns = function() list()
  )

  expect_error(
    agent$forward(list(question = "compute"), .llm = mock_llm),
    class = "dsprrr_codeact_iteration_limit"
  )
})

test_that("CodeAct ignores inherited tool turns from a cloned chat", {
  skip_if_not_installed("callr")
  runner <- r_code_runner(timeout = 5)
  agent <- code_act(
    "question -> answer",
    runner = runner,
    max_iterations = 1L
  )
  old_request <- structure(list(), class = "ellmer::ContentToolRequest")
  turns <- list(list(
    role = "assistant",
    contents = list(old_request)
  ))
  mock_llm <- NULL
  mock_llm <- list(
    clone = function() mock_llm,
    register_tool = function(tool) invisible(NULL),
    chat = function(prompt, ...) {
      turns <<- c(
        turns,
        list(list(
          role = "assistant",
          contents = list("done")
        ))
      )
      "done"
    },
    get_turns = function() turns
  )

  result <- agent$forward(list(question = "compute"), .llm = mock_llm)

  expect_identical(result$metadata[[1L]]$tool_calls, 0L)
  expect_identical(result$output[[1L]]$answer, "done")
})

test_that("code_act accepts tools list", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5)

  # Create a simple tool using current ellmer API
  mock_tool <- ellmer::tool(
    fun = function(x) paste("result:", x),
    description = "A test tool",
    arguments = list(x = ellmer::type_string())
  )

  agent <- code_act(
    "question -> answer",
    tools = list(test = mock_tool),
    runner = runner
  )

  expect_equal(length(agent$tools), 1)
  expect_true("test" %in% names(agent$tools))
})

# ============================================================================
# Module Structure Tests
# ============================================================================

test_that("CodeActModule has correct fields", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5)
  agent <- code_act("question -> answer", runner = runner)

  expect_true(!is.null(agent$tools))
  expect_true(!is.null(agent$runner))
  expect_true(!is.null(agent$max_iterations))
  expect_true(!is.null(agent$signature))
})

test_that("CodeAct protects its runner tool namespace", {
  skip_if_not_installed("callr")
  runner <- r_code_runner(timeout = 5)
  tool <- ellmer::tool(
    function(code) code,
    description = "shadow",
    arguments = list(code = ellmer::type_string()),
    name = "execute_r_code"
  )

  expect_error(
    code_act(
      "question -> answer",
      runner = runner,
      tools = list(execute_r_code = tool)
    ),
    class = "dsprrr_codeact_tools_error"
  )
  keyed_reserved <- ellmer::tool(
    function(x) x,
    description = "ordinary intrinsic name",
    arguments = list(x = ellmer::type_string()),
    name = "ordinary"
  )
  expect_error(
    code_act(
      "question -> answer",
      runner = runner,
      tools = list(execute_r_code = keyed_reserved)
    ),
    class = "dsprrr_codeact_tools_error"
  )
  duplicate_tool <- ellmer::tool(
    function(x) x,
    description = "duplicate",
    arguments = list(x = ellmer::type_string()),
    name = "duplicate"
  )
  duplicate_tools <- list(duplicate_tool, duplicate_tool)
  expect_error(
    CodeActModule$new(
      signature = signature("question -> answer"),
      runner = runner,
      tools = duplicate_tools
    ),
    class = "dsprrr_codeact_tools_error"
  )
  expect_error(
    CodeActModule$new(
      signature = signature("question -> answer"),
      runner = runner,
      tools = list(function() 1)
    ),
    class = "dsprrr_codeact_tools_error"
  )

  invalid_alias <- ellmer::tool(
    function(x) x,
    description = "valid intrinsic name",
    arguments = list(x = ellmer::type_string()),
    name = "valid_name"
  )
  for (alias in c("bad name", "bad.name", "bad/name")) {
    expect_error(
      code_act(
        "question -> answer",
        runner = runner,
        tools = stats::setNames(list(invalid_alias), alias)
      ),
      class = "dsprrr_codeact_tools_error",
      info = alias
    )
  }
})

test_that("CodeActModule get_trajectories returns list", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5)
  agent <- code_act("question -> answer", runner = runner)

  trajectories <- agent$get_trajectories()

  expect_type(trajectories, "list")
  expect_length(trajectories, 0) # No trajectories yet
})

test_that("CodeActModule reset_copy creates fresh module", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5)
  agent <- code_act(
    "question -> answer",
    runner = runner,
    max_iterations = 15
  )

  copy <- agent$reset_copy()

  expect_s3_class(copy, "CodeActModule")
  expect_equal(copy$max_iterations, 15L)
  expect_length(copy$state$trajectories, 0)
})

test_that("CodeActModule print works", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5)
  agent <- code_act("question -> answer", runner = runner)

  expect_invisible(print(agent))
})

# ============================================================================
# Forward Execution Tests
# ============================================================================

test_that("CodeActModule forward returns tibble", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  agent <- code_act(
    "question -> answer",
    runner = runner
  )

  mock_llm <- create_mock_codeact_llm(list("42"))

  result <- agent$forward(
    list(question = "What is the answer?"),
    .llm = mock_llm
  )

  expect_s3_class(result, "tbl_df")
  expect_true("output" %in% names(result))
  expect_true("metadata" %in% names(result))
})

test_that("CodeActModule handles data frame input", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  agent <- code_act(
    "x, y -> result",
    runner = runner
  )

  mock_llm <- create_mock_codeact_llm(list("42"))

  batch <- data.frame(x = 6, y = 7)
  result <- agent$forward(batch, .llm = mock_llm)

  expect_s3_class(result, "tbl_df")
  expect_true(result$metadata[[1]]$iterations >= 1)
})

test_that("CodeActModule stores trajectory", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  agent <- code_act(
    "question -> answer",
    runner = runner
  )

  mock_llm <- create_mock_codeact_llm(list("The answer is 42"))

  agent$forward(
    list(question = "What is the answer?"),
    .llm = mock_llm
  )

  trajectories <- agent$get_trajectories()

  expect_length(trajectories, 1)
  expect_true("trajectory" %in% names(trajectories[[1]]))
  expect_true("iterations" %in% names(trajectories[[1]]))
})

test_that("CodeActModule respects trace=FALSE", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  agent <- code_act(
    "question -> answer",
    runner = runner
  )

  mock_llm <- create_mock_codeact_llm(list("42"))

  agent$forward(list(question = "test"), .llm = mock_llm, trace = FALSE)

  # No history should be stored when trace=FALSE
  expect_length(agent$get_trajectories(), 0)
})

# ============================================================================
# Metadata Tests
# ============================================================================

test_that("CodeActModule returns correct metadata", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  agent <- code_act(
    "question -> answer",
    runner = runner
  )

  mock_llm <- create_mock_codeact_llm(list("42"))

  result <- agent$forward(
    list(question = "What?"),
    .llm = mock_llm
  )

  metadata <- result$metadata[[1]]

  expect_equal(metadata$model, "codeact")
  expect_true("iterations" %in% names(metadata))
  expect_true("trajectory_length" %in% names(metadata))
  expect_true("duration_ms" %in% names(metadata))
  expect_true("tools_available" %in% names(metadata))
  expect_true("execute_r_code" %in% metadata$tools_available)
})

# ============================================================================
# Integration with module() factory
# ============================================================================

test_that("module() factory works with type='codeact'", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5)
  agent <- module(
    signature("question -> answer"),
    type = "codeact",
    runner = runner
  )

  expect_s3_class(agent, "CodeActModule")
})

test_that("module() factory requires runner for codeact", {
  expect_error(
    module(
      signature("question -> answer"),
      type = "codeact"
    ),
    "codeact requires a runner"
  )
})

# ============================================================================
# Tool Building Tests
# ============================================================================

test_that("CodeActModule includes execute_r_code tool", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  agent <- code_act(
    "question -> answer",
    runner = runner
  )

  # Access private method to check tools
  tools <- agent$.__enclos_env__$private$build_tools(list(x = 1))

  expect_true("execute_r_code" %in% names(tools))
})

test_that("CodeActModule preserves user tools", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)

  mock_tool <- ellmer::tool(
    fun = function(query) "result",
    description = "Search",
    arguments = list(query = ellmer::type_string())
  )

  agent <- code_act(
    "question -> answer",
    tools = list(search = mock_tool),
    runner = runner
  )

  tools <- agent$.__enclos_env__$private$build_tools(list(x = 1))

  expect_true("search" %in% names(tools))
  expect_true("execute_r_code" %in% names(tools))
  expect_equal(length(tools), 2)
})
