# Code Execution Modules Demo
# Demonstrates Issue #17 acceptance criteria
#
# This script shows how ProgramOfThought and CodeAct modules work.
# Run interactively with an LLM API key set.

library(dsprrr)
library(ellmer)

# =============================================================================
# Acceptance Criterion 1: Default install does NOT silently enable code execution
# =============================================================================

cli::cli_h1("Opt-in Security Model")

# This should error - runner is required (opt-in security)
cli::cli_alert_info("Attempting to create modules without a runner...")

tryCatch(
  program_of_thought("question -> answer"),
  error = function(e) {
    cli::cli_alert_success("program_of_thought() correctly requires runner")
  }
)

tryCatch(
  code_act("question -> answer"),
  error = function(e) {
    cli::cli_alert_success("code_act() correctly requires runner")
  }
)

# =============================================================================
# Setup: Create a runner (explicit opt-in)
# =============================================================================

cli::cli_h1("Runner Setup")

runner <- r_code_runner(
  timeout = 30,
  allowed_packages = c("base", "stats", "utils", "methods")
)

print(runner)

# =============================================================================
# Acceptance Criterion 2: PoT solves deterministic compute tasks reliably
# =============================================================================

if (interactive()) {
  cli::cli_h1("ProgramOfThought: Exact Computation")

  llm <- chat_openai(model = "gpt-4o-mini")
  pot <- program_of_thought("question -> answer", runner = runner)

  # Simple arithmetic
  cli::cli_alert_info("Computing 847 * 293...")
  result1 <- run(pot, question = "What is 847 * 293?", .llm = llm)
  cli::cli_alert_success("Result: {result1$answer}")

  # Prime calculation
  cli::cli_alert_info("Computing sum of primes under 50...")
  result2 <- run(
    pot,
    question = "What is the sum of all prime numbers under 50?",
    .llm = llm
  )
  cli::cli_alert_success("Result: {result2$answer}")
}

# =============================================================================
# Acceptance Criterion 3: PoT retries on code errors with useful context
# =============================================================================

if (interactive()) {
  cli::cli_h1("ProgramOfThought: Error Recovery")

  pot_retry <- program_of_thought(
    "question -> answer",
    runner = runner,
    max_iters = 3
  )

  cli::cli_alert_info("Computing standard deviation (may retry on errors)...")
  result <- run(
    pot_retry,
    question = "Calculate the standard deviation of c(1, 2, 3, 4, 5)",
    .llm = llm
  )

  executions <- pot_retry$get_executions()
  cli::cli_alert_success(
    "Completed in {length(executions[[1]]$iterations)} iteration(s)"
  )
}

# =============================================================================
# Acceptance Criterion 4: CodeAct can solve tool-only, code-only, and hybrid tasks
# =============================================================================

if (interactive()) {
  cli::cli_h1("CodeAct: Hybrid Tool + Code Agent")

  # Create a simple lookup tool
  lookup_tool <- ellmer::tool(
    fun = function(country) {
      populations <- list(
        "France" = 67000000,
        "Germany" = 83000000,
        "UK" = 67000000,
        "USA" = 330000000
      )
      populations[[country]] %||% "Unknown"
    },
    description = "Look up population of a country",
    arguments = list(country = ellmer::type_string())
  )

  agent <- code_act(
    "question -> answer",
    tools = list(lookup = lookup_tool),
    runner = runner,
    max_iterations = 5
  )

  cli::cli_alert_info("Hybrid task: lookup population, then compute 15%...")
  result <- run(
    agent,
    question = "What is 15% of France's population?",
    .llm = llm
  )
  cli::cli_alert_success("Result: {result$answer}")
}

# =============================================================================
# Acceptance Criterion 5: Traces include generated code, execution outputs, etc.
# =============================================================================

if (interactive()) {
  cli::cli_h1("Tracing and Debugging")

  # ProgramOfThought traces
  pot_trace <- program_of_thought("question -> answer", runner = runner)
  run(pot_trace, question = "Calculate 2^10", .llm = llm)

  executions <- pot_trace$get_executions()
  exec <- executions[[1]]

  cli::cli_h2("ProgramOfThought Execution Trace")
  cli::cli_ul(c(
    "Timestamp: {exec$timestamp}",
    "Inputs: {paste(names(exec$inputs), collapse = ', ')}",
    "Iterations: {length(exec$iterations)}",
    "Success: {exec$success}"
  ))

  # CodeAct trajectories
  agent_trace <- code_act("question -> answer", runner = runner)
  run(agent_trace, question = "What is sqrt(144)?", .llm = llm)

  trajectories <- agent_trace$get_trajectories()
  traj <- trajectories[[1]]

  cli::cli_h2("CodeAct Trajectory")
  cli::cli_ul(c(
    "Timestamp: {traj$timestamp}",
    "Iterations: {traj$iterations}",
    "Trajectory steps: {length(traj$trajectory)}"
  ))
}

# =============================================================================
# Security Model Demonstration
# =============================================================================

cli::cli_h1("Security Features")

runner <- r_code_runner(timeout = 5)

# Helper to test and report
test_blocked <- function(code, description) {
  result <- runner$execute(code)
  if (!result$success) {
    cli::cli_alert_success("{description}: blocked")
  } else {
    cli::cli_alert_danger("{description}: NOT blocked (unexpected)")
  }
}

test_blocked("system('ls')", "system()")
test_blocked("base::system('ls')", "base::system()")
test_blocked("unlink('/tmp/test')", "unlink()")

# Package loading enforcement
runner_strict <- r_code_runner(
  timeout = 5,
  allowed_packages = c("base", "stats")
)
result <- runner_strict$execute("library(dplyr)")
if (!result$success) {
  cli::cli_alert_success("Disallowed package (dplyr): blocked")
} else {
  cli::cli_alert_danger("Disallowed package (dplyr): NOT blocked (unexpected)")
}

cli::cli_h1("Demo Complete")
cli::cli_alert_success("All acceptance criteria demonstrated.")
