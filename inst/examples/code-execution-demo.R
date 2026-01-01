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

# This should error - runner is required (opt-in security)
try(program_of_thought("question -> answer"))
#> Error: Code execution requires an explicit runner

try(code_act("question -> answer"))
#> Error: CodeAct requires an explicit runner for code execution

# =============================================================================
# Setup: Create a runner (explicit opt-in)
# =============================================================================

runner <- r_code_runner(
  timeout = 30,
  allowed_packages = c("base", "stats", "utils", "methods")
)

print(runner)
#> RCodeRunner
#> * Timeout: 30 seconds
#> * Max output: 100000 chars
#> * Allowed packages: base, stats, utils, methods

# =============================================================================
# Acceptance Criterion 2: PoT solves deterministic compute tasks reliably
# =============================================================================

# Create ProgramOfThought module
pot <- program_of_thought("question -> answer", runner = runner)

# Test with arithmetic (LLMs often fail these, R computes exactly)
if (interactive()) {
  llm <- chat_openai(model = "gpt-4o-mini")

  # Simple arithmetic
  result1 <- run(pot, question = "What is 847 * 293?", .llm = llm)
  cat("847 * 293 =", result1$output[[1]]$answer, "\n")
  # Expected: 248171 (R computes exactly)


  # Prime calculation
  result2 <- run(pot, question = "What is the sum of all prime numbers under 50?", .llm = llm)
  cat("Sum of primes under 50:", result2$output[[1]]$answer, "\n")
  # Expected: 328
}

# =============================================================================
# Acceptance Criterion 3: PoT retries on code errors with useful context
# =============================================================================

pot_retry <- program_of_thought(
  "question -> answer",
  runner = runner,
  max_iters = 3  # Allow up to 3 attempts
)

if (interactive()) {
  # This might trigger a retry if first attempt has a bug
  result <- run(
    pot_retry,
    question = "Calculate the standard deviation of c(1, 2, 3, 4, 5)",
    .llm = llm
  )

  # Check execution history for retries
  executions <- pot_retry$get_executions()
  cat("Iterations used:", executions[[1]]$iterations |> length(), "\n")
  cat("Success:", executions[[1]]$success, "\n")
}

# =============================================================================
# Acceptance Criterion 4: CodeAct can solve tool-only, code-only, and hybrid tasks
# =============================================================================

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

# Create CodeAct agent with tools
agent <- code_act(
  "question -> answer",
  tools = list(lookup = lookup_tool),
  runner = runner,
  max_iterations = 5
)

if (interactive()) {
  # Hybrid task: lookup then compute
  result <- run(
    agent,
    question = "What is 15% of France's population?",
    .llm = llm
  )
  cat("15% of France's population:", result$output[[1]]$answer, "\n")
  # Agent might: 1) Call lookup("France") to get 67000000
  #              2) Execute: 67000000 * 0.15 to get 10050000
}

# =============================================================================
# Acceptance Criterion 5: Traces include generated code, execution outputs, etc.
# =============================================================================

if (interactive()) {
  # ProgramOfThought traces
  pot_trace <- program_of_thought("question -> answer", runner = runner)
  run(pot_trace, question = "Calculate 2^10", .llm = llm)

  executions <- pot_trace$get_executions()
  cat("\n=== ProgramOfThought Trace ===\n")
  cat("Timestamp:", as.character(executions[[1]]$timestamp), "\n")
  cat("Inputs:", names(executions[[1]]$inputs), "\n")
  cat("Iterations:", length(executions[[1]]$iterations), "\n")
  cat("Success:", executions[[1]]$success, "\n")

  # Each iteration contains:
  iter1 <- executions[[1]]$iterations[[1]]
  cat("\nIteration 1:\n")
  cat("  Code generated:", substr(iter1$code, 1, 50), "...\n")
  cat("  Explanation:", iter1$explanation, "\n")
  cat("  Execution success:", iter1$execution$success, "\n")
  cat("  Execution result:", iter1$execution$result, "\n")

  # CodeAct trajectories
  agent_trace <- code_act("question -> answer", runner = runner)
  run(agent_trace, question = "What is sqrt(144)?", .llm = llm)

  trajectories <- agent_trace$get_trajectories()
  cat("\n=== CodeAct Trajectory ===\n")
  cat("Timestamp:", as.character(trajectories[[1]]$timestamp), "\n")
  cat("Iterations:", trajectories[[1]]$iterations, "\n")
  cat("Trajectory length:", length(trajectories[[1]]$trajectory), "\n")
}

# =============================================================================
# Security Model Demonstration
# =============================================================================

cat("\n=== Security Features ===\n")

# Dangerous patterns are blocked
runner <- r_code_runner(timeout = 5)

# system() blocked
result <- runner$execute("system('ls')")
cat("system() blocked:", !result$success, "\n")
cat("Error:", result$error, "\n")

# base::system() bypass also blocked
result <- runner$execute("base::system('ls')")
cat("base::system() blocked:", !result$success, "\n")

# unlink() blocked
result <- runner$execute("unlink('/tmp/test')")
cat("unlink() blocked:", !result$success, "\n")

# Package loading enforced
runner_strict <- r_code_runner(
  timeout = 5,
  allowed_packages = c("base", "stats")
)
result <- runner_strict$execute("library(dplyr)")
cat("Disallowed package blocked:", !result$success, "\n")

cat("\n=== Demo Complete ===\n")
cat("All acceptance criteria demonstrated.\n")
