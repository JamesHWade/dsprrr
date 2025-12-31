# Code Execution Modules: ProgramOfThought and CodeAct

## Summary

Implement code execution modules that enable LLMs to solve problems by generating and executing R code. This extends dsprrr with two powerful reasoning patterns from DSPy:

- **ProgramOfThought**: Generate R code to solve computational problems, execute safely via `callr`, return results
- **CodeAct**: Combine code generation with tool calling for hybrid agentic workflows

These modules address a fundamental limitation of language models: while LLMs excel at reasoning about problems, they're notoriously unreliable at arithmetic, symbolic manipulation, and precise computation. By delegating computation to actual code execution, we get the best of both worlds.

## Motivation

### Why Code Execution Matters

| Task Type | ChainOfThought | ProgramOfThought |
|-----------|---------------|------------------|
| "What is 847 * 293?" | Often wrong | Always correct |
| "Calculate compound interest over 30 years" | Approximates | Exact result |
| "Find all primes under 1000" | Hallucinates | Correct list |
| "Parse this JSON and extract field X" | Unreliable | Precise |
| "Statistical analysis of dataset" | Vague | Actual numbers |

Code execution transforms LLMs from unreliable calculators into powerful computational orchestrators.

### R-Native Advantages

R is uniquely positioned for scientific and statistical code execution:

- **Statistical computing**: Native support for complex statistical operations
- **Data manipulation**: Tidyverse ecosystem for data wrangling
- **Visualization**: ggplot2 for generating plots as output
- **Domain packages**: Access to CRAN's 20,000+ packages for specialized tasks
- **Reproducibility**: R's functional paradigm aligns well with LLM-generated code

### Use Cases

1. **Data Analysis**: "Analyze this dataset and report the correlation matrix"
2. **Mathematical Computation**: "Solve this system of equations"
3. **Statistical Inference**: "Run a t-test on these two groups"
4. **Data Transformation**: "Pivot this data from wide to long format"
5. **Visualization**: "Create a scatter plot showing the relationship between X and Y"
6. **Code-Assisted Research**: "Calculate the effect size from these study parameters"

---

## Scope

### In Scope
- [ ] `ProgramOfThoughtModule` - Generate and execute R code
- [ ] `CodeActModule` - Hybrid code generation + tool calling
- [ ] Safe execution via `callr::r()` subprocess
- [ ] Iterative error recovery with feedback
- [ ] Integration with `run()`, `evaluate()`, `optimize_grid()`
- [ ] Support for constrained execution environments

### Out of Scope (Future Work)
- [ ] Multi-language code execution (Python, JavaScript)
- [ ] Persistent execution environments across calls
- [ ] GPU-accelerated code execution
- [ ] Remote code execution (cloud functions)
- [ ] Visual output capture (plots as return values)

---

## Technical Design

### Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                    ProgramOfThoughtModule                    │
├─────────────────────────────────────────────────────────────┤
│  1. LLM generates R code via ChainOfThought                 │
│  2. Code parsed and validated                               │
│  3. Executed in sandboxed subprocess (callr::r())           │
│  4. Results captured and returned                           │
│  5. On error: feedback loop with error context              │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                      CodeActModule                           │
├─────────────────────────────────────────────────────────────┤
│  Extends ProgramOfThought + ReactModule                     │
│  • Can call registered tools via ellmer                     │
│  • Can generate code for complex computation                │
│  • Decides dynamically: tool call vs code execution         │
└─────────────────────────────────────────────────────────────┘
```

### Execution Environment

The code execution sandbox provides:

| Feature | Implementation |
|---------|---------------|
| Process isolation | `callr::r()` subprocess |
| Timeout protection | `timeout` parameter (default: 30s) |
| Memory limits | OS-level via `callr` |
| Package whitelist | Configurable allowed packages |
| Error capture | Full stack traces via `callr_error` |
| Output capture | stdout/stderr routing |

### Security Model

```r
# Default: Conservative sandbox
CodeInterpreter$new(
  timeout = 30,
  allowed_packages = c("base", "stats", "utils"),  # Minimal

max_output_size = 1e6,  # 1MB limit
  allow_file_access = FALSE,
  allow_network = FALSE
)

# Research mode: More permissive
CodeInterpreter$new(
  timeout = 120,
  allowed_packages = c("base", "stats", "dplyr", "tidyr", "ggplot2"),
  allow_file_access = TRUE,  # Read-only
  allow_network = FALSE
)
```

**Security Note**: Code execution always carries inherent risk. The `callr` subprocess provides isolation but not complete sandboxing. For production use with untrusted inputs, additional OS-level sandboxing (containers, AppArmor) is recommended per [RAppArmor best practices](https://cran.r-project.org/web/packages/RAppArmor/vignettes/v55i07.pdf).

---

## Implementation Plan

### 1. CodeInterpreter Helper Class

**Purpose**: Encapsulate safe R code execution with configurable constraints.

**API Design**:
```r
# Create interpreter with constraints
interpreter <- CodeInterpreter$new(
  timeout = 30,
  allowed_packages = c("base", "stats", "dplyr"),
  prelude = "library(dplyr)",  # Code run before each execution
  max_output_size = 1e6
)

# Execute code
result <- interpreter$execute("
  x <- 1:100
  mean(x^2)
")
# list(success = TRUE, output = "3383.5", error = NULL, duration_ms = 12)

# Handle errors gracefully
result <- interpreter$execute("stop('oops')")
# list(success = FALSE, output = NULL, error = "Error: oops", duration_ms = 5)

# Cleanup (optional, for long-running interpreters)
interpreter$shutdown()
```

**Implementation Details**:

| File | Changes |
|------|---------|
| `R/code-interpreter.R` (new) | `CodeInterpreter` R6 class |
| `tests/testthat/test-code-interpreter.R` (new) | Unit tests |

**CodeInterpreter R6 Class**:
```r
CodeInterpreter <- R6::R6Class(
  "CodeInterpreter",
  public = list(
    timeout = NULL,
    allowed_packages = NULL,
    prelude = NULL,
    max_output_size = NULL,

    initialize = function(
      timeout = 30,
      allowed_packages = c("base", "stats", "utils"),
      prelude = "",
      max_output_size = 1e6
    ) {
      self$timeout <- timeout
      self$allowed_packages <- allowed_packages
      self$prelude <- prelude
      self$max_output_size <- max_output_size
    },

    execute = function(code) {
      private$validate_code(code)
      private$run_in_subprocess(code)
    },

    shutdown = function() {
      # Cleanup any persistent resources
    }
  ),

  private = list(
    validate_code = function(code) {
      # Check for disallowed patterns (system calls, file I/O, etc.)
      # This is defense-in-depth, not primary security
    },

    run_in_subprocess = function(code) {
      full_code <- paste(self$prelude, code, sep = "\n")

      tryCatch({
        result <- callr::r(
          function(code_to_run) {
            # Parse and execute the code string in subprocess
            # Uses source(textConnection()) pattern for safety
            output <- capture.output({
              con <- textConnection(code_to_run)
              on.exit(close(con))
              result <- source(con, local = TRUE)$value
            })
            list(
              result = result,
              output = paste(output, collapse = "\n")
            )
          },
          args = list(code_to_run = full_code),
          timeout = self$timeout,
          error = "error"
        )

        list(
          success = TRUE,
          result = result$result,
          output = result$output,
          error = NULL
        )
      }, callr_timeout_error = function(e) {
        list(
          success = FALSE,
          result = NULL,
          output = NULL,
          error = sprintf("Execution timed out after %d seconds", self$timeout)
        )
      }, error = function(e) {
        list(
          success = FALSE,
          result = NULL,
          output = NULL,
          error = conditionMessage(e)
        )
      })
    }
  )
)
```

---

### 2. ProgramOfThoughtModule

**Purpose**: Generate R code to solve problems, execute, and return results.

**API Design**:
```r
# Create module
pot <- program_of_thought(
  signature("question -> answer"),
  max_iters = 3,
  interpreter = CodeInterpreter$new(timeout = 30)
)

# Or via module factory
pot <- module(
  signature("question -> answer"),
  type = "program_of_thought",
  max_iters = 3
)

# Execute
result <- run(pot,
  question = "What is the sum of all prime numbers under 100?",
  .llm = llm
)
# answer = "1060"

# With data context
pot_data <- program_of_thought(
  signature("data, question -> answer"),
  max_iters = 3,
  allowed_packages = c("base", "stats", "dplyr")
)

result <- run(pot_data,
  data = mtcars,
  question = "What is the correlation between mpg and hp?",
  .llm = llm
)
# answer = "-0.776"
```

**Implementation Details**:

| File | Changes |
|------|---------|
| `R/module-pot.R` (new) | `ProgramOfThoughtModule` R6 class |
| `R/module.R` | Add `type = "program_of_thought"` |
| `tests/testthat/test-module-pot.R` (new) | Unit tests |

**ProgramOfThoughtModule R6 Class**:
```r
ProgramOfThoughtModule <- R6::R6Class(
  "ProgramOfThoughtModule",
  inherit = Module,

  public = list(
    max_iters = NULL,
    interpreter = NULL,

    # Internal ChainOfThought predictors
    code_generator = NULL,
    code_regenerator = NULL,
    answer_extractor = NULL,

    initialize = function(
      signature,
      max_iters = 3L,
      interpreter = NULL,
      config = list(),
      chat = NULL
    ) {
      super$initialize(signature, config, chat)

      self$max_iters <- as.integer(max_iters)
      self$interpreter <- interpreter %||% CodeInterpreter$new()

      # Build internal signatures
      private$build_internal_signatures()
    },

    forward = function(batch, .llm = NULL, trace = TRUE, ...) {
      inputs <- private$normalize_inputs(batch)
      llm <- .llm %||% self$chat %||% private$get_default_llm()

      # Phase 1: Generate initial code
      code_result <- self$code_generator$forward(inputs, .llm = llm)
      code <- private$parse_code(code_result$output[[1]]$code)

      # Phase 2: Execute with retry loop
      for (iter in seq_len(self$max_iters)) {
        exec_result <- self$interpreter$execute(code)

        if (exec_result$success) {
          # Phase 3: Extract answer from execution output
          answer_inputs <- c(inputs, list(
            code = code,
            execution_output = exec_result$output
          ))

          final_result <- self$answer_extractor$forward(
            answer_inputs,
            .llm = llm
          )

          # Record trace and return
          if (trace) {
            private$record_trace(inputs, code, exec_result, final_result)
          }

          return(final_result)
        }

        # Regenerate code with error context
        if (iter < self$max_iters) {
          regen_inputs <- c(inputs, list(
            previous_code = code,
            error_message = exec_result$error
          ))

          code_result <- self$code_regenerator$forward(
            regen_inputs,
            .llm = llm
          )
          code <- private$parse_code(code_result$output[[1]]$code)
        }
      }

      cli::cli_abort(c(
        "ProgramOfThought failed after {self$max_iters} iterations",
        "i" = "Last error: {exec_result$error}"
      ))
    }
  ),

  private = list(
    build_internal_signatures = function() {
      # Code generation signature
      gen_sig <- Signature(
        inputs = self$signature@inputs,
        output_type = ellmer::type_object(
          reasoning = ellmer::type_string(
            .description = "Reasoning about how to solve this with R code"
          ),
          code = ellmer::type_string(
            .description = "R code that computes the answer. Must be valid R syntax."
          )
        ),
        instructions = private$generation_instructions()
      )
      self$code_generator <- PredictModule$new(gen_sig)

      # Code regeneration signature (with error context)
      regen_inputs <- c(
        self$signature@inputs,
        list(
          input("previous_code", "The R code that failed"),
          input("error_message", "The error message from execution")
        )
      )
      regen_sig <- Signature(
        inputs = regen_inputs,
        output_type = ellmer::type_object(
          reasoning = ellmer::type_string(
            .description = "Analysis of what went wrong and how to fix it"
          ),
          code = ellmer::type_string(
            .description = "Corrected R code"
          )
        ),
        instructions = private$regeneration_instructions()
      )
      self$code_regenerator <- PredictModule$new(regen_sig)

      # Answer extraction signature
      extract_inputs <- c(
        self$signature@inputs,
        list(
          input("code", "The R code that was executed"),
          input("execution_output", "The output from code execution")
        )
      )
      extract_sig <- Signature(
        inputs = extract_inputs,
        output_type = self$signature@output_type,
        instructions = private$extraction_instructions()
      )
      self$answer_extractor <- PredictModule$new(extract_sig)
    },

    generation_instructions = function() {
      "You are an expert R programmer. Generate clean, executable R code to solve the given problem.

Rules:
- Write valid R code that will execute without errors
- The code should compute and print/return the final answer
- Use only base R and standard packages unless specified otherwise
- Do not include library() calls unless absolutely necessary
- Avoid side effects (file I/O, network calls, plotting)
- Keep the code concise and focused on the computation"
    },

    regeneration_instructions = function() {
      "The previous R code failed to execute. Analyze the error and generate corrected code.

Rules:
- Carefully read the error message to understand what went wrong
- Fix the specific issue while preserving the overall approach
- If the approach is fundamentally flawed, try a different strategy
- Ensure the corrected code is syntactically valid R"
    },

    extraction_instructions = function() {
      "Extract the final answer from the code execution output.

Rules:
- Report only the computed result, not the code or reasoning
- Format the answer appropriately for the question
- If the output contains multiple values, summarize as appropriate"
    },

    parse_code = function(code_text) {
      # Remove markdown code fences if present
      code_text <- gsub("^```r?\\n?", "", code_text)
      code_text <- gsub("\\n?```$", "", code_text)
      trimws(code_text)
    },

    record_trace = function(inputs, code, exec_result, final_result) {
      trace_entry <- list(
        timestamp = Sys.time(),
        inputs = inputs,
        generated_code = code,
        execution_result = exec_result,
        output = final_result$output[[1]],
        iterations = 1  # Will be updated in retry loop
      )
      self$state$traces <- append(self$state$traces, list(trace_entry))
    }
  )
)
```

**Factory Function**:
```r
#' Create a ProgramOfThought module
#'
#' @param signature Signature defining inputs and output
#' @param max_iters Maximum code generation attempts (default: 3)
#' @param interpreter CodeInterpreter instance (optional)
#' @param allowed_packages Character vector of allowed R packages
#' @param timeout Execution timeout in seconds (default: 30)
#' @param chat Optional ellmer Chat object
#' @return ProgramOfThoughtModule
#' @export
program_of_thought <- function(
  signature,
  max_iters = 3L,
  interpreter = NULL,
  allowed_packages = c("base", "stats", "utils"),
  timeout = 30,
  chat = NULL
) {
  if (is.null(interpreter)) {
    interpreter <- CodeInterpreter$new(
      timeout = timeout,
      allowed_packages = allowed_packages
    )
  }

  ProgramOfThoughtModule$new(
    signature = signature,
    max_iters = max_iters,
    interpreter = interpreter,
    chat = chat
  )
}
```

---

### 3. CodeActModule

**Purpose**: Hybrid agent combining code execution with tool calling.

**Design Philosophy**: CodeAct extends both ReactModule and ProgramOfThoughtModule patterns. The LLM can:
1. Call registered tools (like ReactModule)
2. Generate and execute R code (like ProgramOfThought)
3. Choose dynamically based on the task

This is powerful for scenarios where some information requires tool calls (web search, database queries) while computation requires code execution.

**API Design**:
```r
# Create CodeAct module with tools
search_tool <- ellmer::tool(
  search_fn,
  description = "Search for information",
  arguments = list(query = ellmer::type_string())
)

codeact <- code_act(
  signature("question -> answer"),
  tools = list(search_tool),
  max_iters = 5,
  interpreter = CodeInterpreter$new(
    allowed_packages = c("base", "stats", "dplyr")
  )
)

# Or via module factory
codeact <- module(
  signature("question -> answer"),
  type = "codeact",
  tools = list(search_tool),
  max_iters = 5
)

# Execute - agent decides whether to use tools, code, or both
result <- run(codeact,
  question = "What is the population of France divided by the area of Texas?",
  .llm = llm
)
# Agent: 1) Calls search for France population
#        2) Calls search for Texas area
#        3) Generates code: 67390000 / 268596
#        4) Returns: "250.9 people per square mile"
```

**Implementation Details**:

| File | Changes |
|------|---------|
| `R/module-codeact.R` (new) | `CodeActModule` R6 class |
| `R/module.R` | Add `type = "codeact"` |
| `tests/testthat/test-module-codeact.R` (new) | Unit tests |

**CodeActModule R6 Class**:
```r
CodeActModule <- R6::R6Class(
  "CodeActModule",
  inherit = Module,

  public = list(
    tools = NULL,
    max_iters = NULL,
    interpreter = NULL,

    initialize = function(
      signature,
      tools = list(),
      max_iters = 5L,
      interpreter = NULL,
      config = list(),
      chat = NULL
    ) {
      super$initialize(signature, config, chat)

      self$tools <- tools
      self$max_iters <- as.integer(max_iters)
      self$interpreter <- interpreter %||% CodeInterpreter$new()

      private$build_codeact_signature()
    },

    forward = function(batch, .llm = NULL, trace = TRUE, ...) {
      inputs <- private$normalize_inputs(batch)
      llm <- .llm %||% self$chat %||% private$get_default_llm()

      # Register tools on the LLM
      for (tool in self$tools) {
        llm$register_tool(tool)
      }

      # Register code execution as a special tool
      code_tool <- private$create_code_execution_tool()
      llm$register_tool(code_tool)

      # Build initial prompt with available capabilities
      prompt <- private$build_codeact_prompt(inputs)

      trajectory <- list()

      for (iter in seq_len(self$max_iters)) {
        # Get LLM response
        response <- llm$chat(prompt, echo = "none")
        last_turn <- llm$last_turn(role = "assistant")

        # Check for tool requests
        tool_requests <- private$extract_tool_requests(last_turn)

        if (length(tool_requests) == 0) {
          # No tool calls - check if finished
          break
        }

        # Process each tool request
        for (req in tool_requests) {
          trajectory <- append(trajectory, list(list(
            iteration = iter,
            action = req$name,
            input = req$arguments,
            observation = NULL  # Will be filled by ellmer
          )))
        }

        # Continue conversation (ellmer handles tool execution)
        prompt <- ""
      }

      # Extract final answer
      final_result <- llm$chat_structured(
        "Based on the trajectory above, provide your final answer.",
        type = self$signature@output_type,
        echo = "none"
      )

      # Record trace
      if (trace) {
        private$record_trace(inputs, trajectory, final_result)
      }

      tibble::tibble(
        output = list(final_result),
        chat = list(llm),
        metadata = list(list(
          iterations = length(trajectory),
          trajectory = trajectory
        ))
      )
    },

    add_tool = function(tool) {
      self$tools <- c(self$tools, list(tool))
      invisible(self)
    }
  ),

  private = list(
    create_code_execution_tool = function() {
      interpreter <- self$interpreter

      ellmer::tool(
        function(code) {
          result <- interpreter$execute(code)
          if (result$success) {
            sprintf("Output: %s", result$output)
          } else {
            sprintf("Error: %s", result$error)
          }
        },
        name = "execute_r_code",
        description = "Execute R code and return the output. Use this for computations, data manipulation, or any task that benefits from precise code execution.",
        arguments = list(
          code = ellmer::type_string("R code to execute")
        )
      )
    },

    build_codeact_prompt = function(inputs) {
      tool_descriptions <- vapply(self$tools, function(t) {
        sprintf("- %s: %s", t@name, t@description)
      }, character(1))

      glue::glue("
You are an intelligent agent that can use tools and execute R code to solve problems.

## Available Tools
{paste(tool_descriptions, collapse = '\n')}
- execute_r_code: Execute R code and return the output

## Instructions
1. Analyze the problem and decide whether to use tools, code, or both
2. For information retrieval, use the appropriate tool
3. For computation, data analysis, or precise calculations, use execute_r_code
4. You can combine multiple steps to solve complex problems
5. When you have gathered all necessary information and computed the result, provide your final answer

## Problem
{private$format_inputs(inputs)}
")
    },

    format_inputs = function(inputs) {
      lines <- vapply(names(inputs), function(name) {
        sprintf("%s: %s", name, inputs[[name]])
      }, character(1))
      paste(lines, collapse = "\n")
    },

    extract_tool_requests = function(turn) {
      requests <- list()
      for (content in turn@contents) {
        if (inherits(content, "ContentToolRequest")) {
          requests <- append(requests, list(list(
            name = content@name,
            arguments = content@arguments
          )))
        }
      }
      requests
    },

    build_codeact_signature = function() {
      # Internal signature for action generation
    },

    record_trace = function(inputs, trajectory, final_result) {
      trace_entry <- list(
        timestamp = Sys.time(),
        inputs = inputs,
        trajectory = trajectory,
        output = final_result
      )
      self$state$traces <- append(self$state$traces, list(trace_entry))
    }
  )
)
```

**Factory Function**:
```r
#' Create a CodeAct module
#'
#' @param signature Signature defining inputs and output
#' @param tools List of ellmer ToolDef objects
#' @param max_iters Maximum iterations (default: 5)
#' @param interpreter CodeInterpreter instance (optional)
#' @param allowed_packages Character vector of allowed R packages
#' @param timeout Execution timeout in seconds (default: 30)
#' @param chat Optional ellmer Chat object
#' @return CodeActModule
#' @export
code_act <- function(
  signature,
  tools = list(),
  max_iters = 5L,
  interpreter = NULL,
  allowed_packages = c("base", "stats", "utils"),
  timeout = 30,
  chat = NULL
) {
  if (is.null(interpreter)) {
    interpreter <- CodeInterpreter$new(
      timeout = timeout,
      allowed_packages = allowed_packages
    )
  }

  CodeActModule$new(
    signature = signature,
    tools = tools,
    max_iters = max_iters,
    interpreter = interpreter,
    chat = chat
  )
}
```

---

### 4. Module Factory Update

**Changes to `R/module.R`**:
```r
module <- function(
  signature,
  type = "predict",
  tools = NULL,
  max_iterations = 10L,
  max_iters = 3L,          # For program_of_thought and codeact
  M = 3L,
  temperature = 0.7,
  interpreter = NULL,      # For code execution modules
  allowed_packages = c("base", "stats", "utils"),
  timeout = 30,
  template = "",
  demos = list(),
  config = list(),
  chat = NULL,
  ...
) {
  # ... existing validation ...

  # Auto-upgrade to codeact if tools + code execution requested
  if (!is.null(tools) && length(tools) > 0 && type == "program_of_thought") {
    type <- "codeact"
  }

  type <- match.arg(type, c(
    "predict", "react", "multichain",
    "program_of_thought", "codeact"  # New types
  ))

  switch(
    type,
    # ... existing cases ...

    program_of_thought = {
      interp <- interpreter %||% CodeInterpreter$new(
        timeout = timeout,
        allowed_packages = allowed_packages
      )
      ProgramOfThoughtModule$new(
        signature = signature,
        max_iters = max_iters,
        interpreter = interp,
        config = config,
        chat = chat
      )
    },

    codeact = {
      interp <- interpreter %||% CodeInterpreter$new(
        timeout = timeout,
        allowed_packages = allowed_packages
      )
      CodeActModule$new(
        signature = signature,
        tools = tools %||% list(),
        max_iters = max_iters,
        interpreter = interp,
        config = config,
        chat = chat
      )
    },

    cli::cli_abort("Unknown module type: {type}")
  )
}
```

---

## Integration Points

### Optimization Support

Both modules work with existing optimization infrastructure:

```r
# Grid search over execution parameters
pot <- program_of_thought(sig)
optimize_grid(
  pot,
  devset = train_data,
  metric = metric_exact_match(),
  parameters = list(
    max_iters = c(1, 3, 5),
    temperature = c(0.3, 0.7)  # For code generation
  )
)

# CodeAct optimization
codeact <- code_act(sig, tools = tools)
optimize_grid(
  codeact,
  parameters = list(max_iters = c(3, 5, 10))
)
```

### Teleprompter Compatibility

```r
# Add demonstrations to code generation
tp <- LabeledFewShot(k = 3)
compiled_pot <- compile(tp, pot, trainset)

# Few-shot examples help LLM generate better code
```

### Tracing & Observability

```r
# ProgramOfThought traces
pot$get_traces()
# tibble: inputs, generated_code, execution_result, iterations, output

# CodeAct traces
codeact$get_traces()
# tibble: inputs, trajectory (tool calls + code executions), output

# Cost tracking across iterations
pot$trace_summary()$total_cost
```

### Data Context

A key feature: passing R objects as context for code execution.

```r
# Module can reference data objects in generated code
pot_data <- program_of_thought(
  signature("data, question -> answer"),
  interpreter = CodeInterpreter$new(
    prelude = "data <- .context$data"  # Inject data
  )
)

result <- run(pot_data,
  data = mtcars,
  question = "What is the mean mpg by cylinder count?",
  .llm = llm
)
# Generated code: data %>% group_by(cyl) %>% summarise(mean_mpg = mean(mpg))
```

---

## Implementation Checklist

### Phase 1: CodeInterpreter (2-3 days)
- [ ] Create `R/code-interpreter.R`
- [ ] Implement `CodeInterpreter` R6 class
- [ ] Add callr-based execution with timeout
- [ ] Implement code validation (defense-in-depth)
- [ ] Add output size limiting
- [ ] Write comprehensive unit tests
- [ ] Test timeout behavior
- [ ] Test error capture and reporting

### Phase 2: ProgramOfThoughtModule (3-4 days)
- [ ] Create `R/module-pot.R`
- [ ] Implement `ProgramOfThoughtModule` R6 class
- [ ] Build internal signatures (generate, regenerate, extract)
- [ ] Implement retry loop with error feedback
- [ ] Add code parsing (handle markdown fences)
- [ ] Create `program_of_thought()` factory function
- [ ] Update `module()` for `type = "program_of_thought"`
- [ ] Write unit tests with mock LLM
- [ ] Record VCR cassettes for integration tests

### Phase 3: CodeActModule (3-4 days)
- [ ] Create `R/module-codeact.R`
- [ ] Implement `CodeActModule` R6 class
- [ ] Create code execution tool wrapper
- [ ] Implement trajectory tracking
- [ ] Build agentic prompt construction
- [ ] Create `code_act()` factory function
- [ ] Update `module()` for `type = "codeact"`
- [ ] Write unit tests
- [ ] Record VCR cassettes

### Phase 4: Integration & Polish (2-3 days)
- [ ] Ensure `run()`, `evaluate()`, `optimize_grid()` work correctly
- [ ] Add support for data context injection
- [ ] Update `module_trials()` and `module_metrics()` helpers
- [ ] Performance testing with real LLMs
- [ ] Documentation: roxygen2 + examples
- [ ] Update CLAUDE.md implementation status

### Phase 5: Documentation & Vignettes (1-2 days)
- [ ] Create vignette: `code-execution.Rmd`
- [ ] Add examples to README.Rmd
- [ ] Update pkgdown reference index
- [ ] Run `devtools::check()` and fix issues

---

## Testing Strategy

### Unit Tests (Mock LLM)

```r
test_that("CodeInterpreter executes valid R code", {
  interp <- CodeInterpreter$new(timeout = 5)
  result <- interp$execute("1 + 1")
  expect_true(result$success)
  expect_equal(result$result, 2)
})

test_that("CodeInterpreter handles timeout", {
  interp <- CodeInterpreter$new(timeout = 1)
  result <- interp$execute("Sys.sleep(10)")
  expect_false(result$success)
  expect_match(result$error, "timeout")
})

test_that("ProgramOfThought retries on error", {
  mock_llm <- MockLLM$new(responses = list(
    list(code = "bad_code"),  # First attempt fails
    list(code = "1 + 1")      # Retry succeeds
  ))

  pot <- program_of_thought(sig, max_iters = 3)
  result <- run(pot, question = "test", .llm = mock_llm)

  expect_equal(mock_llm$call_count, 3)  # generate, regenerate, extract
})
```

### Integration Tests (VCR Cassettes)

```r
test_that("ProgramOfThought solves math problems", {
  skip_if_not_installed("vcr")
  vcr::local_cassette("pot-math")

  llm <- ellmer::chat_openai(model = "gpt-4o-mini")
  pot <- program_of_thought(signature("question -> answer"))

  result <- run(pot, question = "What is 847 * 293?", .llm = llm)
  expect_equal(result$output[[1]]$answer, "248171")
})

test_that("CodeAct uses tools and code together", {
  skip_if_not_installed("vcr")
  vcr::local_cassette("codeact-hybrid")

  search_tool <- ellmer::tool(
    function(query) "France population: 67.39 million",
    name = "search",
    description = "Search for information"
  )

  llm <- ellmer::chat_openai(model = "gpt-4o-mini")
  codeact <- code_act(sig, tools = list(search_tool))

  result <- run(codeact,
    question = "What is 10% of France's population?",
    .llm = llm
  )

  expect_match(result$output[[1]]$answer, "6.7")
})
```

### Example VCR Cassettes to Record

- `tests/_vcr/pot-basic.yml` - Simple computation
- `tests/_vcr/pot-retry.yml` - Error recovery scenario
- `tests/_vcr/pot-data-context.yml` - With data frame input
- `tests/_vcr/codeact-basic.yml` - Code execution only
- `tests/_vcr/codeact-tools.yml` - Tool calling only
- `tests/_vcr/codeact-hybrid.yml` - Mixed tool + code

---

## API Summary

| Function | Purpose | Returns |
|----------|---------|---------|
| `CodeInterpreter$new(...)` | Create code execution sandbox | CodeInterpreter |
| `interpreter$execute(code)` | Execute R code safely | list(success, output, error) |
| `program_of_thought(sig, ...)` | Create PoT module | ProgramOfThoughtModule |
| `code_act(sig, tools, ...)` | Create CodeAct module | CodeActModule |
| `module(sig, type = "program_of_thought")` | Factory for PoT | ProgramOfThoughtModule |
| `module(sig, type = "codeact", tools)` | Factory for CodeAct | CodeActModule |

---

## Security Considerations

### Threat Model

| Threat | Mitigation | Residual Risk |
|--------|------------|---------------|
| Arbitrary code execution | Subprocess isolation via callr | Medium - not full sandbox |
| Resource exhaustion | Timeout + output limits | Low |
| File system access | Code validation | Medium - defense-in-depth |
| Network access | Code validation | Medium - defense-in-depth |
| Package loading | Whitelist | Low |

### Recommendations

1. **Development/Research**: Default configuration is sufficient
2. **Production with untrusted inputs**:
   - Run in containers (Docker)
   - Consider [RAppArmor](https://cran.r-project.org/web/packages/RAppArmor/vignettes/v55i07.pdf) for OS-level sandboxing
   - Implement input validation before module execution

### Configuration Examples

```r
# Conservative (default)
CodeInterpreter$new(
  timeout = 30,
  allowed_packages = c("base", "stats", "utils"),
  max_output_size = 1e6
)

# Data science workload
CodeInterpreter$new(
  timeout = 120,
  allowed_packages = c("base", "stats", "dplyr", "tidyr", "stringr"),
  max_output_size = 1e7
)

# Visualization enabled (use with caution)
CodeInterpreter$new(
  timeout = 60,
  allowed_packages = c("base", "stats", "ggplot2")
  # Note: plot capture requires additional infrastructure
)
```

---

## References

- [DSPy ProgramOfThought Documentation](https://dspy.ai/api/modules/ProgramOfThought/)
- [DSPy CodeAct Documentation](https://dspy.ai/api/modules/CodeAct/)
- [DSPy Program of Thought Tutorial](https://dspy.ai/tutorials/program_of_thought/)
- [callr Package Documentation](https://callr.r-lib.org/reference/r.html)
- [ellmer Tool Calling](https://ellmer.tidyverse.org/articles/tool-calling.html)
- [RAppArmor for Sandboxing](https://cran.r-project.org/web/packages/RAppArmor/vignettes/v55i07.pdf)

---

## Success Criteria

- [ ] CodeInterpreter provides safe, timeout-protected execution
- [ ] ProgramOfThought solves computational problems via code generation
- [ ] CodeAct combines tool calling and code execution seamlessly
- [ ] Full integration with `run()`, `evaluate()`, `optimize_grid()`
- [ ] Comprehensive documentation with examples
- [ ] VCR cassettes for reproducible integration tests
- [ ] R CMD check passes with no errors/warnings
- [ ] Security model documented with clear recommendations

## Labels

`enhancement`, `module-types`, `code-execution`, `phase-5`
