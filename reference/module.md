# Create an LLM Module

The primary function for creating executable LLM modules. Supports
"predict" for standard structured prediction, "react" for ReAct-style
tool-using modules, "chain_of_thought" for step-by-step reasoning,
"multichain" for multi-chain comparison, and "program_of_thought" for
code execution modules.

## Usage

``` r
module(
  signature,
  type = "predict",
  tools = NULL,
  max_iterations = 10L,
  M = 3L,
  temperature = 0.7,
  runner = NULL,
  max_iters = 3L,
  extract_answer = TRUE,
  template = "",
  demos = list(),
  config = list(),
  chat = NULL,
  ...
)
```

## Arguments

- signature:

  A Signature object defining the module's interface

- type:

  Character string specifying the module type:

  - `"predict"` (default): Standard prediction module

  - `"react"`: ReAct-style module with tool support

  - `"chain_of_thought"`: Adds step-by-step reasoning to the signature

  - `"multichain"`: MultiChainComparison module for ensemble reasoning

  - `"program_of_thought"`: Code execution module (requires runner)

  - `"codeact"`: Hybrid agent with tools + code execution (requires
    runner)

  - `"rlm"`: Recursive Language Model for REPL-based context exploration
    (requires runner)

- tools:

  Optional tools configuration:

  - for `type = "react"` or `type = "codeact"`: list of ellmer ToolDef
    objects.

  - for `type = "rlm"`: named list of R functions injected into the
    REPL. If provided with `type = "predict"`, automatically upgrades to
    react.

- max_iterations:

  Maximum iterations for ReAct, CodeAct, or RLM modules created through
  this generic factory (default: 10). For CodeAct it also caps tool
  calls within one invocation; exceeding that inner budget errors.

- M:

  Number of reasoning chains for multichain (default: 3)

- temperature:

  Temperature for multichain diversity (default: 0.7)

- runner:

  Code runner implementing `execute()` and `policy()` for code execution
  types. Create the built-in backend with
  [`r_code_runner()`](https://jameshwade.github.io/dsprrr/reference/r_code_runner.md).

- max_iters:

  Maximum code repair iterations for program_of_thought (default: 3), or
  the DSPy 3.3-compatible alias for RLM's `max_iterations`. For RLM,
  supply only one spelling.

- extract_answer:

  Logical. For program_of_thought, whether to use LLM to extract final
  answer from execution result (default: TRUE)

- template:

  Optional glue template for prompt generation

- demos:

  Optional list of demonstration examples

- config:

  Optional configuration list

- chat:

  Optional ellmer Chat object for LLM operations. If provided, the
  module will use this Chat for all predictions unless overridden with
  `.llm`.

- ...:

  Additional arguments for future module types

## Value

A module object (R6) that can be executed with
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md)

## Examples

``` r
# Create a simple prediction module
classifier <- signature("text -> sentiment") |>
  module(type = "predict", template = "Analyze: {text}")

# With demonstrations
qa <- signature("context, question -> answer") |>
  module(
    type = "predict",
    demos = list(
      list(
        inputs = list(context = "...", question = "..."),
        output = "..."
      )
    )
  )

# Create a multichain comparison module
mcc <- signature("question -> answer") |>
  module(type = "multichain", M = 5, temperature = 0.8)

if (FALSE) { # \dontrun{
# Execute the module (requires an llm object)
llm <- ellmer::chat_openai()
result <- classifier |> run(text = "Great package!", .llm = llm)

# Or create module with Chat attached
classifier <- signature("text -> sentiment") |>
  module(type = "predict", chat = chat_openai())
result <- classifier |> run(text = "Great package!")  # No .llm needed

# Create a ReAct module with tools
search_tool <- ellmer::tool(
  search_fn,
  description = "Search for information",
  arguments = list(query = ellmer::type_string())
)
agent <- signature("question -> answer") |>
  module(type = "react", tools = list(search_tool), chat = chat_openai())
} # }
```
