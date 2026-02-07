# Recursive Language Model (RLM) Module

A module that transforms context from "input" to "environment", enabling
LLMs to programmatically explore large contexts through a REPL interface
rather than embedding them in prompts.

## Details

Instead of `llm(prompt, context=huge_document)`, RLM stores context as R
variables that the LLM can peek, slice, search, and recursively query.

The execution flow is:

1.  Context is made available as variables in an R execution environment

2.  LLM generates R code to explore and analyze the context

3.  Code is executed in an isolated subprocess via RCodeRunner

4.  Results are fed back to the LLM for the next iteration

5.  Process continues until SUBMIT() is called or max_iterations reached

6.  If max_iterations reached without SUBMIT(), fallback extraction is
    used

Available REPL tools:

- `SUBMIT(...)`: Terminate and return final output values

- `peek(var, start, end)`: View a slice of a variable (default: first
  1000 chars)

- `search(var, pattern)`: Regex search in variable

- `llm_query(query, context_slice)`: Recursive LLM call (requires
  sub_lm)

- `llm_query_batched(queries, slices)`: Batched recursive calls

Security: Code execution requires explicit opt-in via a runner
parameter. The runner provides subprocess isolation but is NOT a
security sandbox. For untrusted inputs, use OS-level sandboxing
(containers, AppArmor).

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a runner (required for code execution)
runner <- r_code_runner(timeout = 30)

# Create an RLM module for exploring large documents
rlm <- rlm_module(
  signature = "document, question -> answer",
  runner = runner
)

# Use it for context exploration
long_doc <- paste(readLines("large_file.txt"), collapse = "\n")
result <- run(rlm, document = long_doc, question = "What are the main themes?", .llm = llm)

# Enable recursive LLM calls for complex reasoning
rlm_recursive <- rlm_module(
  signature = "document -> summary",
  runner = runner,
  sub_lm = ellmer::chat_openai(model = "gpt-4o-mini"),
  max_llm_calls = 10
)
} # }
```
