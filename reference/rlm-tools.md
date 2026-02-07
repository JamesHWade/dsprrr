# RLM Tools - Prelude Generator

Generates R code that defines RLM tools in the execution environment.
This code is run before user-generated code via RCodeRunner.

## Details

The prelude defines these functions in the execution environment:

- `SUBMIT(...)`: Terminate and return final output values

- `peek(var, start, end)`: View a slice of a variable

- `search(var, pattern)`: Regex search in variable

- `llm_query(query, context_slice)`: Request a recursive LLM call
  (returns marker for interception)

- `llm_query_batched(queries, slices)`: Request batched LLM calls
  (returns marker for interception)

- `rlm_query()` / `rlm_query_batch()`: Backward-compatible aliases

The recursive-query helper functions return special marker objects that
the main RLM process intercepts and handles. The actual LLM calls happen
in the parent R process, not in the sandboxed code execution
environment.
