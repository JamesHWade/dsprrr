# RLM Tools - Prelude Generator

Generates R code that defines RLM tools in the execution environment.
This code is run before user-generated code by the configured code
runner.

## Details

The prelude defines these functions in the execution environment:

- `SUBMIT(...)`: Terminate and return final output values

- `peek(var, start, end)`: View a slice of a variable

- `search(var, pattern)`: Regex search in variable

- `llm_query(query, context_slice)`: Request a recursive LLM call

- `llm_query_batched(queries, slices)`: Request batched LLM calls

Recursive-query helpers suspend execution with a nonce-bound,
schema-checked request. The main RLM process handles the request and
replays the code with an immutable response, so the helpers behave like
ordinary value-returning R functions. The actual LLM calls happen in the
parent R process, not in the sandboxed code execution environment.
