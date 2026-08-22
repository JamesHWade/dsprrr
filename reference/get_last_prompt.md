# Get the Last Prompt

Returns detailed information about the most recent LLM call, including
the prompt sent, response received, and metadata like tokens and cost.
Works with module-based calls.

## Usage

``` r
get_last_prompt()
```

## Value

A `dsprrr_prompt_inspection` object containing:

- `prompt`: The full prompt sent to the LLM

- `response`: The LLM's response

- `model`: The model used

- `tokens_in`: Input tokens used

- `tokens_out`: Output tokens generated

- `cost`: Cost in USD (if available)

- `timestamp`: When the call was made

- `source`: The module class that originated the call

- `program_artifact_id`: Exact executable program identity, when
  available

- `trace_context`: Caller-supplied correlation context

Returns `NULL` if no LLM calls have been made.

## Examples

``` r
if (FALSE) { # \dontrun{
# Make an LLM call
mod <- module(signature("question -> answer"))
run(mod, question = "What is 2+2?", .llm = ellmer::chat_openai())

# Inspect what happened
get_last_prompt()
#> ─── Last Prompt ───────────────────────────────────
#> System: Given the fields `question`, produce the fields `answer`.
#>
#> User: question: What is 2+2?
#>
#> ─── Response ──────────────────────────────────────
#> Assistant: {"answer": "4"}
#>
#> ─── Metadata ──────────────────────────────────────
#> Model: gpt-4o-mini | Tokens: 45 in, 12 out | Cost: $0.0001
} # }
```
