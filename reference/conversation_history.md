# Construct a portable conversation history

Converts native ellmer user and assistant turns into a versioned
envelope containing only allowlisted conversation data. Tool requests
and results are retained as data; they are never executed. System turns
and unsupported or nonportable content fail explicitly.

## Usage

``` r
conversation_history(turns = list())
```

## Arguments

- turns:

  An unnamed list of ellmer UserTurn and AssistantTurn objects. The
  empty list constructs an empty history.

## Value

An object with class dsprrr_history. This constructor creates a
version-1 empty or ReAct history. RLM execution returns version-2
history containing observed invocation exchanges in result metadata.

## Details

Supported content includes text, JSON, thinking with JSON-safe extra
data, inline and remote images, PDFs, tool requests, and tool results.
Runtime tool objects are omitted. A tool result may contain JSON-safe
data or an ordered list of allowlisted content. Condition errors are
retained as their message; runtime condition and call objects are
dropped. Uploaded content and arbitrary R objects are rejected. Tool
request IDs must be unique and every request must have one matching
result before the history is accepted. Histories are limited to 64 data
nesting levels, 12 MiB of data, 16 MiB of JSON, 10,000 turns, and
100,000 tagged values.

Histories are copied into a tagged data representation. They do not
retain ellmer Chat objects, functions, environments, external pointers,
credentials attached to runtime tools, or usage, cost, or duration
metadata. Conversation text, reasoning, media, and tool data may still
be sensitive; protect saved history files accordingly.

RLM histories instead store visible task previews, inert code and
observed output text, plus complete typed final outputs. They do not
restore variables or interpreter state. ReAct turns and RLM exchanges
cannot be mixed.

Saved JSON is limited to 16 MiB; retained string and media data are
limited to 12 MiB. Histories also have limits on turns, nesting depth,
and values. RLM histories additionally retain at most 100 exchanges and
1,000 observed steps, with a canonical rendered context limit of 128
KiB. Oversized histories fail explicitly rather than silently truncating
evidence.

## Examples

``` r
turns <- list(
  ellmer::UserTurn(list(ellmer::ContentText("What is 2 + 2?"))),
  ellmer::AssistantTurn(list(ellmer::ContentText("4")))
)
history <- conversation_history(turns)
identical(length(history_turns(history)), 2L)
#> [1] TRUE
```
