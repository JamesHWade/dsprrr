# How RLM works

A recursive language model (RLM) (Zhang et al. 2025) answers questions
about inputs it never sees in full. dsprrr keeps the inputs in an R
session. The model sees their names, sizes and short previews, writes R
code to inspect them, reads the printed result, and repeats until it
submits typed outputs. This page explains that loop, how to choose where
the generated code runs, and the limits that apply.
[`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md)
is experimental.

## When to use RLM

| Your task | Use |
|----|----|
| Short inputs and a direct answer | [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md) |
| Reasoning over context you have already selected | [`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md) |
| A calculation the model writes as R code | [`program_of_thought()`](https://jameshwade.github.io/dsprrr/reference/program_of_thought.md) |
| Calling tools or acting on outside systems | [`react()`](https://jameshwade.github.io/dsprrr/reference/react.md) or [`code_act()`](https://jameshwade.github.io/dsprrr/reference/code_act.md) |
| Answering from a prepared retrieval index | [`rag_module()`](https://jameshwade.github.io/dsprrr/reference/rag_module.md) |
| Exploring large or irregular R objects when you don’t know where the answer is | [`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md) |
| Searching for the best program structure over labeled examples | [`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md) |

Every RLM step costs one model call and one code execution. If an R
function, a query or an ordinary module already describes the path to
the answer, use that instead.

## Choose where the code runs

RLM executes code that a model wrote, so pick the runner first. It
decides what that code can touch and how large the inputs can be.

|  | [`mcp_repl_runner()`](https://jameshwade.github.io/dsprrr/reference/mcp_repl_runner.md) | `r_code_runner(persistent = TRUE)` |
|----|----|----|
| Isolation | Operating-system sandbox from Posit’s [mcp-repl](https://github.com/posit-dev/mcp-repl): network off, writes only to the workspace and temporary files | A separate R process with your user’s file, network and environment access |
| Use it for | Inputs you do not control, or code that must not reach your files and network | Inputs and code you trust |
| Inputs | Serialized and sent with every step, so they must be small (see [limits](#limits)) | Any object callr can serialize, staged once per call |
| Requires | The mcptools package and the `mcp-repl` executable | The callr package |

Install the sandbox with:

``` sh
Rscript -e 'install.packages("mcptools")'
uv tool install posit-mcp-repl   # or: pipx install posit-mcp-repl
```

Pass the runner as a zero-argument `interpreter_factory`. dsprrr then
creates a fresh runner for each call and shuts it down afterwards, even
when the call fails. Creating the module does not start a runner:

``` r

library(dsprrr)

# Sandboxed: a short policy text you did not write
reviewer <- rlm_module(
  "policy, question -> answer",
  interpreter_factory = function() mcp_repl_runner(timeout = 30)
)

# Trusted: large data frames from your own pipeline
analyst <- rlm_module(
  "sessions, changes, question -> answer, evidence",
  interpreter_factory = function() {
    r_code_runner(timeout = 30, persistent = TRUE)
  }
)
analyst
#> 
#> ── RLMModule
#> • Signature: sessions, changes, question -> answer, evidence
#> • Max iterations: 20
#> • Max LLM calls: 50
#> • Runner: fresh runner per invocation
#> • Recursive queries: "outer LM"
#> • Custom tools: 0
```

A runner passed as `runner =` instead belongs to you. dsprrr reuses it
across calls, never shuts it down, and after each call removes only the
variables and inputs it staged; files or options the generated code
changed stay changed. Use such a runner for one call at a time, within
one trust boundary.

For a one-off question,
[`rlm()`](https://jameshwade.github.io/dsprrr/reference/rlm.md) builds
the module and runs it in one call, with a fresh
[`mcp_repl_runner()`](https://jameshwade.github.io/dsprrr/reference/mcp_repl_runner.md)
unless you pass `.runner` or `.interpreter_factory`.

## One call, step by step

1.  dsprrr loads each input into the runner as `.context$<name>`. For
    `analyst` these are `.context$sessions`, `.context$changes` and
    `.context$question`.
2.  The first prompt lists each input’s class, size, declared type and a
    preview of at most 1,000 characters (for a data frame, its
    [`str()`](https://rdrr.io/r/utils/str.html)), the output fields, and
    the helper functions below.
3.  The model returns its reasoning and one block of R code, such as
    `str(.context$sessions)`. The runner executes it, and variables
    persist to the next step.
4.  The printed result, cut to `max_output_chars`, is added to the next
    prompt together with every earlier step’s code and output.
5.  The loop ends at the first valid `SUBMIT()`. If `max_iterations`
    (default
    20. runs out first, dsprrr warns and a separate `extract` predictor
        produces the outputs from the steps so far.

| Helper | What it does |
|----|----|
| `peek(x, start, end)` | Characters `start` to `end` of a string, or elements of a vector |
| `search(x, pattern)` | All matches of a regular expression in `x` |
| `llm_query(query, context_slice)` | Asks the sub-model one question about a string and returns the answer |
| `llm_query_batched(queries, slices)` | Asks several independent questions; answers come back in input order |
| `SUBMIT(...)` | Ends the loop with the output fields |

[Investigate a regression with
RLM](https://jameshwade.github.io/dsprrr/articles/tutorial-rlm-dsprrr.md)
shows a complete trajectory for a module like `analyst`.

## Recursive queries

`llm_query()` reads like a function call, but the model call happens in
your R session, not in the runner:

``` r

notes <- .context$changes$note[.context$changes$release == "2.4.0"]
suspects <- llm_query(
  "Which of these change notes could lower mobile checkout conversion?",
  paste(notes, collapse = "\n")
)
```

The step stops at the call, dsprrr sends the question to the sub-model,
then reruns the step from the top with the answer filled in. Assignments
made in the step are kept once the rerun completes, and tools run once.
Anything with outside effects before the query (writing a file, calling
an API) runs again on every rerun, so keep that part read-only. dsprrr
does not pass model credentials to the runner, but that alone does not
hide them: an
[`r_code_runner()`](https://jameshwade.github.io/dsprrr/reference/r_code_runner.md)
process inherits your environment variables.

`sub_lm` sets the chat for these queries, for example
`sub_lm = ellmer::chat_openai(model = "gpt-6-luna")` in
[`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md).
The default, `NULL`, reuses the chat that runs the main loop. Every
question counts against `max_llm_calls` (default 50), including each one
in a batch, and `max_llm_calls = 0` removes both query helpers. In
`llm_query_batched()`, a provider error for one question comes back as
an `"[ERROR] ..."` string in its position; any other failure ends the
call.

## Typed submission

The signature decides what `SUBMIT()` accepts. For
`"logs -> error_count: int, severity: enum('low', 'high'), evidence: list[str]"`,
generated code submits named values:

``` r

SUBMIT(
  error_count = 7L,
  severity = "high",
  evidence = c("AUTH-401 rate tripled at 14:02", "Token refresh retries fell to 0")
)
```

A missing required field, an unknown field, or a value of the wrong type
does not end the loop. The error is added to the trajectory and the
model can fix its submission on the next step. Optional fields may be
left out. Outputs must use ellmer’s string, number, integer, boolean,
enum, array and object types; a
[`type_from_schema()`](https://ellmer.tidyverse.org/reference/type_boolean.html)
output is rejected when the module is created because RLM cannot check
it.

## Read the result

Ask for the structured result to keep the trajectory with the answer:

``` r

result <- run(
  analyst,
  sessions = sessions,
  changes = changes,
  question = "Which cohort's conversion fell after release 2.4.0, and why?",
  .llm = ellmer::chat_openai(model = "gpt-6-luna"),
  .return_format = "structured"
)

result$output$answer
result$metadata$output_source # "submit", or "fallback" if no step submitted
result$metadata$repl_history # one entry per step
```

Each `repl_history` entry holds the step’s reasoning, its code, the
output the model saw, and whether it succeeded or submitted. The
metadata also counts steps (`iterations`), recursive queries
(`llm_calls`), provider calls, tokens and cost; totals are `NA` when a
provider cannot report every call. Treat `output_source == "fallback"`
as a warning that the step budget ran out or the instructions need work.

[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) stages
each input as one object, whatever its length. For many investigations,
use
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
with one row per investigation and list-columns for data frames; with a
factory, each row gets its own runner.

Errors in generated code, such as a missing column, are shown to the
model so it can try again. Failures of the runner itself (it cannot
start, its connection breaks) end the call with an error.

## Limits

| Limit | Size | Applies to | When exceeded |
|----|----|----|----|
| Output the model sees per step | `max_output_chars`, default 10,000 characters | RLM, any runner | The middle is cut; the model sees the head, the tail and how much was omitted |
| Preview of each input in the first prompt | 1,000 characters | RLM, any runner | Cut the same way |
| One request to mcp-repl: the step’s code plus its serialized `.context` | 7,000 bytes, gzip-compressed when larger | RLM steps, executable Flex steps and harness sandbox actions in [`mcp_repl_runner()`](https://jameshwade.github.io/dsprrr/reference/mcp_repl_runner.md) | Rejected before it runs; in RLM the model sees the error |
| One message back from the runner: an RLM `SUBMIT()`, `llm_query()` or tool request, or a Flex predictor or tool call | 3,000 bytes, encoded | RLM and executable Flex in [`mcp_repl_runner()`](https://jameshwade.github.io/dsprrr/reference/mcp_repl_runner.md) | The step fails; in RLM the model sees the error and can retry with smaller values |
| Tool calls in one step | 1,000 | RLM, any runner | The call ends with an error |
| Sandbox code from an agent | 12,000 characters | [`AutoResearch()`](https://jameshwade.github.io/dsprrr/reference/AutoResearch.md) and [`MetaHarness()`](https://jameshwade.github.io/dsprrr/reference/MetaHarness.md) | The action is rejected without running |
| Sandbox result shown to the agent | 4,000 characters | [`AutoResearch()`](https://jameshwade.github.io/dsprrr/reference/AutoResearch.md) and [`MetaHarness()`](https://jameshwade.github.io/dsprrr/reference/MetaHarness.md) | Truncated and marked `[truncated]` |

The request limit is the one you will meet. In RLM, `.context` holds
every input, and RLM’s own helper code already takes about 4,700 of the
7,000 bytes after compression (about 5,200 when `llm_query()` is
enabled). That leaves roughly 2,000 bytes for the compressed inputs: a
few thousand characters of ordinary text or a couple of hundred rows of
a small data frame, and much less for text that barely compresses, such
as IDs or hashes. Inputs are sent again with every step, so an input
that is too large fails every step and the call ends in fallback
extraction. `r_code_runner(persistent = TRUE)` has neither the request
nor the frame limit; use it for larger inputs you trust.

mcp-repl also moves very large printed output into a file preview. RLM
treats that as a failed step rather than reading a partial result, so
generated code should print summaries, not whole objects.

## Privacy and caching

The runner boundary does not keep data away from the model provider. The
first prompt carries each input’s preview: a string of up to 1,000
characters in full, and the start and end of a longer one. Anything
printed, passed to `llm_query()`, or submitted also reaches the
provider. Tools given through `tools =` run in your R session with your
permissions, outside any sandbox.

The main-loop and fallback calls use dsprrr’s response cache, which can
include a disk cache. Pass `.cache = FALSE` to
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) for
sensitive work. Recursive queries and code execution are never cached.

## Relationship to DSPy

dsprrr follows the contract of DSPy’s `dspy.RLM`, with R instead of
Python in the interpreter and ellmer chats for the models. The two
predictors inside an RLM, `generate_action` (writes each step) and
`extract` (the fallback), are ordinary modules that
[`GEPA()`](https://jameshwade.github.io/dsprrr/reference/GEPA.md) can
tune. [dsprrr for DSPy
users](https://jameshwade.github.io/dsprrr/articles/dspy-comparison.md)
lists the differences.

For a worked example on a fixed dataset, with an independent check of
the answer, see [Investigate a regression with
RLM](https://jameshwade.github.io/dsprrr/articles/tutorial-rlm-dsprrr.md).

## References

Zhang, Alex L., Tim Kraska, and Omar Khattab. 2025. *Recursive Language
Models*. <https://arxiv.org/abs/2512.24601>.
