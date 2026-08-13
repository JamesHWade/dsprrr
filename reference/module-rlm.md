# Recursive Language Model (RLM) Module

An experimental inference-time analyst for inputs whose useful evidence
is too large, irregular, or unpredictable to place in one prompt. RLM
keeps the inputs in an R environment and lets the model iteratively
inspect summaries, run computations, and decide what to examine next.

## Details

Each input is available under `.context`. Generated R code can use
ordinary R plus `peek()`,
[`search()`](https://rdrr.io/r/base/search.html), value-returning
`llm_query()` calls, declared host tools, and `SUBMIT(...)`. RLM
requires a runner whose `policy()` advertises `persistent = TRUE`;
variables therefore remain available across turns within one invocation.
Invalid submitted fields or types become iteration errors that the model
can repair. If no valid submission is produced, the separate `extract`
predictor performs one typed fallback. RLM validates explicit ellmer
string, number, integer, boolean, enum, array, and object outputs.
Opaque `TypeJsonSchema` nodes are rejected at construction because they
cannot participate in this strict repair loop.

The `generate_action` and `extract` predictors are graph-visible through
[`named_modules()`](https://jameshwade.github.io/dsprrr/reference/module-graph.md).
GEPA can tune them; nested MIPROv2 is instruction-only with bootstrapped
demos disabled. BootstrapFewShot and LabeledFewShot reject programs
containing an RLM until predictor-local demonstrations are available.
Model-visible execution evidence defaults to a 10,000-character
head-and-tail view after any runner-level transport limit; that
formatted evidence is retained in the returned trajectory. Trace state
retains hashes and sizes for input objects, not their full values.
Structured metadata separates logical action, recursive, and extraction
counts from verified provider calls. A child-predictor cache hit
contributes zero provider calls and zero current-run usage; totals
remain `NA` whenever every contributing provider turn cannot be
verified.

For generated code, prefer a fresh sandboxed
[`mcp_repl_runner()`](https://jameshwade.github.io/dsprrr/reference/mcp_repl_runner.md)
factory. Its managed transport is intentionally bounded and is best for
compact, JSON-compatible context. Its default policy disables network
access but allows writes within the configured workspace. Declared host
tools execute in the dsprrr host process, outside that guest sandbox.
This backend requires the suggested `mcptools` package and Posit's
external `mcp-repl` executable. For large data frames or richer local R
objects, `r_code_runner(persistent = TRUE)` can stage context once, but
it is trusted-input-only: the child process retains the host user's
file, network, and environment permissions.

Supply exactly one runtime source. A caller-owned `runner` is reused and
never closed by dsprrr. An `interpreter_factory` creates one
invocation-owned runner which dsprrr shuts down on success, error, or
interrupt. Factory-backed RLM supports
[`run_async()`](https://jameshwade.github.io/dsprrr/reference/run_async.md)
and isolated
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
execution; token streaming is unavailable. A direct
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) call
always stages each supplied value as one REPL variable, regardless of
its R length. Use
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
for multiple invocations, with list-columns for data frames, vectors,
lists, matrices, or other rich per-row values.

## Examples

``` r
if (FALSE) { # \dontrun{
analyst <- rlm_module(
  signature = "document, question -> answer",
  interpreter_factory = function() mcp_repl_runner(timeout = 30)
)
compact_doc <- "Section 1: ...\nSection 2: ..."
result <- run(
  analyst,
  document = compact_doc,
  question = "What evidence supports the conclusion?",
  .llm = ellmer::chat_openai()
)
} # }
```
