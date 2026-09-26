# Run R code in a separate process

`r_code_runner()` creates a runner that executes R code in a separate R
process with [callr](https://callr.r-lib.org/), with a timeout and
captured output. Code-writing modules such as
[`program_of_thought()`](https://jameshwade.github.io/dsprrr/reference/program_of_thought.md),
[`code_act()`](https://jameshwade.github.io/dsprrr/reference/code_act.md)
and
[`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md)
use a runner to execute the code the model writes.

The runner is for trusted input only. The child process has the same
file, network, environment and operating-system permissions as your R
session; the package allow-list and pattern checks are not a security
boundary. For untrusted input, use a sandboxed runner such as
[`mcp_repl_runner()`](https://jameshwade.github.io/dsprrr/reference/mcp_repl_runner.md).

## Usage

``` r
r_code_runner(
  timeout = 30,
  max_output_chars = 100000L,
  allowed_packages = c("base", "stats", "utils", "methods"),
  prelude = character(),
  persistent = FALSE
)
```

## Arguments

- timeout:

  Maximum execution time per call, in seconds (default 30).

- max_output_chars:

  Maximum number of characters kept from `stdout` and `stderr`; longer
  output is truncated (default `100000L`).

- allowed_packages:

  Packages the code may load (default `base`, `stats`, `utils` and
  `methods`). This check is a convenience, not a security boundary.

- prelude:

  Character vector of R code run before each call's code, for example to
  set options.

- persistent:

  Whether to reuse one R session, and its variables, across calls to
  `execute()` (default `FALSE`, a fresh process per call).

## Value

An `RCodeRunner` R6 object.

## Details

### Using a runner

`runner$execute(code, context = list())` runs `code` and returns a list
with `success`, `result` (the value of the last expression), `stdout`,
`stderr`, `messages`, `warnings`, `error`, `error_type`, `retryable` and
`duration_ms`. Errors and timeouts are returned in `error` rather than
thrown. Values in `context` are available to the code as `.context`.
`runner$policy()` describes the trust boundary, and `runner$shutdown()`
stops a persistent session.

With `persistent = TRUE`, one R session is reused, so variables survive
between calls. `runner$prepare_context(context)` stages large objects
once for later calls, and `runner$reset()` clears the session.

### Writing your own runner

Code-executing modules accept any object with
`execute(code, context = list())` and `policy()` methods. `policy()`
returns a named list with at least `backend` (character), `trust`
(character) and `sandboxed` (logical). `execute()` returns a named list
with at least `success` and `result`; missing `stdout`, `stderr`,
`messages` and `warnings` become empty strings and a missing
`duration_ms` becomes `NA`. A failed result needs a non-empty `error`
and an `error_type` of `"execution"` (code the model may fix) or
`"interpreter"` (a process or protocol failure that ends the run).

Modules take exactly one of `runner` or `interpreter_factory`. A
`runner` belongs to you: it is reused across calls and never shut down
by dsprrr. An `interpreter_factory` is a function with no arguments that
returns a fresh runner; dsprrr starts it (calling
[`start()`](https://rdrr.io/r/stats/start.html) when the runner has one)
and calls its `shutdown()` method exactly once when the call ends,
whether it succeeds, fails or is interrupted.

## See also

Other code execution:
[`code_act()`](https://jameshwade.github.io/dsprrr/reference/code_act.md),
[`mcp_repl_runner()`](https://jameshwade.github.io/dsprrr/reference/mcp_repl_runner.md),
[`program_of_thought()`](https://jameshwade.github.io/dsprrr/reference/program_of_thought.md),
[`rlm()`](https://jameshwade.github.io/dsprrr/reference/rlm.md),
[`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md)

## Examples

``` r
runner <- r_code_runner(timeout = 10)
runner
#> 
#> ── RCodeRunner 
#> • Timeout: 10 seconds
#> • Max output: 100000 chars
#> • Allowed packages: "base", "stats", "utils", and "methods"
#> ! Trust: "trusted-input-only"; not sandboxed

result <- runner$execute("sqrt(16)")
result$success
#> [1] TRUE
result$result
#> [1] 4

# Pass data in through `.context`
runner$execute(
  "mean(.context$data$mpg)",
  context = list(data = mtcars)
)$result
#> [1] 20.09062

# Errors are returned, not thrown
runner$execute("stop('boom')")$error
#> [1] "boom"

# A persistent runner keeps variables between calls
session <- r_code_runner(persistent = TRUE)
invisible(session$execute("x <- 21"))
session$execute("x * 2")$result
#> [1] 42
session$shutdown()
```
