# Create an R Code Runner

Factory function to create a trusted-input-only RCodeRunner instance.

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

  Numeric. Maximum execution time in seconds. Default 30.

- max_output_chars:

  Integer. Maximum characters for stdout/stderr. Output exceeding this
  limit is truncated. Default 100000 (100KB).

- allowed_packages:

  Character vector. Packages that may be loaded. This is
  defense-in-depth only; not a security boundary.

  Default: c("base", "stats", "utils", "methods").

- prelude:

  Character vector. R code to run before user code. Useful for setting
  options or loading common utilities.

- persistent:

  Logical. Whether to reuse one callr session and execution environment
  across calls to `execute()`. This preserves variables and supports
  staging a base context with `prepare_context()`. Default `FALSE`
  preserves one fresh subprocess per execution.

## Value

An RCodeRunner R6 object

## Examples

``` r
if (FALSE) { # \dontrun{
runner <- r_code_runner(timeout = 10)
result <- runner$execute("sqrt(16)")
result$result
# [1] 4
} # }
```
