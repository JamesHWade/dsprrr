# Wrap a Module with Assertions

Factory function to create an assertion wrapper around any module.
Validates outputs against assertions and retries with backtracking when
hard assertions fail.

## Usage

``` r
with_assertions(
  module,
  assertions,
  max_retries = 3L,
  on_failure = c("error", "warn"),
  feedback_template = NULL,
  ...
)
```

## Arguments

- module:

  A Module object to wrap

- assertions:

  List of Assertion objects (from
  [`assert_output()`](https://jameshwade.github.io/dsprrr/reference/assertions.md)
  or
  [`suggest_output()`](https://jameshwade.github.io/dsprrr/reference/assertions.md))
  or an AssertionSet

- max_retries:

  Maximum number of retry attempts (default 3)

- on_failure:

  What to do when max retries exceeded: "error" (default) or "warn"
  (return best attempt with warning)

- feedback_template:

  Template for feedback injection on retry. Uses glue syntax. Available
  variables:

  - `{failures}`: Bulleted list of hard assertion failure messages

- ...:

  Additional arguments passed to the module constructor

## Value

An AssertModule object

## Details

### Assertion Types

- **[`assert_output()`](https://jameshwade.github.io/dsprrr/reference/assertions.md)**:
  Hard assertion. Must pass or execution retries. Use for critical
  constraints like length limits, required patterns, etc.

- **[`suggest_output()`](https://jameshwade.github.io/dsprrr/reference/assertions.md)**:
  Soft suggestion. Logs warning but doesn't retry. Use for style
  preferences, optional improvements, etc.

### Backtracking Behavior

When hard assertions fail:

1.  Feedback is generated from the failure messages

2.  The module is re-run with feedback injected as `assertion_feedback`

3.  This continues until assertions pass or max_retries is exceeded

4.  If max_retries exceeded, behavior depends on `on_failure` parameter

### Performance Considerations

Each retry makes a new LLM call. Use assertions judiciously and
consider:

- Starting with max_retries = 2-3 for most use cases

- Using "warn" for non-critical assertions to avoid blocking

- Combining with caching to reduce costs during development

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a QA module
qa <- module(signature("question -> answer"))

# Wrap with assertions
validated <- with_assertions(
  qa,
  assertions = list(
    assert_output(~ nchar(.x$answer) <= 100, "Answer must be 100 chars or less"),
    assert_output(~ nchar(.x$answer) >= 10, "Answer must be at least 10 chars"),
    suggest_output(~ grepl("^[A-Z]", .x$answer), "Should start with capital")
  ),
  max_retries = 3
)

# Run - will retry if assertions fail
result <- run(validated, question = "What is the capital of France?", .llm = llm)

# Check attempt history
validated$get_attempts()
} # }
```
