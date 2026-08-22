# Execute Code with a Scoped LM Override

Temporarily sets a default LLM for all dsprrr operations within a code
block. Similar to DSPy's `dspy.context(lm=lm)` context manager. When the
block exits (normally or due to an error), the previous LM is restored.

## Usage

``` r
with_lm(lm, code)
```

## Arguments

- lm:

  An ellmer Chat object to use as the default LLM within the block.

- code:

  An expression to evaluate with the scoped LLM.

## Value

The result of evaluating `code`.

## Details

The scoped LM has higher priority than `options(dsprrr.default_chat)`
and auto-detection, but lower priority than an explicit `.llm`
parameter.

Resolution order (highest to lowest):

1.  Explicit `.llm` parameter

2.  Scoped LM from `with_lm()` /
    [`local_lm()`](https://jameshwade.github.io/dsprrr/reference/local_lm.md)

3.  Module's stored `$chat`

4.  `options(dsprrr.default_chat)`

5.  Auto-detection from environment variables

## Examples

``` r
if (FALSE) { # \dontrun{
# Use Claude for a specific block
claude <- ellmer::chat_claude()
result <- with_lm(claude, {
  run(module(signature("question -> answer")), question = "What is 2+2?")
  run(module(signature("text -> summary")), text = "Long article...")
})

# Nested contexts work correctly
gpt4 <- ellmer::chat_openai(model = "gpt-4o")
with_lm(gpt4, {
  run(mod1, text = "outer uses gpt4")
  with_lm(claude, {
    run(mod2, text = "inner uses claude")
  })
  run(mod3, text = "back to gpt4")
})
} # }
```
