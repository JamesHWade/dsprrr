# Wrap an R function as a module

`module_fn()` turns an ordinary R function into a module, so it can be
used with
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md),
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md),
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
and
[`pipeline()`](https://jameshwade.github.io/dsprrr/reference/pipeline.md)
like any other. Use it for rule-based baselines, for steps that call
other code or services, or for your own logic around several model
calls.

## Usage

``` r
module_fn(signature, forward, chat = NULL, name = NULL, config = list())
```

## Arguments

- signature:

  A signature from
  [`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md),
  or a signature string.

- forward:

  A function called once per input row with the inputs as named
  arguments. If it has a `.llm` argument or `...`, it also receives a
  chat as `.llm`: the one passed to
  [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md), else
  the module's chat, else the scoped or default chat, else `NULL` (no
  chat is auto-detected from API keys). It returns a named list with the
  signature's output fields, or a single value when there is one output
  field. Missing or unknown fields, and values of the wrong type, are an
  error.

- chat:

  An ellmer Chat stored on the module and passed to `forward` as `.llm`.

- name:

  Optional module name, stored in `config$name`.

- config:

  Optional list of settings stored on the module.

## Value

A module (an R6 object of class `FnModule`).

## Details

Function-backed modules record traces but not token counts or costs.
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
and the optimizers refuse them, because there is no prompt to optimize.

## See also

Other program constructors:
[`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md),
[`code_act()`](https://jameshwade.github.io/dsprrr/reference/code_act.md),
[`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md),
[`module()`](https://jameshwade.github.io/dsprrr/reference/module.md),
[`multi_chain_comparison()`](https://jameshwade.github.io/dsprrr/reference/multi_chain_comparison.md),
[`program_of_thought()`](https://jameshwade.github.io/dsprrr/reference/program_of_thought.md),
[`rag_module()`](https://jameshwade.github.io/dsprrr/reference/rag_module.md),
[`react()`](https://jameshwade.github.io/dsprrr/reference/react.md),
[`rlm()`](https://jameshwade.github.io/dsprrr/reference/rlm.md),
[`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md)

## Examples

``` r
truncate <- module_fn(
  "text -> summary",
  function(text) substr(text, 1, 20)
)
run(truncate, text = "A long piece of text that needs a summary")
#> $summary
#> [1] "A long piece of text"
#> 

# Return a named list for several outputs
stats <- module_fn(
  "text -> n_words: int, n_chars: int",
  function(text) {
    list(n_words = length(strsplit(text, "\\s+")[[1]]), n_chars = nchar(text))
  }
)
run(stats, text = "Four words right here")
#> $n_words
#> [1] 4
#> 
#> $n_chars
#> [1] 21
#> 

if (FALSE) { # \dontrun{
# Take `.llm` to call a model yourself
translate <- module_fn(
  "text -> translation",
  function(text, .llm) {
    .llm$chat(paste("Translate to French:", text), echo = "none")
  }
)
run(translate, text = "Good morning", .llm = ellmer::chat_openai(model = "gpt-6-luna"))
} # }
```
