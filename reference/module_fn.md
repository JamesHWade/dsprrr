# Callable Module

`module_fn()` wraps an ordinary R function in a dsprrr Module. This
gives custom callables the same
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) and
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
surface as built-in modules without requiring users to subclass dsprrr
internals.

## Usage

``` r
module_fn(signature, forward, chat = NULL, name = NULL, config = list())
```

## Arguments

- signature:

  A signature object created by
  [`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md),
  or a signature string.

- forward:

  Function called with named signature inputs. If the function accepts
  `.llm` or `...`, the active chat object is passed as `.llm`. Return a
  named list matching the signature output fields, or a scalar when the
  signature has exactly one output field. `forward` is invoked once per
  row; use
  [`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
  or
  [`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
  to process multi-row datasets.

- chat:

  Optional ellmer Chat object stored on the module.

- name:

  Optional module name stored in `config$name`.

- config:

  Optional configuration metadata.

## Value

An R6 `FnModule` object inheriting from the internal Module base class.

## Examples

``` r
summarizer <- module_fn(
  "text -> summary",
  function(text, ...) list(summary = substr(text, 1, 20))
)
run(summarizer, text = "A long piece of text")
#> $summary
#> [1] "A long piece of text"
#> 
```
