# Test whether a model name is a reasoning model

`is_reasoning_model()` guesses from its name whether a model is a
reasoning model: OpenAI's o-series (`o1`, `o3`, `o4-mini`, ...), the
GPT-5 and GPT-6 families (such as `gpt-6-luna`), and any name containing
"reasoning". Reasoning models are tuned with `reasoning_effort` rather
than `temperature` or `top_p`; gpt-6-luna, for example, accepts
`temperature` and `top_p` only with `reasoning_effort = "none"`.

## Usage

``` r
is_reasoning_model(model_name)
```

## Arguments

- model_name:

  A model name, such as `"gpt-6-luna"` or `"o3"`.

## Value

`TRUE` or `FALSE`. `NULL`, `NA` and empty names give `FALSE`.

## Details

[`module_parameters()`](https://jameshwade.github.io/dsprrr/reference/module_parameters.md)
uses this check to decide which parameters to offer for tuning. dsprrr
does not change the parameters of calls made with
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) for
reasoning models.

## See also

Other configuration:
[`cache_stats()`](https://jameshwade.github.io/dsprrr/reference/cache_stats.md),
[`clear_cache()`](https://jameshwade.github.io/dsprrr/reference/clear_cache.md),
[`configure_cache()`](https://jameshwade.github.io/dsprrr/reference/configure_cache.md),
[`dsp_configure()`](https://jameshwade.github.io/dsprrr/reference/dsp_configure.md),
[`dsprrr_sitrep()`](https://jameshwade.github.io/dsprrr/reference/dsprrr_sitrep.md),
[`get_default_chat()`](https://jameshwade.github.io/dsprrr/reference/get_default_chat.md),
[`with_lm()`](https://jameshwade.github.io/dsprrr/reference/with_lm.md)

## Examples

``` r
is_reasoning_model("gpt-6-luna")
#> [1] TRUE
is_reasoning_model("o4-mini")
#> [1] TRUE
is_reasoning_model("gpt-4o")
#> [1] FALSE
```
