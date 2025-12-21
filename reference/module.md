# Create an LLM Module

The primary function for creating executable LLM modules. Currently
supports "predict" type modules with planned support for additional
types.

## Usage

``` r
module(
  signature,
  type = "predict",
  template = "",
  demos = list(),
  config = list(),
  ...
)
```

## Arguments

- signature:

  A Signature object defining the module's interface

- type:

  Character string specifying the module type (currently only "predict")

- template:

  Optional glue template for prompt generation

- demos:

  Optional list of demonstration examples

- config:

  Optional configuration list

- ...:

  Additional arguments for future module types

## Value

A module object (R6) that can be executed with [`run()`](run.md)

## Examples

``` r
# Create a simple prediction module
classifier <- signature("text -> sentiment") |>
  module(type = "predict", template = "Analyze: {text}")

# With demonstrations
qa <- signature("context, question -> answer") |>
  module(
    type = "predict",
    demos = list(
      list(
        inputs = list(context = "...", question = "..."),
        output = "..."
      )
    )
  )

if (FALSE) { # \dontrun{
# Execute the module (requires an llm object)
llm <- ellmer::chat_openai()
result <- classifier |> run(text = "Great package!", .llm = llm)
} # }
```
