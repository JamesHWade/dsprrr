# Create a Typed Module Signature

The primary function for creating signatures. Accepts either DSPy-style
string notation or explicit arguments. Input and output names must be
valid, unique R field names, and the two namespaces must not overlap.

## Usage

``` r
signature(x = NULL, inputs = NULL, output_type = NULL, instructions = "", ...)
```

## Arguments

- x:

  Either a string in DSPy format ("inputs -\> output") or NULL

- inputs:

  List of input specifications (when using explicit notation)

- output_type:

  An ellmer type object (when using explicit notation)

- instructions:

  Optional instructions for the operation

- ...:

  Additional arguments

## Value

A signature object for use with
[`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)
and related constructors.

## Examples

``` r
# String notation (recommended for simple cases)
sig1 <- signature("text -> sentiment")
sig2 <- signature("context, question -> answer: string")
sig3 <- signature("text -> label: enum('positive', 'negative', 'neutral')")

# Explicit notation (for complex cases)
sig4 <- signature(
  inputs = list(
    input("text", description = "Text to analyze")
  ),
  output_type = ellmer::type_object(
    sentiment = ellmer::type_string(),
    confidence = ellmer::type_number()
  ),
  instructions = "Analyze the text"
)
```
