# Convert a Signature to JSON Schema

Converts a dsprrr
[Signature](https://jameshwade.github.io/dsprrr/reference/signature.md)
output contract to a plain R list matching JSON Schema. This is useful
for runtimes that accept JSON schema structured output definitions,
including agent runtimes built on ellmer-compatible contracts.

## Usage

``` r
signature_to_json_schema(signature)
```

## Arguments

- signature:

  A
  [Signature](https://jameshwade.github.io/dsprrr/reference/signature.md)
  object or signature string.

## Value

A named list containing a JSON Schema representation of the signature
output type.

## Examples

``` r
schema <- signature_to_json_schema(
  "question -> answer, confidence: number, citations: array(string)"
)
```
