# Describe one signature input

`input()` describes one input field for the explicit form of
[`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md):
its name, its type and an optional description.

## Usage

``` r
input(name, type = NULL, description = NULL, ...)
```

## Arguments

- name:

  The field name. Callers pass the value under this name, as in
  `run(mod, review = "...")`.

- type:

  An ellmer type, one of the labels `"string"`, `"number"`, `"integer"`,
  `"boolean"`, `"array"` (an array of strings) or `"object"`, or `NULL`
  for a string.

- description:

  Optional description. Unless the module has its own template, it is
  written above the value in the prompt, as `# description`. With a
  label or `NULL` `type`, it also becomes the ellmer type's description.

- ...:

  Extra fields stored in the specification.

## Value

A list of class `dsprrr_input` with elements `name`, `type` (an ellmer
type) and `description`.

## See also

Other signatures:
[`has_reasoning()`](https://jameshwade.github.io/dsprrr/reference/has_reasoning.md),
[`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md),
[`with_instructions()`](https://jameshwade.github.io/dsprrr/reference/with_instructions.md),
[`with_reasoning()`](https://jameshwade.github.io/dsprrr/reference/with_reasoning.md),
[`without_reasoning()`](https://jameshwade.github.io/dsprrr/reference/without_reasoning.md)

## Examples

``` r
review <- input("review", description = "A customer review")
stars <- input("stars", "integer")
tags <- input("tags", ellmer::type_array(ellmer::type_string()))

# Inputs make up the explicit form of a signature
signature(
  inputs = list(review, stars, tags),
  output_type = ellmer::type_object(summary = ellmer::type_string())
)
#> 
#> ── Signature ──
#> 
#> ── Inputs 
#> • review: "string" - A customer review
#> • stars: "integer"
#> • tags: "array(string)"
#> 
#> ── Output 
#> Type: "object(summary: string)"
```
