# Define a module's inputs and outputs

A signature declares what a module takes in, what it returns and what it
should do. Write it as a DSPy-style string such as
`"question -> answer"`, or build it from
[`input()`](https://jameshwade.github.io/dsprrr/reference/input.md)
specifications and an ellmer type.

Field names must be valid, unique R names, and no name can be both an
input and an output.

## Usage

``` r
signature(x = NULL, inputs = NULL, output_type = NULL, instructions = "", ...)
```

## Arguments

- x:

  A signature string, `"inputs -> outputs"` (see below).

- inputs:

  For the explicit form: a list of
  [`input()`](https://jameshwade.github.io/dsprrr/reference/input.md)
  specifications.

- output_type:

  For the explicit form: an ellmer type. Use
  [`ellmer::type_object()`](https://ellmer.tidyverse.org/reference/type_boolean.html)
  for named output fields; a bare type such as
  [`ellmer::type_enum()`](https://ellmer.tidyverse.org/reference/type_boolean.html)
  makes [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md)
  return a bare value.

- instructions:

  What the module should do. The instructions are placed at the top of
  every prompt. For a string signature, the default is generated from
  the field names, for example "Given the fields `question`, produce the
  fields `answer`."

- ...:

  Ignored. Misspelled arguments are dropped without a warning, so check
  the spelling of `instructions`.

## Value

A `Signature` object (S7) with properties `@inputs` (a list of
[`input()`](https://jameshwade.github.io/dsprrr/reference/input.md)
specifications), `@output_type` (an ellmer type) and `@instructions` (a
string). Pass it to
[`module()`](https://jameshwade.github.io/dsprrr/reference/module.md) or
another constructor.

## Details

### String notation

A signature string has comma-separated field names on each side of one
`->`. Any field can take a type after a colon, as in
`"question: str, k: int -> answer: str, confidence: float"`. Fields
without a type are strings. The outputs always form a named list, so a
module built from `"text -> sentiment"` returns `list(sentiment = ...)`.

|  |  |
|----|----|
| Type in the string | Field type |
| `str`, `string` | string |
| `int`, `integer` | integer |
| `float`, `number`, `numeric` | number |
| `bool`, `boolean`, `logical` | logical |
| `enum('a', 'b')`, `Literal['a', 'b']` | one of the listed values |
| `list[T]`, `array(T)`, `T[]` | array of `T` |
| `dict[K, V]` | object with free-form keys (`K` and `V` are not enforced) |
| `Optional[T]` | `T`, marked as not required |

Bounds such as `number[0, 100]` or `string[5, 10]` are accepted but
dropped, leaving a plain number or string. `Union[A, B]` uses `A` and
warns. An unknown type name is an error that suggests the closest known
type. The string form has no syntax for field descriptions: use the
explicit form with
[`input()`](https://jameshwade.github.io/dsprrr/reference/input.md)
descriptions and described ellmer types, such as
`ellmer::type_string("One sentence")`.

## See also

Other signatures:
[`has_reasoning()`](https://jameshwade.github.io/dsprrr/reference/has_reasoning.md),
[`input()`](https://jameshwade.github.io/dsprrr/reference/input.md),
[`with_instructions()`](https://jameshwade.github.io/dsprrr/reference/with_instructions.md),
[`with_reasoning()`](https://jameshwade.github.io/dsprrr/reference/with_reasoning.md),
[`without_reasoning()`](https://jameshwade.github.io/dsprrr/reference/without_reasoning.md)

## Examples

``` r
signature("text -> sentiment")
#> 
#> ── Signature ──
#> 
#> ── Inputs 
#> • text: "string" - Input: text
#> 
#> ── Output 
#> Type: "object(sentiment: string)"
#> 
#> ── Instructions 
#> Given the fields `text`, produce the fields `sentiment`.
signature(
  "text -> label: enum('positive', 'negative', 'neutral')",
  instructions = "Classify the sentiment of a product review."
)
#> 
#> ── Signature ──
#> 
#> ── Inputs 
#> • text: "string" - Input: text
#> 
#> ── Output 
#> Type: "object(label: enum(positive, negative, neutral))"
#> 
#> ── Instructions 
#> Classify the sentiment of a product review.
signature("question: str, k: int -> answers: list[str], confidence: float")
#> 
#> ── Signature ──
#> 
#> ── Inputs 
#> • question: "string" - Input: question
#> • k: "integer" - Input: k
#> 
#> ── Output 
#> Type: "object(answers: array(string), confidence: number)"
#> 
#> ── Instructions 
#> Given the fields `question`, `k`, produce the fields `answers`, `confidence`.

# Explicit form, with descriptions
signature(
  inputs = list(input("review", description = "A customer review")),
  output_type = ellmer::type_object(
    sentiment = ellmer::type_enum(c("positive", "negative")),
    confidence = ellmer::type_number("Between 0 and 1")
  ),
  instructions = "Classify the review."
)
#> 
#> ── Signature ──
#> 
#> ── Inputs 
#> • review: "string" - A customer review
#> 
#> ── Output 
#> Type: "object(sentiment: enum(positive, negative), confidence: number)"
#> 
#> ── Instructions 
#> Classify the review.
```
