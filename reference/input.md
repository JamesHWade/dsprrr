# Create an input specification for a Signature

Create an input specification using an ellmer type or a canonical type
label.

## Usage

``` r
input(name, type = NULL, description = NULL, ...)
```

## Arguments

- name:

  Character string naming the input

- type:

  An ellmer type object, one of `"string"`, `"number"`, `"integer"`,
  `"boolean"`, `"array"`, or `"object"`, or `NULL` to use a string type.

- description:

  Optional description of the input. When `type` is a canonical label or
  `NULL`, this description is passed to the ellmer type.

- ...:

  Additional metadata for the input

## Value

A list with class "dsprrr_input" containing the input specification

## Examples

``` r
# Using ellmer types (recommended for consistency with outputs)
input("text", ellmer::type_string())
#> $name
#> [1] "text"
#> 
#> $type
#> <ellmer::TypeBasic>
#>  @ description: NULL
#>  @ required   : logi TRUE
#>  @ type       : chr "string"
#> 
#> $description
#> NULL
#> 
#> $.type_explicit
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "dsprrr_input"
input("age", ellmer::type_number())
#> $name
#> [1] "age"
#> 
#> $type
#> <ellmer::TypeBasic>
#>  @ description: NULL
#>  @ required   : logi TRUE
#>  @ type       : chr "number"
#> 
#> $description
#> NULL
#> 
#> $.type_explicit
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "dsprrr_input"
input("active", ellmer::type_boolean())
#> $name
#> [1] "active"
#> 
#> $type
#> <ellmer::TypeBasic>
#>  @ description: NULL
#>  @ required   : logi TRUE
#>  @ type       : chr "boolean"
#> 
#> $description
#> NULL
#> 
#> $.type_explicit
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "dsprrr_input"

# Using canonical labels
input("text", "string")
#> $name
#> [1] "text"
#> 
#> $type
#> <ellmer::TypeBasic>
#>  @ description: NULL
#>  @ required   : logi TRUE
#>  @ type       : chr "string"
#> 
#> $description
#> NULL
#> 
#> $.type_explicit
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "dsprrr_input"
input("count", "integer")
#> $name
#> [1] "count"
#> 
#> $type
#> <ellmer::TypeBasic>
#>  @ description: NULL
#>  @ required   : logi TRUE
#>  @ type       : chr "integer"
#> 
#> $description
#> NULL
#> 
#> $.type_explicit
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "dsprrr_input"
input("score", "number")
#> $name
#> [1] "score"
#> 
#> $type
#> <ellmer::TypeBasic>
#>  @ description: NULL
#>  @ required   : logi TRUE
#>  @ type       : chr "number"
#> 
#> $description
#> NULL
#> 
#> $.type_explicit
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "dsprrr_input"

# Type optional (defaults to string)
input("name")
#> $name
#> [1] "name"
#> 
#> $type
#> <ellmer::TypeBasic>
#>  @ description: NULL
#>  @ required   : logi TRUE
#>  @ type       : chr "string"
#> 
#> $description
#> NULL
#> 
#> $.type_explicit
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "dsprrr_input"
input("name", description = "User's name")
#> $name
#> [1] "name"
#> 
#> $type
#> <ellmer::TypeBasic>
#>  @ description: chr "User's name"
#>  @ required   : logi TRUE
#>  @ type       : chr "string"
#> 
#> $description
#> [1] "User's name"
#> 
#> $.type_explicit
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "dsprrr_input"

# With ellmer types for structured data
input("tags", ellmer::type_array(ellmer::type_string()))
#> $name
#> [1] "tags"
#> 
#> $type
#> <ellmer::TypeArray>
#>  @ description: NULL
#>  @ required   : logi TRUE
#>  @ items      : <ellmer::TypeBasic>
#>  .. @ description: NULL
#>  .. @ required   : logi TRUE
#>  .. @ type       : chr "string"
#> 
#> $description
#> NULL
#> 
#> $.type_explicit
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "dsprrr_input"
input("status", ellmer::type_enum(c("pending", "active", "done")))
#> $name
#> [1] "status"
#> 
#> $type
#> <ellmer::TypeEnum>
#>  @ description: NULL
#>  @ required   : logi TRUE
#>  @ values     : chr [1:3] "pending" "active" "done"
#> 
#> $description
#> NULL
#> 
#> $.type_explicit
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "dsprrr_input"
```
