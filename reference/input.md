# Create an input specification for a Signature

Create an input specification with flexible type notation. Supports
ellmer types, string shortcuts, or S7 classes for backward
compatibility.

## Usage

``` r
input(name, type = NULL, description = NULL, ...)
```

## Arguments

- name:

  Character string naming the input

- type:

  Input type specification. Can be:

  - An ellmer type object (e.g., `type_string()`, `type_number()`)

  - A string shortcut (e.g., "string", "number", "boolean")

  - An S7 class (for backward compatibility)

  - NULL/missing (defaults to string type)

- description:

  Optional description of the input. When type is a string shortcut or
  NULL, this description will be passed to the ellmer type

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
#> $class
#> <S7_base_class>: <character>
#> 
#> $description
#> NULL
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
#> $class
#> <S7_base_class>: <double>
#> 
#> $description
#> NULL
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
#> $class
#> <S7_base_class>: <logical>
#> 
#> $description
#> NULL
#> 
#> attr(,"class")
#> [1] "dsprrr_input"

# Using string shortcuts (simple and readable)
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
#> $class
#> <S7_base_class>: <character>
#> 
#> $description
#> NULL
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
#> $class
#> <S7_base_class>: <integer>
#> 
#> $description
#> NULL
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
#> $class
#> <S7_base_class>: <double>
#> 
#> $description
#> NULL
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
#> $class
#> <S7_base_class>: <character>
#> 
#> $description
#> NULL
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
#> $class
#> <S7_base_class>: <character>
#> 
#> $description
#> [1] "User's name"
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
#> $class
#> <S7_base_class>: <list>
#> 
#> $description
#> NULL
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
#> $class
#> <S7_base_class>: <character>
#> 
#> $description
#> NULL
#> 
#> attr(,"class")
#> [1] "dsprrr_input"
```
