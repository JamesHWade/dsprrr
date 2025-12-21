# Create typed input helpers for common cases

Create typed input helpers for common cases

## Usage

``` r
input_string(name, description = NULL, ...)

input_number(name, description = NULL, ...)

input_boolean(name, description = NULL, ...)

input_integer(name, description = NULL, ...)

input_enum(name, values, description = NULL, ...)

input_array(name, item_type = ellmer::type_string(), description = NULL, ...)

input_object(name, ..., description = NULL)
```

## Arguments

- name:

  Name of the input field

- description:

  Optional description of the input

- ...:

  Additional arguments passed to the type constructor

- values:

  For input_enum, the allowed values

- item_type:

  For input_array, the type of array items
