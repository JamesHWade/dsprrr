# Convert module demos to a tibble

Converts the demos list from a compiled module into a tidy tibble
format. Handles both flat outputs (single values) and nested outputs
(named lists).

## Usage

``` r
module_demos_as_tibble(module)
```

## Arguments

- module:

  A Module object (typically a PredictModule with demos)

## Value

A tibble with input columns and output column(s). For nested outputs,
output fields are flattened into separate columns.

## Examples

``` r
if (FALSE) { # \dontrun{
# After compiling a module with LabeledFewShot
compiled <- compile(LabeledFewShot(k = 3), module, trainset)

# View demos as tibble
module_demos_as_tibble(compiled)

# Or use the active binding
compiled$demo_table
} # }
```
