# Pipe Operator for Module Composition

Chains modules together into a pipeline. The output of the left module
flows into the input of the right module. When field names match between
output and input, they are automatically connected.

## Usage

``` r
lhs %>>% rhs
```

## Arguments

- lhs:

  A Module or PipelineModule

- rhs:

  A Module or PipelineModule.

## Value

A PipelineModule combining both modules

## Examples

``` r
if (FALSE) { # \dontrun{
# Simple chaining - automatic field matching
qa_pipeline <- mod_parse %>>% mod_answer %>>% mod_format

# Run the pipeline
result <- run(qa_pipeline, text = "What is 2+2?", .llm = llm)

} # }
```
