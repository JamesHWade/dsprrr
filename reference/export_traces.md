# Export Module Traces

Export traces from a module as a tidy tibble for analysis and
visualization.

## Usage

``` r
export_traces(module, include_prompts = FALSE, include_outputs = FALSE)
```

## Arguments

- module:

  A DSPrrr module with recorded traces

- include_prompts:

  Logical; whether to include full prompts in the output (default FALSE)

- include_outputs:

  Logical; whether to include full outputs in the output (default FALSE)

## Value

A tibble with one row per trace containing:

- timestamp: When the trace was recorded

- latency_ms: Response time in milliseconds

- input_tokens: Number of input tokens used

- output_tokens: Number of output tokens generated

- total_tokens: Total tokens (input + output)

- cost: Cost in USD (if available from the provider)

- model: The model name/version used

- prompt_length: Character length of the prompt

- program_artifact_id: Exact executable program identity, when available

- trace_context: Caller-supplied correlation context

- prompt: The full prompt text (if include_prompts = TRUE)

- output: The model output (if include_outputs = TRUE)

## Examples

``` r
if (FALSE) { # \dontrun{
# Get basic trace metrics
traces <- export_traces(my_module)

# Include full prompts and outputs for analysis
full_traces <- export_traces(my_module,
                             include_prompts = TRUE,
                             include_outputs = TRUE)

# Visualize token usage over time
library(ggplot2)
ggplot(traces, aes(x = timestamp, y = total_tokens)) +
  geom_line() +
  geom_point()
} # }
```
