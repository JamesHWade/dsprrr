# Get the Last DSP Trace

Returns the trace from the most recent
[`dsp()`](https://jameshwade.github.io/dsprrr/reference/dsp.md) call.
Useful for debugging and understanding what happened.

## Usage

``` r
last_trace()
```

## Value

A list containing:

- `signature`: The Signature object used

- `inputs`: The inputs provided

- `prompt`: The full prompt sent to the LLM

- `output`: The raw output from the LLM

- `timestamp`: When the call was made

## Examples

``` r
if (FALSE) { # \dontrun{
dsp("q -> a", q = "What is 2+2?")
trace <- last_trace()
trace$prompt  # See the prompt that was sent
} # }
```
