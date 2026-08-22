# Configure dsprrr Default Settings

Configure the default LLM provider and settings for dsprrr. Similar to
DSPy's `dspy.configure(lm=lm)`, this sets up a default Chat that will be
used by modules when no explicit Chat is provided.

## Usage

``` r
dsp_configure(
  provider = NULL,
  model = NULL,
  api_key = NULL,
  temperature = NULL,
  ...
)
```

## Arguments

- provider:

  Character string specifying the provider. One of: `"openai"`,
  `"anthropic"`, `"google"`. If `NULL` (default), auto-detects from
  environment variables.

- model:

  Character string specifying the model name. If `NULL`, uses the
  provider's default model.

- api_key:

  Character string with the API key. If `NULL`, reads from the
  appropriate environment variable.

- temperature:

  Numeric value for temperature (0-2). Default is `NULL` (use provider
  default).

- ...:

  Additional arguments passed to the ellmer chat constructor.

## Value

Invisibly returns the configured Chat object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Configure with auto-detection (uses env vars)
dsp_configure()

# Configure with specific provider and model
dsp_configure(provider = "openai", model = "gpt-4o-mini")

# Configure with temperature
dsp_configure(provider = "anthropic", model = "claude-3-5-sonnet-latest",
              temperature = 0.7)

# Now run() uses this configuration
run(module(signature("question -> answer")), question = "What is 2+2?")
} # }
```
