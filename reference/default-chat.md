# Default Chat Configuration

Functions for managing the default ellmer Chat object used by dsprrr.
When no Chat is explicitly provided to a module or
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) call,
these functions determine which Chat to use.

## Details

The default Chat is resolved in this order:

1.  Explicit `options(dsprrr.default_chat = chat_object)`

2.  Auto-detection from environment variables:

    - `OPENAI_API_KEY` →
      [`ellmer::chat_openai()`](https://ellmer.tidyverse.org/reference/chat_openai.html)

    - `ANTHROPIC_API_KEY` →
      [`ellmer::chat_claude()`](https://ellmer.tidyverse.org/reference/chat_anthropic.html)

3.  Error with helpful setup instructions
