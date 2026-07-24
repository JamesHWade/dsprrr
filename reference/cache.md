# LLM Response Caching

dsprrr provides automatic caching of LLM responses to speed up
development and reduce costs. The cache uses a two-tier architecture:

1.  **Memory cache**: Fast in-session LRU cache

2.  **Disk cache**: Persistent cache across R sessions

Versioned cache envelopes can contain raw request content, parsed model
outputs, and semantic conversation-turn deltas. Persistent cache
directories must therefore be treated as sensitive storage; see
[`configure_cache()`](https://jameshwade.github.io/dsprrr/reference/configure_cache.md).
