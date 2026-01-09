# LLM Response Caching

dsprrr provides automatic caching of LLM responses to speed up
development and reduce costs. The cache uses a two-tier architecture:

1.  **Memory cache**: Fast in-session LRU cache

2.  **Disk cache**: Persistent cache across R sessions
