# KNNFewShot Teleprompter

A teleprompter that uses k-nearest neighbor retrieval to dynamically
select demonstrations at runtime based on similarity to the input query.
Unlike
[LabeledFewShot](https://jameshwade.github.io/dsprrr/reference/LabeledFewShot.md)
which selects static demos at compile time, KNNFewShot selects different
demos for each query based on embedding similarity.

## Usage

``` r
KNNFewShot(
  metric = NULL,
  metric_threshold = NULL,
  max_errors = 5L,
  k = 3L,
  vectorizer = function() NULL,
  input_text = NULL,
  cache_embeddings = TRUE,
  merge_demos = FALSE
)
```

## Arguments

- metric:

  Optional metric stored with the teleprompter. KNNFewShot selects
  demonstrations by embedding similarity rather than candidate
  evaluation.

- metric_threshold:

  Minimum score required to be considered successful. If NULL, uses the
  metric's default threshold.

- max_errors:

  Maximum number of errors allowed during optimization. Default is 5.

- k:

  Number of nearest neighbors to use as demonstrations. Default is 3.

- vectorizer:

  A function that takes a character vector and returns a numeric matrix
  of embeddings (one row per input). Required.

- input_text:

  A function that converts a training example (data frame row) to a
  character string for embedding. Default concatenates all input
  columns.

- cache_embeddings:

  Whether to cache embeddings for the training set. Default is TRUE.

- merge_demos:

  If TRUE, merge KNN-selected demos with any existing demos on the
  module. Default is FALSE (replace).

## Details

KNNFewShot works by:

1.  Pre-computing embeddings for all training examples at compile time

2.  At runtime, embedding each input query

3.  Finding the k most similar training examples using cosine similarity

4.  Using those examples as demonstrations for the current query

This approach is particularly effective when:

- The training set is large and diverse

- Different types of queries benefit from different demonstrations

- Semantic similarity is a good proxy for task relevance

## Vectorizer Function

The `vectorizer` parameter should be a function that takes a character
vector and returns a numeric matrix where each row is an embedding.
Common options:

- `ragnar::embed_openai()` for OpenAI embeddings

- Custom embedding functions using other providers

For testing, you can provide a deterministic vectorizer that returns
consistent embeddings for the same inputs.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a simple vectorizer (in practice, use ragnar::embed_openai or similar)
simple_vectorizer <- function(texts) {
  # Return random embeddings for demonstration
  matrix(runif(length(texts) * 10), nrow = length(texts))
}

# Create the teleprompter
tp <- KNNFewShot(
  k = 3L,
  vectorizer = simple_vectorizer,
  input_text = function(example) example$question
)

# Compile a module
compiled <- compile(my_module, tp, trainset, .llm = llm)

# Now queries will get dynamically selected demos
run(compiled, question = "What is 2+2?", .llm = llm)
} # }
```
