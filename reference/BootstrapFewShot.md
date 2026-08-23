# BootstrapFewShot Teleprompter

A teleprompter that bootstraps demonstrations by having a teacher model
generate predictions on training examples and selecting successful ones
as demonstrations. This is DSPy's foundational optimization approach.

The optimizer:

1.  Starts with optional labeled demonstrations from the training set

2.  Uses a teacher model to generate predictions on remaining examples

3.  Evaluates predictions using the provided metric

4.  Selects top-scoring predictions as bootstrapped demonstrations

5.  Optionally runs multiple rounds, updating the teacher with new demos

## Usage

``` r
BootstrapFewShot(
  metric = NULL,
  metric_threshold = NULL,
  max_errors = 5L,
  max_bootstrapped_demos = 4L,
  max_labeled_demos = 16L,
  max_rounds = 1L,
  teacher_settings = NULL,
  seed = NULL,
  log_dir = NULL
)
```

## Arguments

- metric:

  A metric function for evaluating predictions (required).

- metric_threshold:

  Minimum score for a demo to be accepted. If NULL, accepts any
  successful prediction. Default is NULL.

- max_errors:

  Maximum number of errors allowed during optimization. Default is 5.

- max_bootstrapped_demos:

  Maximum number of bootstrapped demonstrations to include. Default is
  4.

- max_labeled_demos:

  Maximum number of labeled demonstrations from the training set.
  Default is 16.

- max_rounds:

  Number of bootstrap rounds to perform. Default is 1.

- teacher_settings:

  List of settings for the teacher model, such as `temperature` or
  `model`. If NULL, defaults to `list(temperature = 0.7)`.

- seed:

  Random seed for reproducibility. Default is NULL.

- log_dir:

  Directory for trial logging. Default is NULL.

## Details

### Joint pipeline compilation

When `program` is a pipeline (built with
[`pipeline()`](https://jameshwade.github.io/dsprrr/reference/pipeline.md)
or `%>>%`), BootstrapFewShot compiles the whole program jointly, like
DSPy: the teacher pipeline runs end-to-end on each training example, the
*final* output is scored with the metric, and when a run passes the
threshold every step's `(inputs, output)` pair from that trace is
harvested as a demonstration for the corresponding step module.
Intermediate steps therefore receive demos even though the training set
only labels the final output. Labeled demos (`max_labeled_demos`) are
applied to the final step only, and only when its input fields exist in
the trainset. Programs containing Flex or an RLM, at the root or nested,
are rejected. Flex constructs its inner predictors per invocation; RLM
root examples do not match its children's `state -> ...` signatures. Use
GEPA for these programs, or instruction-only MIPROv2 for an RLM graph.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a BootstrapFewShot teleprompter
tp <- BootstrapFewShot(
  metric = metric_exact_match(field = "answer"),
  max_bootstrapped_demos = 4L,
  max_labeled_demos = 8L
)

# Compile a module
qa_module <- module(signature("question -> answer"))
trainset <- data.frame(question = "Capital of France?", answer = "Paris")
llm <- ellmer::chat_openai()
compiled <- compile(qa_module, tp, trainset, .llm = llm)
} # }
```
