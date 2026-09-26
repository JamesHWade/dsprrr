# Choose a module type

``` r

library(dsprrr)
```

Every dsprrr module comes from a constructor and runs with
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md). The
constructors differ in how they reach an answer: one prompt, several
attempts, tools, or generated R code. This page helps you pick one. The
table compares them, and the sections below run one small task through
each pattern so you can see what changes in the code and in the number
of model calls.

| Constructor | What it does | Use it when | Model calls per input |
|----|----|----|----|
| [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md) | One structured prediction | The task fits in one prompt | 1 |
| [`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md) | Adds a `reasoning` output before the answer | Worked steps help the answer | 1 |
| [`best_of_n()`](https://jameshwade.github.io/dsprrr/reference/best_of_n.md) | Reruns a module and keeps the attempt with the highest reward | You can score an output in R | 1 to `N` |
| [`refine()`](https://jameshwade.github.io/dsprrr/reference/refine.md) | Like [`best_of_n()`](https://jameshwade.github.io/dsprrr/reference/best_of_n.md), and each retry gets feedback text | A retry can be told what to fix | 1 to `N` |
| [`with_assertions()`](https://jameshwade.github.io/dsprrr/reference/with_assertions.md) | Checks outputs against rules and retries with the failed rules’ messages | Outputs must meet hard constraints | 1 to `max_retries` + 1 |
| [`multi_chain_comparison()`](https://jameshwade.github.io/dsprrr/reference/multi_chain_comparison.md) | Runs `M` reasoning chains, then one call compares them | Samples disagree and you want one reconciled answer | `M` + 1 |
| [`ensemble()`](https://jameshwade.github.io/dsprrr/reference/ensemble.md) | Runs several modules and combines their outputs | You have variants to vote between | Sum over members |
| [`react()`](https://jameshwade.github.io/dsprrr/reference/react.md) | Lets the model call your R functions as tools | The answer needs data or actions the model lacks | 2, plus 1 per tool round |
| [`program_of_thought()`](https://jameshwade.github.io/dsprrr/reference/program_of_thought.md) | The model writes R code and R runs it | The answer needs exact computation | 1, plus 1 per repair |
| [`code_act()`](https://jameshwade.github.io/dsprrr/reference/code_act.md) | An agent with your tools and an R execution tool | A task needs both tools and computation | 1, plus 1 per tool round |
| [`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md) | The model explores large R objects one step at a time (experimental) | The input is too big or irregular for one prompt | 1 per step, plus recursive queries |
| [`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md) | A program whose structure an optimizer can rewrite (experimental) | The best program shape is the open question | Depends on the program |
| [`module_fn()`](https://jameshwade.github.io/dsprrr/reference/module_fn.md) | Wraps an R function as a module | You need custom logic around model calls | Whatever the function makes |
| [`pipeline()`](https://jameshwade.github.io/dsprrr/reference/pipeline.md) | Runs modules in sequence | A task splits into steps | Sum over steps |

With `.return_format = "structured"`, most modules report what a call
actually used in `metadata$provider_calls`, `metadata$total_tokens` and
`metadata$cost`. The examples below answer one question whose correct
answer is 205:

``` r

llm <- ellmer::chat_openai(model = "gpt-6-luna")
question <- "A train leaves at 9:40 and arrives at 13:05. How many minutes is the trip?"
```

## One prompt: module() and chain_of_thought()

[`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)
makes one structured call.
[`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md)
makes the same call with an extra `reasoning` output placed before the
answer, so the model writes out its steps first:

``` r

basic <- module(signature("question -> answer"))
run(basic, question = question, .llm = llm)

cot <- chain_of_thought("question -> answer")
result <- run(cot, question = question, .llm = llm)
result$reasoning
result$answer
```

The reasoning costs output tokens and is not guaranteed to reflect how
the model reached its answer, so judge the answer with a metric. Under
the hood,
[`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md)
builds its signature with
[`with_reasoning()`](https://jameshwade.github.io/dsprrr/reference/with_reasoning.md),
which describes the new field with `prefix` followed by “produce the”
and the output names. Write the prefix as the start of a sentence that
ends in “to”:

``` r

sig <- with_reasoning(
  "question -> answer",
  prefix = "Convert both times to minutes after midnight in order to"
)
sig@output_type@properties$reasoning@description
#> [1] "Reasoning: Convert both times to minutes after midnight in order to produce the answer."
```

Pass the same `prefix` to
[`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md).
[`without_reasoning()`](https://jameshwade.github.io/dsprrr/reference/without_reasoning.md)
removes the field again, which is handy for comparing both versions on
the same data.

## Retry until an output passes

Suppose the answer must be a bare number such as `205`, not
`205 minutes` or `3 h 25 min`. Three wrappers can enforce that, and they
differ in what the next attempt learns from a failure:

|  | [`best_of_n()`](https://jameshwade.github.io/dsprrr/reference/best_of_n.md) | [`refine()`](https://jameshwade.github.io/dsprrr/reference/refine.md) | [`with_assertions()`](https://jameshwade.github.io/dsprrr/reference/with_assertions.md) |
|----|----|----|----|
| Scores an attempt with | a reward function (0 to 1) | a reward function | pass/fail rules |
| The next attempt sees | nothing new | your feedback template | the messages of the failed rules |
| When nothing passes | returns the best attempt | returns the best attempt | errors, or warns with `on_failure = "warn"` |

A reward function takes the prediction and the inputs and returns a
score:

``` r

digits_only <- function(prediction, inputs) {
  as.numeric(grepl("^[0-9]+$", trimws(prediction$answer)))
}

digits_only(list(answer = "205"), list())
#> [1] 1
digits_only(list(answer = "205 minutes"), list())
#> [1] 0
```

### best_of_n()

[`best_of_n()`](https://jameshwade.github.io/dsprrr/reference/best_of_n.md)
runs the module up to `N` times and stops at the first attempt that
scores at least `threshold` (default 1). Each attempt gets its own cache
key, so retries are fresh samples, but they are independent: nothing
from a failed attempt reaches the next one. If no attempt reaches the
threshold, you get the highest-scoring one.

``` r

qa <- module(signature("question -> answer"))
best <- best_of_n(qa, N = 3L, reward_fn = digits_only)

result <- run(best, question = question, .llm = llm, .return_format = "structured")
result$output$answer
result$metadata$all_scores
best$get_attempts()
```

Always pass a `reward_fn`. The default gives every non-`NULL` prediction
a score of 1, so the first answer ends the loop and
[`best_of_n()`](https://jameshwade.github.io/dsprrr/reference/best_of_n.md)
behaves like the module it wraps.

When you have labeled data,
[`as_reward_fn()`](https://jameshwade.github.io/dsprrr/reference/as_reward_fn.md)
turns a metric into a reward. It compares one output field
(`prediction_field`) with an input that holds the expected value
(`expected_field`). It extracts both values itself, so pass the metric
without `field`:

``` r

matches_label <- as_reward_fn(
  metric_exact_match(),
  expected_field = "expected",
  prediction_field = "answer"
)

matches_label(list(answer = "205"), list(expected = "205"))
#> [1] 1
matches_label(list(answer = "205 minutes"), list(expected = "205"))
#> [1] 0
```

At run time the expected value travels as an extra input, and dsprrr
warns that it is not declared in the signature. A module without a
`template` adds every input to its prompt, so give it a template that
leaves the label out:

``` r

labeled <- best_of_n(
  module(signature("question -> answer"), template = "{question}"),
  N = 3L,
  reward_fn = matches_label
)
run(labeled, question = question, expected = "205", .llm = llm)
```

### refine()

[`refine()`](https://jameshwade.github.io/dsprrr/reference/refine.md)
takes the same reward and threshold. When an attempt falls short, it
fills `feedback_template`, a glue string that can use `{score}`,
[prediction](https://github.com/bbolker/prediction) and any input field,
and passes the text to the next attempt. That rendered template is the
whole feedback: the model is not asked to diagnose its mistake.

``` r

refined <- refine(
  qa,
  N = 3L,
  reward_fn = digits_only,
  feedback_template = paste(
    "Your previous attempt ({prediction}) was not a bare number.",
    "Reply with the number of minutes only."
  )
)
run(refined, question = question, .llm = llm)
refined$get_feedback_history()
```

[prediction](https://github.com/bbolker/prediction) renders as
`field: value` pairs, for example `answer: 3 h 25 min`. Here the wrapped
signature has no `feedback` input, so the feedback is added to later
prompts as a `feedback:` line. A signature can declare the input instead
(`"question, feedback -> answer"`); the first attempt then receives “No
feedback yet.”.

### with_assertions()

[`with_assertions()`](https://jameshwade.github.io/dsprrr/reference/with_assertions.md)
checks each output against rules.
[`assert_output()`](https://jameshwade.github.io/dsprrr/reference/assertions.md)
rules must pass: when one fails, the module runs again with the failure
messages added as an `assertion_feedback` input, up to `max_retries`
more times.
[`suggest_output()`](https://jameshwade.github.io/dsprrr/reference/assertions.md)
rules only warn.

``` r

checked <- with_assertions(
  qa,
  assertions = list(
    assert_output(
      ~ grepl("^[0-9]+$", .x$answer),
      "Reply with the number of minutes only, as digits."
    )
  ),
  max_retries = 2L
)
run(checked, question = question, .llm = llm)
checked$get_attempts()
```

## Compare several answers

[`multi_chain_comparison()`](https://jameshwade.github.io/dsprrr/reference/multi_chain_comparison.md)
runs a chain-of-thought version of the signature `M` times at
`temperature` (default 0.7), then makes one more call that reads all
attempts and returns a reasoned final answer. gpt-6-luna accepts that
temperature only with reasoning turned off, so the example gives it its
own chat with `reasoning_effort = "none"`. The default comparison prompt
lists the attempts but not the original inputs, so pass a
`comparison_template` that includes them. It is a glue string with
`{M}`, `{attempts_text}` and your input fields:

``` r

mcc <- multi_chain_comparison(
  "question -> answer",
  M = 3L,
  comparison_template = paste0(
    "Question: {question}\n\n{attempts_text}\n\n",
    "Compare the {M} attempts above and give the best final answer."
  )
)
mcc_llm <- ellmer::chat_openai(
  model = "gpt-6-luna",
  params = ellmer::params(reasoning_effort = "none")
)
run(mcc, question = question, .llm = mcc_llm)
mcc$get_attempts()
```

[`ensemble()`](https://jameshwade.github.io/dsprrr/reference/ensemble.md)
runs each member once and combines their outputs with `reduce_fn`.
Members must share input names. Unlike the comparison call above, a
reducer only picks among the answers it got:

``` r

voters <- ensemble(
  list(
    module(signature("question -> answer")),
    chain_of_thought("question -> answer"),
    chain_of_thought(
      "question -> answer",
      prefix = "Convert both times to minutes after midnight in order to"
    )
  ),
  reduce_fn = reduce_majority(field = "answer")
)
run(voters, question = question, .llm = llm)
```

[`reduce_majority()`](https://jameshwade.github.io/dsprrr/reference/reduce_majority.md)
votes on the first output field unless you name one, and for a
chain-of-thought member the first field is `reasoning`, so pass `field`.
Other reducers include
[`reduce_weighted_vote()`](https://jameshwade.github.io/dsprrr/reference/reduce_weighted_vote.md),
which uses `weights` such as validation scores, and
[`reduce_first()`](https://jameshwade.github.io/dsprrr/reference/reduce_first.md).
Ensembles are most useful over variants you already have, such as one
program compiled with different demos.

## Use tools: react()

When the answer depends on data the model does not have, give it tools.
A tool is an ordinary R function described with
[`ellmer::tool()`](https://ellmer.tidyverse.org/reference/tool.html), so
you can test it before a model sees it:

``` r

timetable <- data.frame(
  train = c("412", "415"),
  departs = c("09:40", "11:15"),
  arrives = c("13:05", "14:50")
)

lookup_train <- ellmer::tool(
  function(train) {
    row <- timetable[timetable$train == train, ]
    if (nrow(row) == 0) return(paste("No train numbered", train))
    paste("Train", train, "departs at", row$departs, "and arrives at", row$arrives)
  },
  description = "Look up when a train departs and arrives.",
  arguments = list(train = ellmer::type_string("Train number, such as '412'")),
  name = "lookup_train"
)

lookup_train(train = "412")
#> [1] "Train 412 departs at 09:40 and arrives at 13:05"
```

[`react()`](https://jameshwade.github.io/dsprrr/reference/react.md) lets
the model call the tool as often as it needs, then makes one structured
call for the signature’s outputs:

``` r

agent <- react("question -> answer", tools = list(lookup_train))
result <- run(
  agent,
  question = "How many minutes does train 412 take?",
  .llm = ellmer::chat_openai(model = "gpt-6-luna"),
  .return_format = "structured"
)
result$output$answer
result$metadata$tools_used
```

`max_iterations` (default 10) caps the tool rounds and errors when the
model goes past it. The agent registers its tools on the chat it runs
with, which is why this example gives it a chat of its own. Tools run in
your R session with your permissions, so keep them narrow.

## Compute with code

[`program_of_thought()`](https://jameshwade.github.io/dsprrr/reference/program_of_thought.md)
asks the model for R code, runs it in a separate R process, and uses the
value as the answer. A single value becomes the answer directly;
anything else goes back to the model to phrase. If the code fails, the
error goes back to the model for a repair, up to `max_iters` (default 3)
attempts. Inputs are available to the code as `.context$<name>`.

``` r

pot <- program_of_thought(
  "question -> answer",
  interpreter_factory = function() r_code_runner(timeout = 30)
)
result <- run(pot, question = question, .llm = llm, .return_format = "structured")
result$output$answer
result$metadata$final_code
```

Code execution is opt-in. Pass exactly one of `runner` (a runner you
create and reuse) or `interpreter_factory` (a function that returns a
fresh runner for each
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md), shut
down afterwards).
[`r_code_runner()`](https://jameshwade.github.io/dsprrr/reference/r_code_runner.md)
runs code in a separate R process with your user’s file and network
access; it is not a sandbox. For code you do not trust, use a sandboxed
runner such as
[`mcp_repl_runner()`](https://jameshwade.github.io/dsprrr/reference/mcp_repl_runner.md),
which needs the mcptools package and Posit’s `mcp-repl` executable.

[`code_act()`](https://jameshwade.github.io/dsprrr/reference/code_act.md)
combines tools with code: the model gets your tools plus an
`execute_r_code` tool, so it can look up the times and do the arithmetic
in R. Its answer is the model’s final message, returned as the
signature’s one output field.

``` r

agent <- code_act(
  "question -> answer",
  tools = list(lookup_train),
  interpreter_factory = function() r_code_runner(timeout = 30)
)
run(agent, question = "How many minutes does train 412 take?", .llm = llm)
```

## Custom logic: module_fn()

[`module_fn()`](https://jameshwade.github.io/dsprrr/reference/module_fn.md)
turns an R function into a module with a signature, so it works with
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md),
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md),
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
and pipelines. This one needs no model at all:

``` r

trip_minutes <- module_fn(
  "departs, arrives -> minutes: int",
  function(departs, arrives) {
    to_minutes <- function(time) {
      parts <- as.integer(strsplit(time, ":")[[1]])
      parts[1] * 60L + parts[2]
    }
    list(minutes = to_minutes(arrives) - to_minutes(departs))
  }
)

run(trip_minutes, departs = "9:40", arrives = "13:05")
#> $minutes
#> [1] 205
```

Put a model step in front of it and the model only has to read the
times, while R does the arithmetic:

``` r

extract_times <- module(signature("question -> departs, arrives"))
trip <- extract_times %>>% trip_minutes
run(trip, question = question, .llm = llm)
```

If the function has a `.llm` (or `...`) argument, it receives the active
chat and can call other modules itself.
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
refuses
[`module_fn()`](https://jameshwade.github.io/dsprrr/reference/module_fn.md)
programs, because an optimizer cannot see inside the function; compile
the modules it calls instead.

## Explore large inputs: rlm_module() (experimental)

[`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md)
keeps its inputs in an R session and lets the model examine them one
step at a time: it writes a little R, reads bounded output, decides what
to look at next, and calls `SUBMIT()` when it has the answer. Use it
when the evidence sits in an object too large or irregular to paste into
a prompt and you cannot say in advance which slice matters. It needs a
persistent runner. [How RLM
works](https://jameshwade.github.io/dsprrr/articles/how-rlm-works.md)
explains the execution model and runner choices, and [Investigate a
regression with
RLM](https://jameshwade.github.io/dsprrr/articles/tutorial-rlm-dsprrr.md)
works through an example.

## Search the program shape: flex() (experimental)

[`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md) wraps
a signature in a program whose source GEPA can rewrite: it can add or
remove predictor steps and, with executable source, add R logic or tool
calls. Use it when the right structure is what you want to find out. If
you already know the structure, write a module or a pipeline. [Flex:
optimize a whole
program](https://jameshwade.github.io/dsprrr/articles/flex-optimization.md)
shows a complete run.

## Wrappers and optimization

Wrappers keep the module they wrap.
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)
can tune their settings, for example `parameters = list(N = c(1L, 3L))`,
and changes the wrapper in place. Demo-based compilation does not work
on a wrapped program
([`LabeledFewShot()`](https://jameshwade.github.io/dsprrr/reference/LabeledFewShot.md)
stops with “cannot label nested predictors”), so compile the inner
module on a labeled `trainset` (columns `question` and `answer`) and
wrap the compiled copy:

``` r

compiled_qa <- compile(qa, LabeledFewShot(k = 4L), trainset)
reliable <- best_of_n(compiled_qa, N = 3L, reward_fn = digits_only)
```

For multi-step programs, [Chain modules into
pipelines](https://jameshwade.github.io/dsprrr/articles/chaining-modules.md)
shows how to connect modules and compile them together.
