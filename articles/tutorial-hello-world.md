# Tutorial 1: Your first LLM call

By the end of this tutorial you can describe a task as a signature, run
it against a language model, and get the answer back as an R value of
the type you asked for: a string, a number, a label or `TRUE`/`FALSE`.

## Set up

Install dsprrr from GitHub. pak installs ellmer from CRAN along with it.

``` r

# install.packages("pak")
pak::pak("JamesHWade/dsprrr")
```

The tutorials use OpenAI. Add `OPENAI_API_KEY=<your key>` to your
`.Renviron` file and restart R;
[`dsprrr_sitrep()`](https://jameshwade.github.io/dsprrr/reference/dsprrr_sitrep.md)
then reports which API keys it can see.

``` r

library(dsprrr)
library(ellmer)
```

dsprrr sends requests through an ellmer chat object. The tutorials use
OpenAI’s `gpt-6-luna`:

``` r

chat <- chat_openai(model = "gpt-6-luna")
```

The answers on this page were recorded with `gpt-4.1` and are replayed
when the site is built, so `gpt-6-luna` may word them differently.

Any other ellmer chat, such as
[`chat_anthropic()`](https://ellmer.tidyverse.org/reference/chat_anthropic.html),
works the same way.

## Ask a question

A signature lists a task’s inputs and outputs: `"question -> answer"`
has one input, `question`, and one output, `answer`.
[`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)
turns the signature into something you can run:

``` r

qa <- module(signature("question -> answer"))
run(qa, question = "What is the capital of France?", .llm = chat)
#> $answer
#> [1] "The capital of France is Paris."
```

[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) returns
a named list with one element per output, so the reply is in `$answer`.
This is the prompt dsprrr wrote from the signature:

``` r

cat(get_last_prompt()$prompt)
#> Given the fields `question`, produce the fields `answer`.
#> 
#> # Input: question
#> question: What is the capital of France?
```

Along with the prompt, dsprrr sends a schema asking for an object with
one string field, `answer`, and ellmer parses the reply into R. With
ellmer alone you would write the prompt and the schema yourself. Because
dsprrr builds both from the signature, it can also change the prompt for
you, which is how later tutorials improve a module.

The same module answers any question:

``` r

run(qa, question = "What is 7 * 8?", .llm = chat)
#> $answer
#> [1] "7 multiplied by 8 is 56."

run(qa, question = "Who wrote Romeo and Juliet?", .llm = chat)
#> $answer
#> [1] "Romeo and Juliet was written by William Shakespeare."
```

The signature fixes the shape of the result, a string named `answer`,
but not its wording: the model chose to reply in full sentences. Output
types and instructions give you more say over the value.

## Choose an output type

Add a type after an output’s name. With `number` you get a number you
can compute with:

``` r

math <- module(signature("math_problem -> result: number"))
run(math, math_problem = "What is 15% of 200?", .llm = chat)
#> $result
#> [1] 30
```

`enum()` lists the allowed labels, and the schema sent to the model
permits only those values:

``` r

sentiment <- module(
  signature("text -> sentiment: enum('positive', 'negative', 'neutral')")
)
run(sentiment, text = "I absolutely loved this movie!", .llm = chat)
#> $sentiment
#> [1] "positive"
```

``` r

run(sentiment, text = "This was a complete waste of time.", .llm = chat)
#> $sentiment
#> [1] "negative"

run(sentiment, text = "It was okay, I guess.", .llm = chat)
#> $sentiment
#> [1] "neutral"
```

`bool` returns `TRUE` or `FALSE`:

``` r

truth <- module(signature("statement -> is_true: bool"))
run(truth, statement = "The Earth orbits the Sun.", .llm = chat)
#> $is_true
#> [1] TRUE

run(truth, statement = "Cats are larger than elephants.", .llm = chat)
#> $is_true
#> [1] FALSE
```

An output without a type is a string. The [quick
reference](https://jameshwade.github.io/dsprrr/articles/cheatsheet.md)
lists the other types, such as `int` and lists.

## Pass several inputs

Separate inputs with commas. A common pattern is to pass in text for the
model to answer from:

``` r

contextual_qa <- module(signature("context, question -> answer"))
run(
  contextual_qa,
  context = "R was created in 1993 by Ross Ihaka and Robert Gentleman at the University of Auckland.",
  question = "When was R created?",
  .llm = chat
)
#> $answer
#> [1] "R was created in 1993."
```

``` r

run(
  contextual_qa,
  context = "The bakery opens at 7am and closes at 6pm. They sell croissants for $3 each.",
  question = "How much do croissants cost?",
  .llm = chat
)
#> $answer
#> [1] "Croissants cost $3 each."
```

## Add instructions

`instructions` replaces the default first line of the prompt (“Given the
fields …”) with your own description of the task:

``` r

one_word <- module(
  signature("question -> answer", instructions = "Answer in exactly one word.")
)
run(one_word, question = "What color is the sky on a clear day?", .llm = chat)
#> $answer
#> [1] "Blue"

pirate <- module(
  signature("question -> answer", instructions = "Answer like a pirate.")
)
run(pirate, question = "What is the capital of France?", .llm = chat)
#> $answer
#> [1] "Arrr, matey! The capital of France be Paris!"
```

Passing `.llm` on every call keeps these examples explicit.
`set_default_chat(chat)` makes a chat the default, so you can leave
`.llm` out.

Next, [Tutorial
2](https://jameshwade.github.io/dsprrr/articles/tutorial-build-classifier.md)
builds a sentiment classifier and runs it on one text, a vector of texts
and a data frame.
