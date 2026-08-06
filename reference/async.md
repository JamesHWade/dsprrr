# Asynchronous Module Operations

Functions for running modules asynchronously using promises. Useful for
parallel execution or non-blocking operations. The direct provider paths
support ordinary `PredictModule` objects. ProgramOfThought, CodeAct, and
RLM modules may also use
[`run_async()`](https://jameshwade.github.io/dsprrr/reference/run_async.md)
when configured with `interpreter_factory`: the complete workflow runs
in an isolated mirai process with one invocation-owned interpreter.
Their constructor-bound caller-owned runners and all specialized
streaming paths remain rejected.
