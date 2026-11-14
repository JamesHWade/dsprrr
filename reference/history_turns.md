# Reconstruct native ellmer turns from conversation history

Reconstruct native ellmer turns from conversation history

## Usage

``` r
history_turns(history)
```

## Arguments

- history:

  An object created by conversation_history(), loaded by load_history(),
  or returned in structured result metadata.

## Value

A fresh unnamed list of ellmer UserTurn and AssistantTurn objects for
empty or ReAct histories. RLM histories contain observed invocation
exchanges and cannot be reconstructed as ellmer turns. Assistant JSON
and finish reason are retained. Usage, cost, and duration metadata use
ellmer defaults.
