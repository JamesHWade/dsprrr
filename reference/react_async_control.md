# Create cooperative control for asynchronous ReAct execution

The control can report ordered progress events and request cooperative
cancellation. Cancellation is checked between provider requests and tool
calls; it does not interrupt a request or tool that is already running.
Progress callback errors reject the async call after cleanup. A failed
or cancelled call can leave partial native history; requested tool calls
and actual tool results are recorded separately, without replaying
tools.

## Usage

``` r
react_async_control(on_progress = NULL)
```

## Arguments

- on_progress:

  Optional function called with progress event lists. Events include a
  sequence number and trace correlation fields, but never include
  prompts, tool arguments, or reasoning content.

## Value

A one-invocation control object. Call `$cancel(reason)` to request
cancellation while a
[`run_async()`](https://jameshwade.github.io/dsprrr/reference/run_async.md)
promise is pending. Repeated cancel requests after the first are
ignored.

## Examples

``` r
control <- react_async_control()
control$is_cancelled()
#> [1] FALSE
control$cancel("User requested a stop")
control$is_cancelled()
#> [1] TRUE
control$cancel_reason()
#> [1] "User requested a stop"
```
