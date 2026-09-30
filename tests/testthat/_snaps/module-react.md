# module() never changes into ReAct when tools are supplied

    Code
      module(sig, tools = list())
    Condition
      Error in `reject_module_arguments()`:
      ! `module()` creates standard prediction modules and does not accept `tools`.
      i Use react() for tool use, or code_act() when code execution is also required.
