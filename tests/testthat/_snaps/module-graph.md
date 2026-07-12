# shared modules and cycles are detected by identity

    Code
      module_graph(cyclic, cycles = "error")
    Condition
      Error in `visit()`:
      ! Cycle detected in module graph
      x Path "$/self" points back to "$".

# shared replacement preflights every writable alias

    Code
      map_modules(program, function(module, path) {
        if (identical(module, shared)) replacement else module
      })
    Condition
      Error in `map_modules()`:
      ! Module graph mapping failed; structural replacements were rolled back
      Caused by error in `module_graph_apply_replacement()`:
      ! Module graph path "$/read_only/alias" is read-only
      i Custom programs must implement set_graph_children(children).

---

    Code
      replace_module(program, "$/writable/modules/1", replacement, shared = "all")
    Condition
      Error in `module_graph_apply_replacement()`:
      ! Module graph path "$/read_only/alias" is read-only
      i Custom programs must implement set_graph_children(children).

# incompatible composite replacement is atomic

    Code
      replace_module(program, "$/modules/left", incompatible)
    Condition
      Error in `validate_signature_compatibility()`:
      ! Incompatible signatures in EnsembleModule
      x Module 1 expects inputs: question
      x Module 2 expects inputs: text
      i All modules must have the same input field names

---

    Code
      replace_module(nested, "$/modules/left/module", incompatible)
    Condition
      Error in `replace_module()`:
      ! Module replacement failed validation and was rolled back
      Caused by error in `validate_signature_compatibility()`:
      ! Incompatible signatures in EnsembleModule
      x Module 1 expects inputs: question
      x Module 2 expects inputs: text
      i All modules must have the same input field names

# mapping rolls back earlier structural replacements on failure

    Code
      map_modules(program, function(module, path) {
        if (identical(path, "$/modules/right")) {
          compatible
        } else if (identical(path, "$/modules/left")) {
          incompatible
        } else {
          module
        }
      })
    Condition
      Error in `map_modules()`:
      ! Module graph mapping failed; structural replacements were rolled back
      Caused by error in `validate_signature_compatibility()`:
      ! Incompatible signatures in EnsembleModule
      x Module 1 expects inputs: question
      x Module 2 expects inputs: text
      i All modules must have the same input field names

# frozen and compiled modules form deterministic boundaries

    Code
      replace_module(outer, "$/module/module", graph_test_module())
    Condition
      Error in `replace_module()`:
      ! Cannot replace a module across a frozen or compiled boundary
      x Protected path: "$/module/module"
      i Use `boundaries = "cross"` only when replacing that boundary is intentional.
