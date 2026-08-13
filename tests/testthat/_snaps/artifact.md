# the closed value codec preserves supported R data types

    Code
      program_artifact(unsafe)
    Condition
      Error in `artifact_encode_runtime()`:
      ! Runtime value is not registered for persistence
      x graph.nodes.$.config.value contains <custom_atomic>.
      i Supply it in a named `registry`, or explicitly use `trusted = TRUE`.

# registered function modules preserve behavior

    Code
      restore_module_config(artifact)
    Condition
      Error in `artifact_decode_runtime()`:
      ! Program artifact requires an unavailable registry value
      x Missing registry ID "first".

---

    Code
      restore_module_config(artifact, registry = list(first = incompatible, second = second_fn))
    Condition
      Error in `artifact_decode_runtime()`:
      ! Program artifact registry interface does not match
      x Registry ID "first" has a different callable or resource interface.

# tools require registry IDs and round-trip by identity

    Code
      program_artifact(program)
    Condition
      Error in `artifact_encode_runtime()`:
      ! Runtime value is not registered for persistence
      x graph.nodes.$.fields.tools[[1]] contains <ellmer::ToolDef>.
      i Supply it in a named `registry`, or explicitly use `trusted = TRUE`.

# embedded runtime values require dual trusted opt-in

    Code
      program_artifact(program)
    Condition
      Error in `artifact_encode_runtime()`:
      ! Runtime value is not registered for persistence
      x graph.nodes.$.fields.reduce_fn contains <function>.
      i Supply it in a named `registry`, or explicitly use `trusted = TRUE`.

---

    Code
      restore_module_config(artifact)
    Condition
      Error in `artifact_decode_runtime()`:
      ! Program artifact contains an embedded runtime value
      i Restore only a trusted artifact with `trusted = TRUE`.

---

    Code
      load_program(path)
    Condition
      Error in `artifact_decode_runtime()`:
      ! Program artifact contains an embedded runtime value
      i Restore only a trusted artifact with `trusted = TRUE`.

# malformed, unsupported, and corrupt artifacts fail with typed errors

    Code
      restore_module_config(malformed)
    Condition
      Error in `malformed()`:
      ! Malformed dsprrr program artifact
      x graph.nodes must be a non-empty, uniquely named list.

---

    Code
      restore_module_config(unsupported)
    Condition
      Error in `artifact_validate_manifest()`:
      ! Unsupported dsprrr program artifact version
      x Got version 999; this package supports versions 3, 4, and 5.

---

    Code
      restore_module_config(corrupt)
    Condition
      Error in `artifact_validate_integrity()`:
      ! Program artifact integrity check failed

---

    Code
      restore_module_config(metadata_leak)
    Condition
      Error in `artifact_validate_metadata()`:
      ! Program artifact has invalid producer metadata

---

    Code
      restore_module_config(unreachable)
    Condition
      Error in `malformed()`:
      ! Malformed dsprrr program artifact
      x Unreachable graph node: $/unused.

---

    Code
      restore_module_config(dependency)
    Condition
      Error in `artifact_validate_dependencies()`:
      ! Program artifact dependency version is unavailable
      x Package ellmer requires version "999.0.0" or newer.

---

    Code
      load_program(path)
    Condition
      Error in `artifact_read_rds()`:
      ! Could not read program artifact
      Caused by error in `readRDS()`:
      ! unknown input format

# cycles and unsupported custom classes fail explicitly

    Code
      program_artifact(cycle)
    Condition
      Error in `program_artifact()`:
      ! Cyclic module graphs cannot be persisted
      x Path "$/self" points back to "$".
      i Break the cycle before creating an artifact. Shared acyclic modules are supported.

---

    Code
      program_artifact(custom)
    Condition
      Error in `artifact_module_class()`:
      ! Unsupported module class in program artifact
      x Cannot reconstruct <ArtifactCycleProgram>.
      i This artifact version supports built-in module classes only; custom Module subclasses need an explicit artifact codec.

# tampered composite signatures fail validation

    Code
      restore_module_config(tampered)
    Condition
      Error in `malformed()`:
      ! Malformed dsprrr program artifact
      x Node $ has a signature inconsistent with its children.

# module code export restores from the complete manifest

    Code
      export_module_code(callable, registry = list(forward = forward))
    Condition
      Error in `export_module_code()`:
      ! Standalone R code cannot embed this program's runtime dependencies
      x Artifact contains "registry" runtime reference.
      i Use `save_program()` with a registry-backed deployment contract instead.
