# GEPA validates properties

    Code
      GEPA(component_selector = "random")
    Condition
      Error in `GEPA()`:
      ! <dsprrr::GEPA> object properties are invalid:
      - @component_selector component_selector must be 'round_robin', 'all', or a function

---

    Code
      GEPA(max_merge_invocations = -1L)
    Condition
      Error in `GEPA()`:
      ! <dsprrr::GEPA> object properties are invalid:
      - @max_merge_invocations max_merge_invocations must be a non-negative integer or NULL

# GEPA rejects missing or invalid reflection Chats before evaluation

    Code
      conditionMessage(missing_chat)
    Output
      [1] "GEPA requires a reflection Chat to propose candidate changes\ni Supply `.reflection_llm` to `compile()`.\ni For compatibility, an explicitly supplied `.llm` is also used for reflection."

---

    Code
      conditionMessage(invalid_chat)
    Output
      [1] "`.reflection_llm` must be an ellmer Chat R6 object\nx Got <list>.\ni Create one with an `ellmer::chat_*()` constructor."

