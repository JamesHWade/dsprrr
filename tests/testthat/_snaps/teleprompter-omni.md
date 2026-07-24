# Omni validates its optimizer surface

    Code
      Omni(metric = omni_metric, explorers = list(a = OmniMarkingTeleprompter(marker = "a")),
      continuation = OmniMarkingTeleprompter(marker = "-c", append = TRUE))
    Condition
      Error:
      ! <dsprrr::Omni> object properties are invalid:
      - @explorers explorers must be a named list of at least two teleprompters

---

    Code
      Omni(metric = omni_metric, explorers = list(OmniMarkingTeleprompter(marker = "a"),
      OmniMarkingTeleprompter(marker = "b")), continuation = OmniMarkingTeleprompter(
        marker = "-c", append = TRUE))
    Condition
      Error:
      ! <dsprrr::Omni> object properties are invalid:
      - @explorers explorers must be named

# Omni isolates explorer failures

    Code
      compiled <- compile(tp, make_omni_mock_module(), omni_trainset, valset = omni_valset)
    Condition
      Warning:
      Omni explorer broken failed; continuing with other candidates
      x intentional Omni explorer failure

# Omni requires comparison data

    Code
      compile(make_omni(), make_omni_mock_module(), data.frame(x = "only-row",
        target = "unused"))
    Condition
      Error in `compile_omni()`:
      ! Omni requires validation data to compare exploration branches
      i Supply `valset` or use a larger `trainset`

# Omni validates per-optimizer compile arguments

    Code
      compile(make_omni(), make_omni_mock_module(), omni_trainset, valset = omni_valset,
      explorer_compile_args = list(missing = list(extra = TRUE)))
    Condition
      Error in `validate_omni_explorer_args()`:
      ! `explorer_compile_args` contains unknown explorer names
      x Unknown: missing
      i Valid names: a and b

---

    Code
      compile(make_omni(), make_omni_mock_module(), omni_trainset, valset = omni_valset,
      continuation_compile_args = list(trainset = omni_trainset))
    Condition
      Error in `validate_omni_step_args()`:
      ! `continuation_compile_args` cannot override Omni core inputs
      x Blocked arguments: trainset

---

    Code
      compile(make_omni(), make_omni_mock_module(), omni_trainset, valset = omni_valset,
      continuation_compile_args = list(TRUE))
    Condition
      Error in `validate_omni_step_args()`:
      ! `continuation_compile_args` must contain only named arguments

# Omni rejects unsafe parallel chat serialization

    Code
      compile(make_omni(parallel = TRUE), make_omni_mock_module(), omni_trainset,
      valset = omni_valset, .llm = list(provider = "not-serializable"))
    Condition
      Error in `compile_omni()`:
      ! Parallel Omni exploration requires `.llm = NULL`
      i Configure worker-visible default chat credentials, or use `parallel = FALSE`
