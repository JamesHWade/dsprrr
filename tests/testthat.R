# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(dsprrr)

# Keep CRAN checks within the Windows time budget. Core contracts and
# representative integration checks run here; developer checks (including
# CI) run every file with NOT_CRAN=true.
cran_files <- c(
  "accessors",
  "artifact-phases",
  "assertion",
  "async-result-contract",
  "compile",
  "decision",
  "ellmer-compat",
  "file-identity",
  "history",
  "input",
  "inspect",
  "interpolate",
  "metric-feedback",
  "metrics",
  "module-fn",
  "optimization-result",
  "optimizer-accessors",
  "optimizer-evaluation-phases",
  "parallel-batch-phases",
  "pipeline",
  "predict",
  "react-submit",
  "reasoning-mode",
  "rlm-phases",
  "rlm-predictor-contract",
  "signature",
  "signature-parser",
  "signature-transforms",
  "storage-contract",
  "streaming",
  "teleprompter",
  "trace-context",
  "traces",
  "utils"
)

if (isTRUE(as.logical(Sys.getenv("NOT_CRAN")))) {
  test_check("dsprrr")
} else {
  # Anchor both ends so an integration suite with a related name does not
  # accidentally become part of the time-bounded tier.
  filter <- paste0("^(", paste(cran_files, collapse = "|"), ")$")
  test_check("dsprrr", filter = filter)
}
