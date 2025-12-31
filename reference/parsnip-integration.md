# parsnip Integration for DSPrrr

Provides tidymodels/parsnip integration for dsprrr modules, enabling
LLM-based prediction within the tidymodels ecosystem.

## Details

This integration allows dsprrr modules to be used as parsnip model
specifications, making them compatible with tidymodels workflows
including
[`tune::tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html),
[`workflows::workflow()`](https://workflows.tidymodels.org/reference/workflow.html),
and `rsample` resampling.

The integration registers a "dsprrr" engine for text classification and
generation tasks.
