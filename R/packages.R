# R/packages.R
library(targets)

tar_option_set(
  packages = c(
    "httr2","jsonlite","dplyr","purrr","tidyr","stringr","lubridate",
    "tibble","furrr","fs","arrow","uuid","readr","future"
  ),
  format = "rds",
  workspace_on_error = FALSE
)

# You control parallelism per-target in _targets.R (using future::plan).
