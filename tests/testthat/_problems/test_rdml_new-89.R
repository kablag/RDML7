# Extracted from test_rdml_new.R:89

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "RDML7", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
filename <- system.file(
      "extdata",
      "from_tables",
      "table.xlsx",
      package = "RDML7"
    )
x <- rdmlRead(
      filename,
      showProgress = FALSE,
      loss = "allow"
    )
