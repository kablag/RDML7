# Extracted from test_rdml_new.R:69

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "RDML7", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
filename <- system.file(
      "extdata",
      "from_tables",
      "fdata.csv",
      package = "RDML7"
    )
x <- rdmlRead(
      filename,
      showProgress = FALSE,
      loss = "allow"
    )
testthat::expect_true(
      S7::S7_inherits(
        x,
        rdmlType
      )
    )
testthat::expect_identical(
      x$version,
      "1.3"
    )
