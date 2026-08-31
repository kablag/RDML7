# Extracted from test-refactor-smoke.R:7

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "RDML7", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
x <- rdmlType()
testthat::expect_identical(
    x$version,
    "1.3"
  )
