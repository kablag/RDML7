# Extracted from test-refactor-smoke.R:35

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "RDML", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
e <- experimentType(
    id = idType("old"),
    run = list()
  )
