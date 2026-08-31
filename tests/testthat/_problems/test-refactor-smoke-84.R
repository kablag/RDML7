# Extracted from test-refactor-smoke.R:84

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "RDML", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
cDNA <- cdnaSynthesisMethodType(
    primingMethod = primingMethodType("random")
  )
