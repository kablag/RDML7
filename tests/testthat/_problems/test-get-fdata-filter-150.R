# Extracted from test-get-fdata-filter.R:150

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "RDML", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
x <- .rdmlNewImport("test")
