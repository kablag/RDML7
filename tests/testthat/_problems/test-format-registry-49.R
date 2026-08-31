# Extracted from test-format-registry.R:49

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "RDML", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
name <- "test-dummy-reader"
on.exit(
    try(
      rdmlUnregisterFormat(name),
      silent = TRUE
    ),
    add = TRUE
  )
rdmlRegisterFormat(
    name = name,
    extensions = "dummyqpcr",
    reader = function(filename) {
      .rdmlNewImport(
        publisher = "dummy"
      )
    }
  )
