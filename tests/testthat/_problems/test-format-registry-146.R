# Extracted from test-format-registry.R:146

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "RDML", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
names_to_remove <- c(
    "test-sniff-a",
    "test-sniff-b"
  )
on.exit(
    for (name in names_to_remove) {
      try(
        rdmlUnregisterFormat(name),
        silent = TRUE
      )
    },
    add = TRUE
  )
rdmlRegisterFormat(
    name = "test-sniff-a",
    extensions = "ambqpcr",
    reader = function(filename) {
      .rdmlNewImport("A")
    },
    sniff = function(filename) 0.2
  )
