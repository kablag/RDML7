# Extracted from test-format-registry.R:101

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "RDML", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
name <- "test-dummy-writer"
on.exit(
    try(
      rdmlUnregisterFormat(name),
      silent = TRUE
    ),
    add = TRUE
  )
rdmlRegisterFormat(
    name = name,
    extensions = "dummyout",
    writer = function(
        x,
        filename,
        ...) {
      writeLines(
        "ok",
        filename
      )

      invisible(filename)
    }
  )
