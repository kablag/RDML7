# Extracted from test-import-data-s7.R:115

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "RDML7", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
fdata <- data.table::data.table(
    cyc = 1:2,
    curve = c(1, 2)
  )
description <- data.table::data.table(
    fdataName = "curve",
    expId = "exp1",
    runId = "run1",
    reactId = "A1",
    sample = "s1",
    sampleType = "unkn",
    target = "ACTB",
    targetDyeId = "FAM"
  )
series <- rdmlImportSeries(
    fdataType = "adp",
    fdata = fdata,
    description = description
  )
parsed <- rdmlImportData(
    series = list(series),
    publisher = "test",
    format = "test"
  )
expect_identical(
    parsed$publisher,
    "test"
  )
expect_length(
    parsed$series,
    1L
  )
expect_identical(
    parsed$losses,
    list()
  )
x <- rdmlBuildImport(
    parsed,
    loss = "allow"
  )
expect_true(
    S7::S7_inherits(
      x,
      rdmlType
    )
  )
expect_true(
    x$experiment$exp1$
      run$run1$
      react$A1$
      data$ACTB$
      adp |>
      .rdmlPresent()
  )
expect_identical(
    x$version,
    "1.3"
  )
expect_error(
    x$version <- "1.2",
    "read-only"
  )
