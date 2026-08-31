# Extracted from test-import-data-s7.R:23

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "RDML", path = "..")
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
