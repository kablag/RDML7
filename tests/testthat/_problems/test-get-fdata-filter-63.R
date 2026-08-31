# Extracted from test-get-fdata-filter.R:63

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "RDML7", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
x <- .rdmlNewImport("test")
adp <- data.table::data.table(
    cyc = 1:2,
    amp = c(1, 2)
  )
adpDescription <- data.table::data.table(
    fdataName = "amp",
    expId = "expA",
    runId = "run1",
    reactId = "A1",
    sample = "s1",
    sampleType = "unkn",
    target = "AMP",
    targetDyeId = "FAM"
  )
x <- setFData(
    x,
    adp,
    adpDescription,
    fdataType = "adp"
  )
mdp <- data.table::data.table(
    tmp = c(70, 71),
    melt = c(10, 12)
  )
mdpDescription <- data.table::data.table(
    fdataName = "melt",
    expId = "expM",
    runId = "run1",
    reactId = "B1",
    sample = "s2",
    sampleType = "unkn",
    target = "MELT",
    targetDyeId = "HEX"
  )
x <- setFData(
    x,
    mdp,
    mdpDescription,
    fdataType = "mdp"
  )
meltLong <- getFData(
    x,
    dpType = "mdp",
    longTable = TRUE
  )
expect_true(
    nrow(meltLong) > 0L
  )
expect_true(
    all(meltLong$mdp)
  )
