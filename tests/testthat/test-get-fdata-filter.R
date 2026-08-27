test_that("canonical getFData filters rows without selected dpType", {

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

  expect_false(
    any(!meltLong$mdp)
  )

  expect_setequal(
    unique(meltLong$expId),
    "expM"
  )

  ampLong <- getFData(
    x,
    dpType = "adp",
    longTable = TRUE
  )

  expect_true(
    all(ampLong$adp)
  )

  expect_false(
    any(!ampLong$adp)
  )

  expect_setequal(
    unique(ampLong$expId),
    "expA"
  )
})


test_that("includeMissing restores the previous canonical left-join behaviour", {

  x <- .rdmlNewImport("test")

  adp <- data.table::data.table(
    cyc = 1:2,
    amp = c(1, 2)
  )

  description <- data.table::data.table(
    fdataName = "amp",
    expId = "exp1",
    runId = "run1",
    reactId = "A1",
    sample = "s1",
    sampleType = "unkn",
    target = "ACTB",
    targetDyeId = "FAM"
  )

  x <- setFData(
    x,
    adp,
    description,
    fdataType = "adp"
  )

  out <- getFData(
    x,
    dpType = "mdp",
    longTable = TRUE,
    includeMissing = TRUE
  )

  expect_equal(
    nrow(out),
    1L
  )

  expect_false(
    out$mdp[[1L]]
  )

  expect_true(
    is.na(out$tmp[[1L]])
  )

  expect_true(
    is.na(out$fluor[[1L]])
  )
})


test_that("legacy GetFData retains legacy missing-row behaviour", {

  x <- .rdmlNewImport("test")

  adp <- data.table::data.table(
    cyc = 1:2,
    amp = c(1, 2)
  )

  description <- data.table::data.table(
    fdataName = "amp",
    expId = "exp1",
    runId = "run1",
    reactId = "A1",
    sample = "s1",
    sampleType = "unkn",
    target = "ACTB",
    targetDyeId = "FAM"
  )

  x <- setFData(
    x,
    adp,
    description,
    fdataType = "adp"
  )

  out <- GetFData(
    x,
    dp.type = "mdp",
    long.table = TRUE
  )

  expect_equal(
    nrow(out),
    1L
  )

  expect_false(
    out$mdp[[1L]]
  )

  expect_true(
    "fdata.name" %in% names(out)
  )
})
