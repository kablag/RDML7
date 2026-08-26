test_that("camel and legacy table APIs remain separate", {

  x <- .rdmlNewImport("test")

  fdata <- data.table::data.table(
    cyc = 1:3,
    curve = c(1, 2, 3)
  )

  description <- data.table::data.table(
    fdataName = "curve",
    expId = "exp1",
    runId = "run1",
    reactId = "1",
    sample = "sample1",
    sampleType = "unkn",
    target = "ACTB",
    targetDyeId = "EvaGreen"
  )

  x <- setFData(
    x,
    fdata,
    description
  )

  modern <- asTable(x)
  legacy <- AsTable(x)

  expect_true(
    all(
      c(
        "fdataName",
        "expId",
        "runId",
        "reactId",
        "sampleType",
        "targetDyeId"
      ) %in% names(modern)
    )
  )

  expect_true(
    all(
      c(
        "fdata.name",
        "exp.id",
        "run.id",
        "react.id",
        "sample.type",
        "target.dyeId"
      ) %in% names(legacy)
    )
  )

  expect_false("fdata.name" %in% names(modern))
  expect_false("fdataName" %in% names(legacy))
})


test_that("legacy SetFData accepts old description columns", {

  x <- .rdmlNewImport("test")

  fdata <- data.table::data.table(
    cyc = 1:2,
    curve = c(2, 4)
  )

  description <- data.table::data.table(
    fdata.name = "curve",
    exp.id = "exp1",
    run.id = "run1",
    react.id = "1",
    sample = "sample1",
    sample.type = "unkn",
    target = "ACTB",
    target.dyeId = "EvaGreen"
  )

  x <- SetFData(
    x,
    fdata,
    description,
    fdata.type = "adp"
  )

  expect_true(
    "fdata.name" %in% names(
      AsTable(x)
    )
  )
})


test_that("GetFData naming follows the chosen API", {

  x <- .rdmlNewImport("test")

  fdata <- data.table::data.table(
    cyc = 1:2,
    curve = c(3, 5)
  )

  description <- data.table::data.table(
    fdataName = "curve",
    expId = "exp1",
    runId = "run1",
    reactId = "1",
    sample = "s1",
    sampleType = "unkn",
    target = "ACTB",
    targetDyeId = "EvaGreen"
  )

  x <- setFData(x, fdata, description)

  modern <- getFData(
    x,
    longTable = TRUE
  )

  legacy <- GetFData(
    x,
    long.table = TRUE
  )

  expect_true("fdataName" %in% names(modern))
  expect_false("fdata.name" %in% names(modern))

  expect_true("fdata.name" %in% names(legacy))
  expect_false("fdataName" %in% names(legacy))
})


test_that("legacy and modern formals are distinct", {

  expect_true("showProgress" %in% names(formals(rdmlRead)))
  expect_true("show.progress" %in% names(formals(rdml_read)))

  expect_true("fdataType" %in% names(formals(rdmlFromFData)))
  expect_true("fdata.type" %in% names(formals(rdml_from_fdata)))

  expect_true("namePattern" %in% names(formals(asTable)))
  expect_true("name.pattern" %in% names(formals(AsTable)))
})


test_that("repeat is never emitted as a bare named argument", {

  rDir <- testthat::test_path("../../R")

  text <- paste(
    unlist(
      lapply(
        list.files(
          rDir,
          pattern = "\\.R$",
          full.names = TRUE
        ),
        readLines,
        warn = FALSE
      )
    ),
    collapse = "\n"
  )

  expect_false(
    grepl(
      "(^|[^\"`[:alnum:]_.])repeat[[:space:]]*=",
      text,
      perl = TRUE
    )
  )
})
