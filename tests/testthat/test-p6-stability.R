test_that("intermediate importer data builds an RDML object", {
  series <- rdmlImportSeries(
    fdataType = "adp",
    fdata = data.table::data.table(cyc = 1:2, curve = c(1, 2)),
    description = data.table::data.table(
      fdataName = "curve",
      expId = "exp1",
      runId = "run1",
      reactId = "A1",
      sample = "s1",
      sampleType = "unkn",
      target = "ACTB",
      targetType = "ref",
      targetDyeId = "EvaGreen"
    )
  )

  parsed <- rdmlImportData(
    series = list(series),
    publisher = "test",
    format = "unit"
  )

  x <- rdmlBuildImport(parsed)
  expect_true(S7::S7_inherits(x, rdmlType))
  expect_identical(.rdmlEnumChr(x$target$ACTB$type), "ref")
})


test_that("semantic validation catches dangling references", {
  x <- rdmlType(
    version = "1.2",
    experiment = list(
      experimentType(
        id = idType("exp1"),
        run = list(
          runType(
            id = idType("run1"),
            react = list(
              reactType(
                id = idType("A1"),
                sample = idReferenceType("missing"),
                data = list(),
                partitions = list()
              )
            )
          )
        )
      )
    )
  )

  issues <- rdmlValidate(x, level = "references")
  expect_true("unknownSampleReference" %in% issues$code)
  expect_false(rdmlIsValid(x, level = "references"))
})


test_that("multiple Tm values are retained in memory", {
  x <- rdmlFromFData(
    fdata = data.table::data.table(tmp = c(70, 71), curve = c(1, 2)),
    description = data.table::data.table(
      fdataName = "curve",
      expId = "exp1",
      runId = "run1",
      reactId = "A1",
      sample = "s1",
      sampleType = "unkn",
      target = "ACTB",
      targetDyeId = "EvaGreen",
      meltTemp = 71.5,
      meltTemps = I(list(c(71.5, 75.2)))
    ),
    fdataType = "mdp"
  )

  d <- x$experiment$exp1$run$run1$react$A1$data$ACTB
  expect_equal(d$meltTemp, 71.5)
  expect_equal(d$meltTemps, c(71.5, 75.2))
})


test_that("format registry reports plugin API and capabilities", {
  formats <- rdmlFormats()
  expect_true(all(c("apiVersion", "capabilities") %in% names(formats)))
  expect_equal(formats[formats$format == "rdes", "apiVersion"], 1L)
  expect_match(formats[formats$format == "rdes", "capabilities"], "multiTm")
})



test_that("legacy rdml_formats keeps the old column contract", {
  old <- rdml_formats()
  expect_identical(
    names(old),
    c(
      "format",
      "extensions",
      "aliases",
      "read",
      "write",
      "sniff",
      "priority",
      "builtin"
    )
  )
})
