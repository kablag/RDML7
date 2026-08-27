testthat::test_that("schema uses corrected property names", {
  x <- rdmlType(version = "1.2")

  testthat::expect_true("dateUpdated" %in% names(x))
  testthat::expect_false("dateUpadted" %in% names(x))

  run <- runType(
    id = idType("run1"),
    pcrFormat = pcrFormatType(
      rows = 8L,
      columns = 12L,
      rowLabel = labelFormatType("ABC"),
      columnLabel = labelFormatType("123")
    ),
    react = list()
  )

  testthat::expect_true("backgroundDeterminationMethod" %in% names(run))

  react <- reactType(
    id = idType("1"),
    sample = idReferenceType("S1"),
    data = list(),
    partitions = list()
  )

  testthat::expect_true("partitions" %in% names(react))
})


testthat::test_that("keyed list supports targetId and rename", {
  e <- experimentType(
    id = idType("old"),
    run = list()
  )

  x <- rdmlKeyedList(list(e), key = "id")

  x[["old"]]$id <- idType("new")

  testthat::expect_identical(names(x), "new")

  d <- dataType(
    targetId = idReferenceType("ACTB"),
    cq = 20
  )

  data <- rdmlKeyedList(
    list(d),
    key = "targetId"
  )

  testthat::expect_identical(names(data), "ACTB")
  testthat::expect_true(S7::S7_inherits(data$ACTB, dataType))
})


testthat::test_that("sample target-aware properties are keyed", {
  sample <- sampleType(
    id = idType("S1"),
    type = list(
      sampleTargetType(
        targetId = idReferenceType("ACTB"),
        sampleType = sampleTypeType("unkn")
      )
    ),
    quantity = list(
      quantityType(
        targetId = idReferenceType("ACTB"),
        value = 1,
        unit = quantityUnitType("ng")
      )
    )
  )

  testthat::expect_identical(names(sample$type), "ACTB")
  testthat::expect_identical(names(sample$quantity), "ACTB")
})


testthat::test_that("cDNA priming method is an enum", {
  cDNA <- cdnaSynthesisMethodType(
    primingMethod = primingMethodType("random")
  )

  testthat::expect_identical(
    as.character(cDNA$primingMethod),
    "random"
  )
})


testthat::test_that("ADP and MDP survive XML serialization", {
  d <- dataType(
    targetId = idReferenceType("EvaGreen"),
    cq = 27.8,
    adp = dpAmpCurveType(
      data.table::data.table(
        cyc = 1:2,
        tmp = c(65, 65),
        fluor = c(1.1, 2.2)
      )
    ),
    mdp = dpMeltingCurveType(
      data.table::data.table(
        tmp = c(70, 71),
        fluor = c(10, 9)
      )
    )
  )

  xml <- .rdmlXmlNode(d, "data")

  testthat::expect_length(xml, 1L)
  testthat::expect_true(grepl("<adp>", xml, fixed = TRUE))
  testthat::expect_true(grepl("<mdp>", xml, fixed = TRUE))
})
