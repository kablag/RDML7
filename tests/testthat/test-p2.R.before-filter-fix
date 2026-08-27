.make_test_rdml <- function(target, dye, fdata_name, fluor = c(1, 2, 3)) {
  x <- rdmlType(version = "1.2")

  fdata <- data.table::data.table(
    cyc = 1:3
  )
  fdata[[fdata_name]] <- fluor

  description <- data.table::data.table(
    fdata.name = fdata_name,
    exp.id = "exp1",
    run.id = "run1",
    react.id = "1",
    sample = "S1",
    target = target,
    target.dyeId = dye,
    sample.type = "unkn"
  )

  SetFData(x, fdata, description, fdata.type = "adp")
}


testthat::test_that("S7 dollar accessor exposes nested keyed collections", {
  x <- .make_test_rdml("ACTB", "FAM", "actb")

  testthat::expect_identical(names(x$experiment), "exp1")
  testthat::expect_identical(names(x$experiment$exp1$run), "run1")
  testthat::expect_identical(names(x$experiment$exp1$run$run1$react), "1")
  testthat::expect_identical(
    names(x$experiment$exp1$run$run1$react$`1`$data),
    "ACTB"
  )
})


testthat::test_that("AsTable default fluorescence names are stable and unique", {
  x <- .make_test_rdml("ACTB", "FAM", "actb")
  tbl <- AsTable(x)

  testthat::expect_false(anyNA(tbl$exp.id))
  testthat::expect_false(anyNA(tbl$run.id))
  testthat::expect_false(anyNA(tbl$target))
  testthat::expect_identical(anyDuplicated(tbl$fdata.name), 0L)
  testthat::expect_match(tbl$fdata.name[[1]], "^exp1_run1_")
})


testthat::test_that("GetFData long output retains requested rows with no points", {
  x <- .make_test_rdml("ACTB", "FAM", "actb")
  request <- AsTable(x)

  out <- GetFData(
    x,
    request = request,
    dp.type = "mdp",
    long.table = TRUE
  )

  testthat::expect_equal(nrow(out), 1L)
  testthat::expect_identical(out$fdata.name, request$fdata.name)
  testthat::expect_true(is.na(out$fluor[[1]]))
})


testthat::test_that("GetFData preserves amplification temperature when stored", {
  d <- dataType(
    targetId = idReferenceType("ACTB"),
    adp = dpAmpCurveType(
      data.table::data.table(
        cyc = 1:3,
        tmp = c(65, 65, 65),
        fluor = c(1, 2, 3)
      )
    )
  )

  out <- GetFData(d, "adp")
  testthat::expect_identical(names(out), c("cyc", "tmp", "fluor"))
})


testthat::test_that("MergeRDMLs merges targets within the same react", {
  x1 <- .make_test_rdml("ACTB", "FAM", "actb")
  x2 <- .make_test_rdml("GAPDH", "HEX", "gapdh", c(4, 5, 6))

  merged <- MergeRDMLs(list(x1, x2))
  data <- merged$experiment$exp1$run$run1$react$`1`$data

  testthat::expect_setequal(names(data), c("ACTB", "GAPDH"))
})
