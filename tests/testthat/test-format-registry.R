test_that("built-in format registry is populated", {
  fmts <- rdmlFormats()

  expect_true(
    all(
      c(
        "rdml",
        "rdml-xml",
        "dtprime",
        "abi",
        "rotorgene",
        "excel",
        "csv",
        "fqd",
        "roche-lc96"
      ) %in% fmts$format
    )
  )

  expect_true(
    fmts[fmts$format == "rdml", "write"]
  )

  expect_false(
    fmts[fmts$format == "dtprime", "write"]
  )
})


test_that("custom extension readers can be registered", {
  name <- "test-dummy-reader"

  on.exit(
    try(
      rdmlUnregisterFormat(name),
      silent = TRUE
    ),
    add = TRUE
  )

  rdmlRegisterFormat(
    name = name,
    extensions = "dummyqpcr",
    reader = function(filename) {
      .rdmlNewImport(
        publisher = "dummy"
      )
    }
  )

  path <- tempfile(
    fileext = ".dummyqpcr"
  )

  writeLines(
    "dummy",
    path
  )

  expect_identical(
    rdmlDetectFormat(path),
    name
  )

  obj <- rdmlRead(path)

  expect_true(
    S7::S7_inherits(
      obj,
      rdmlType
    )
  )
})


test_that("custom writers are dispatched by extension", {
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

  obj <- .rdmlNewImport(
    publisher = "dummy"
  )

  path <- tempfile(
    fileext = ".dummyout"
  )

  rdmlWrite(
    obj,
    path
  )

  expect_identical(
    readLines(path),
    "ok"
  )
})


test_that("overlapping extensions may be resolved by sniff confidence", {
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

  rdmlRegisterFormat(
    name = "test-sniff-b",
    extensions = "ambqpcr",
    reader = function(filename) {
      .rdmlNewImport("B")
    },
    sniff = function(filename) 0.9
  )

  path <- tempfile(
    fileext = ".ambqpcr"
  )

  writeLines(
    "x",
    path
  )

  expect_identical(
    rdmlDetectFormat(path),
    "test-sniff-b"
  )
})
