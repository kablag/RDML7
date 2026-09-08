test_that("registerRDMLFormat has the minimal public API", {
  expect_identical(
    names(formals(registerRDMLFormat)),
    c(
      "name",
      "extensions",
      "reader",
      "writer"
    )
  )
})


test_that("listRDMLFormats exposes only dispatch information", {
  expect_identical(
    names(listRDMLFormats()),
    c(
      "format",
      "extensions",
      "read",
      "write"
    )
  )
})


test_that("the first registered reader is the default for an extension", {
  namesToRemove <- c(
    "test-first-default",
    "test-second-default"
  )

  on.exit(
    for (name in namesToRemove) {
      try(
        unregisterRDMLFormat(name),
        silent = TRUE
      )
    },
    add = TRUE
  )

  registerRDMLFormat(
    "test-first-default",
    extensions = "defaultqpcr",
    reader = function(fileName, ...) rdmlType()
  )

  registerRDMLFormat(
    "test-second-default",
    extensions = "defaultqpcr",
    reader = function(fileName, ...) rdmlType()
  )

  expect_identical(
    detectRDMLFormat("x.defaultqpcr", "read"),
    "test-first-default"
  )

  expect_identical(
    .rdmlResolveFormat(
      "x.defaultqpcr",
      format = "test-second-default",
      operation = "read"
    )$name,
    "test-second-default"
  )
})


test_that("read and write defaults are operation-specific", {
  namesToRemove <- c(
    "test-reader-default",
    "test-writer-default"
  )

  on.exit(
    for (name in namesToRemove) {
      try(
        unregisterRDMLFormat(name),
        silent = TRUE
      )
    },
    add = TRUE
  )

  registerRDMLFormat(
    "test-reader-default",
    extensions = "opqpcr",
    reader = function(fileName, ...) rdmlType()
  )

  registerRDMLFormat(
    "test-writer-default",
    extensions = "opqpcr",
    writer = function(x, fileName, ...) invisible(fileName)
  )

  expect_identical(
    detectRDMLFormat("x.opqpcr", "read"),
    "test-reader-default"
  )

  expect_identical(
    detectRDMLFormat("x.opqpcr", "write"),
    "test-writer-default"
  )
})


test_that("built-in overlapping extensions have stable defaults", {
  expect_identical(
    detectRDMLFormat("x.csv", "read"),
    "csv"
  )

  expect_identical(
    detectRDMLFormat("x.txt", "read"),
    "fqd"
  )

  expect_identical(
    detectRDMLFormat("x.tsv", "read"),
    "rdes"
  )

  expect_identical(
    detectRDMLFormat("x.csv", "write"),
    "rdes"
  )

  expect_identical(
    detectRDMLFormat("x.txt", "write"),
    "rdes"
  )
})


test_that("files without an extension require explicit format", {
  expect_error(
    detectRDMLFormat("no-extension", "read"),
    "without an extension"
  )
})


test_that("unknown extensions fail clearly", {
  expect_error(
    detectRDMLFormat("x.unknownqpcr", "read"),
    "No registered read format"
  )
})


test_that("duplicate format names are rejected", {
  name <- "test-duplicate-name"

  on.exit(
    try(
      unregisterRDMLFormat(name),
      silent = TRUE
    ),
    add = TRUE
  )

  registerRDMLFormat(
    name,
    extensions = "dupqpcr",
    reader = function(fileName, ...) rdmlType()
  )

  expect_error(
    registerRDMLFormat(
      name,
      extensions = "dup2qpcr",
      reader = function(fileName, ...) rdmlType()
    ),
    "already registered"
  )
})


test_that("a format must provide a reader or writer", {
  expect_error(
    registerRDMLFormat(
      "test-empty-format",
      extensions = "emptyqpcr"
    ),
    "at least one"
  )
})
