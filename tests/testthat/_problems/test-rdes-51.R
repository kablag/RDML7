# Extracted from test-rdes.R:51

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "RDML", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
.writeRdesFixture <- function(
    path,
    lines) {

  text <- paste0(
    paste(
      lines,
      collapse = "\n"
    ),
    "\n"
  )

  con <- file(
    path,
    open = "wb"
  )

  on.exit(
    close(con),
    add = TRUE
  )

  writeBin(
    charToRaw(
      enc2utf8(text)
    ),
    con
  )

  invisible(path)
}

# test -------------------------------------------------------------------------
path <- tempfile(
    fileext = ".tsv"
  )
.writeRdesFixture(
    path,
    c(
      "Well\tSample\tSample Type\tTarget\tTarget Type\tDye\tCq\t1\t2\t3",
      "A1\tsample1\tunkn\tACTB\tref\tEvaGreen\t20.5\t10\t20\t40"
    )
  )
expect_identical(
    rdmlDetectFormat(path),
    "rdes"
  )
