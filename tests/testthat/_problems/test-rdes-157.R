# Extracted from test-rdes.R:157

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
      "Well\tSample\tSample Type\tTarget\tTarget Type\tDye\tCq\t1",
      "17\ts1\tunkn\tACTB\ttoi\tEvaGreen\t\t5"
    )
  )
x <- readRDML(
    path,
    format = "rdes",
    expId = "exp1",
    runId = "run1"
  )
