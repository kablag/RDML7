# Extracted from test-rdes.R:116

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
amp <- tempfile(
    fileext = ".tsv"
  )
melt <- tempfile(
    fileext = ".tsv"
  )
.writeRdesFixture(
    amp,
    c(
      "Well\tSample\tSample Type\tTarget\tTarget Type\tDye\tCq\t1\t2",
      "A1\ts1\tunkn\tACTB\ttoi\tEvaGreen\t21.2\t10\t30"
    )
  )
.writeRdesFixture(
    melt,
    c(
      "Well\tSample\tSample Type\tTarget\tTarget Type\tDye\tTm\t70\t71\t72",
      "A1\ts1\tunkn\tACTB\ttoi\tEvaGreen\t71.5\t100\t120\t80"
    )
  )
x <- readRDML(
    amp,
    format = "rdes",
    companionFile = melt,
    expId = "exp1",
    runId = "run1"
  )
