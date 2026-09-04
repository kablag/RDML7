# Extracted from test-rdes.R:358

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
      "Well	Sample	Sample Type	Target	Target Type	Dye	Tm	70	71",
      "A1	s1	unkn	ACTB	toi	EvaGreen	71.5;75.2	10	20"
    )
  )
x <- readRDML(
    path,
    format = "rdes",
    expId = "exp1",
    runId = "run1"
  )
