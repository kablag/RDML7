# Extracted from test-rdes.R:409

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
path <- tempfile(fileext = ".tsv")
header <- paste(
    c(
      "Well", "Sample", "Sample Type", "Target", "Target Type", "Dye", "Cq",
      as.character(3:40)
    ),
    collapse = "\t"
  )
row <- paste(
    c(
      "A1", "gDNA", "unkn", "Exon 1", "toi", "SYBRGreen I", "-1.0",
      as.character(seq_len(38))
    ),
    collapse = "\t"
  )
.writeRdesFixture(
    path,
    c(
      paste0(header, "\t"),
      paste0(row, "\t")
    )
  )
x <- rdmlRead(
    path,
    format = "rdes",
    expId = "exp1",
    runId = "run1"
  )
