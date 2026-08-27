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


test_that("RDES amplification import follows v1.0 layout", {

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

  x <- rdmlRead(
    fileName = path,
    format = "rdes",
    expId = "exp1",
    runId = "run1"
  )

  d <- x$experiment$exp1$
    run$run1$
    react$A1$
    data$ACTB

  expect_equal(
    d$cq,
    20.5
  )

  expect_equal(
    nrow(d$adp$fpoints),
    3L
  )

  expect_identical(
    .rdmlEnumChr(
      x$target$ACTB$type
    ),
    "ref"
  )
})


test_that("RDES companion melting file is imported into the same run", {

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

  x <- rdmlRead(
    amp,
    format = "rdes",
    companionFile = melt,
    expId = "exp1",
    runId = "run1"
  )

  d <- x$experiment$exp1$
    run$run1$
    react$A1$
    data$ACTB

  expect_true(
    .rdmlPresent(d$adp)
  )

  expect_true(
    .rdmlPresent(d$mdp)
  )

  expect_equal(
    d$meltTemp,
    71.5
  )
})


test_that("RDES numeric rotor well labels are preserved", {

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

  x <- rdmlRead(
    path,
    format = "rdes",
    expId = "exp1",
    runId = "run1"
  )

  expect_true(
    is.na(
      x$experiment$exp1$
        run$run1$
        pcrFormat
    )
  )

  tbl <- asTable(x)

  expect_identical(
    tbl$position[[1L]],
    "17"
  )
})


test_that("RDES export writes exact metadata header and LF-separated TSV", {

  x <- .rdmlNewImport(
    "test"
  )

  fdata <- data.table::data.table(
    cyc = 1:3,
    curve = c(
      10,
      20,
      40
    )
  )

  description <- data.table::data.table(
    fdataName = "curve",
    expId = "exp1",
    runId = "run1",
    reactId = "A1",
    sample = "s1",
    sampleType = "unkn",
    target = "ACTB",
    targetDyeId = "EvaGreen",
    cq = 20.5
  )

  x <- setFData(
    x,
    fdata,
    description
  )

  out <- tempfile(
    fileext = ".tsv"
  )

  rdmlWrite(
    x,
    out,
    format = "rdes",
    rdesType = "adp"
  )

  raw <- readBin(
    out,
    what = "raw",
    n = file.info(out)$size
  )

  expect_false(
    any(
      raw == as.raw(13L)
    )
  )

  lines <- readLines(
    out,
    encoding = "UTF-8"
  )

  expect_identical(
    strsplit(
      lines[[1L]],
      "\t",
      fixed = TRUE
    )[[1L]][1:7],
    c(
      "Well",
      "Sample",
      "Sample Type",
      "Target",
      "Target Type",
      "Dye",
      "Cq"
    )
  )
})


test_that("RDES both export creates separate amplification and melting files", {

  x <- .rdmlNewImport(
    "test"
  )

  adp <- data.table::data.table(
    cyc = 1:2,
    curve = c(
      1,
      2
    )
  )

  mdp <- data.table::data.table(
    tmp = c(
      70,
      71
    ),
    curve = c(
      5,
      4
    )
  )

  description <- data.table::data.table(
    fdataName = "curve",
    expId = "exp1",
    runId = "run1",
    reactId = "A1",
    sample = "s1",
    sampleType = "unkn",
    target = "ACTB",
    targetDyeId = "EvaGreen"
  )

  x <- setFData(
    x,
    adp,
    description,
    fdataType = "adp"
  )

  x <- setFData(
    x,
    mdp,
    description,
    fdataType = "mdp"
  )

  base <- tempfile(
    fileext = ".tsv"
  )

  paths <- rdmlWrite(
    x,
    base,
    format = "rdes",
    rdesType = "both"
  )

  expect_true(
    all(
      file.exists(paths)
    )
  )

  expect_true(
    grepl(
      "_amplification\\.tsv$",
      paths[["amplification"]]
    )
  )

  expect_true(
    grepl(
      "_melting\\.tsv$",
      paths[["melting"]]
    )
  )
})


test_that("RDES multiple Tm values are retained", {

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

  x <- rdmlRead(
    path,
    format = "rdes",
    expId = "exp1",
    runId = "run1"
  )

  dataObj <- x$experiment$exp1$
    run$run1$
    react$A1$
    data$ACTB

  expect_equal(
    dataObj$meltTemp,
    71.5
  )

  expect_equal(
    dataObj$meltTemps,
    c(71.5, 75.2)
  )
})


test_that("official-style RDES header starting at cycle 3 imports", {
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

  expect_equal(
    x$experiment$exp1$run$run1$react$A1$data$`Exon 1`$adp$fpoints$cyc,
    3:40
  )
})
