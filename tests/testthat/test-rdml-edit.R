test_that("bundled RDML editor renders curves and edits metadata", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("shinythemes")
  skip_if_not_installed("plotly")
  skip_if_not_installed("DT")

  suppressWarnings(
    suppressPackageStartupMessages(
      library("shiny", character.only = TRUE)
    )
  )

  appDir <- system.file("RDMLedit", package = "RDML7")
  expect_true(nzchar(appDir))

  helperEnv <- new.env(parent = globalenv())
  sys.source(
    file.path(appDir, "helpers.R"),
    envir = helperEnv
  )
  sys.source(
    file.path(appDir, "analysis-helpers.R"),
    envir = helperEnv
  )
  for (module in c("experiment.R", "qpcr.R", "melting.R")) {
    sys.source(
      file.path(appDir, "modules", "ui", module),
      envir = helperEnv
    )
  }
  meltingHtml <- htmltools::renderTags(
    helperEnv$rdml7MeltingPanel()
  )$html
  expect_lt(
    regexpr("meltingPlot", meltingHtml, fixed = TRUE)[[1L]],
    regexpr("meltingPlateUI", meltingHtml, fixed = TRUE)[[1L]]
  )
  expect_identical(
    helperEnv$editor_validate_cq(5, 20, 1:4),
    list(cq = NA_real_, quantFluor = NA_real_)
  )
  expect_identical(
    helperEnv$editor_validate_cq(4, 20, 1:4),
    list(cq = 4, quantFluor = 20)
  )

  if (requireNamespace("MBmca", quietly = TRUE)) {
    meltPoints <- data.frame(
      tmp = seq(60, 79.5, by = 0.5),
      fluor = sin(seq_len(40))
    )
    expect_warning(
      smoothedMelt <- helperEnv$editor_preprocess_mdp(
        meltPoints,
        df_fact = 1.1
      ),
      NA
    )
    expect_equal(nrow(smoothedMelt), 40L)
  }

  legendData <- data.frame(
    cyc = rep(1:2, 3),
    fluor = 1:6,
    curve = rep(c("A1", "A2", "B1"), each = 2),
    target = rep(c("X", "X", "Y"), each = 2)
  )
  legendPlot <- plotly::plot_ly(
    legendData,
    x = ~cyc,
    y = ~fluor,
    split = ~curve,
    color = ~target,
    type = "scatter",
    mode = "lines"
  )
  legendPlot <- helperEnv$editor_compact_plotly_legend(
    legendPlot,
    color_by = "target"
  )
  expect_identical(
    vapply(legendPlot$x$data, `[[`, character(1), "name"),
    c("X", "X", "Y")
  )
  expect_identical(
    vapply(legendPlot$x$data, `[[`, logical(1), "showlegend"),
    c(TRUE, FALSE, TRUE)
  )

  x <- rdmlType()
  description <- data.table::data.table(
    fdataName = "amp",
    expId = "exp1",
    runId = "run1",
    reactId = "A1",
    sample = "sample1",
    sampleType = "unkn",
    target = "ACTB",
    targetDyeId = "FAM"
  )
  x <- setFData(
    x,
    data.table::data.table(
      cyc = 1:4,
      amp = c(10, 20, 45, 90)
    ),
    description,
    fdataType = "adp"
  )

  description$fdataName <- "melt"
  x <- setFData(
    x,
    data.table::data.table(
      tmp = 70:73,
      melt = c(100, 90, 60, 20)
    ),
    description,
    fdataType = "mdp"
  )

  shiny::testServer(appDir, {
    session$setInputs(
      cqMethod = "none",
      autoThLevel = TRUE,
      hookMethod = "none",
      preprocessqPCR = FALSE,
      smoothqPCRmethod = "none",
      baselineqPCR = FALSE,
      colorqPCRby = "none",
      shapeqPCRby = "none",
      showCq = "none",
      logScale = FALSE,
      preprocessMelting = FALSE,
      colorMeltingBy = "none",
      shapeMeltingBy = "none"
    )

    values$rdml <- x
    values$RDMLs <- list(smoke = x)
    session$setInputs(
      rdmlFileSlct = "smoke",
      showqPCRExperiment = "exp1",
      showqPCRRun = "run1",
      showMeltingExperiment = "exp1",
      showMeltingRun = "run1"
    )
    session$flushReact()

    expect_equal(nrow(qPCRCatalog()), 1L)
    expect_equal(nrow(meltingCatalog()), 1L)
    expect_true(any(nchar(output$qPCRPlateUI) > 0L))
    expect_true(any(nchar(output$meltingPlateUI) > 0L))
    expect_match(paste(output$meltingPlateUI, collapse = ""), "A01")
    expect_true(any(nchar(output$qPCRPlot) > 0L))
    expect_true(any(nchar(output$meltingPlot) > 0L))

    session$setInputs(showqPCRPositionsFallback = "A01")
    session$flushReact()
    expect_identical(qPCRSelectedPositions(), "A01")
    expect_equal(nrow(qPCRFilteredCatalog()), 1L)

    values$rdml <- helperEnv$editor_set_data_values(
      values$rdml,
      "exp1",
      "run1",
      "A1",
      "ACTB",
      list(cq = 2, quantFluor = 20)
    )
    session$setInputs(cqMethod = "th")
    session$flushReact()
    expect_identical(
      helperEnv$editor_get_data(
        values$rdml,
        "exp1",
        "run1",
        "A1",
        "ACTB"
      )$cq,
      2
    )

    session$setInputs(cqMethod = "none", recalcQpcrBtn = 1)
    session$flushReact()
    expect_identical(
      helperEnv$editor_get_data(
        values$rdml,
        "exp1",
        "run1",
        "A1",
        "ACTB"
      )$cq,
      2
    )

    session$setInputs(
      sampleSlct = "sample1",
      sampleIdText = "sample1",
      sampleDescriptionText = "edited",
      sampleTypeSlct = "unkn",
      sampleInterRunCalibratorChk = FALSE,
      sampleCalibratorSampleChk = FALSE,
      saveSampleBtn = 1
    )
    session$flushReact()

    sampleObject <- values$rdml$sample[["sample1"]]
    expect_identical(sampleObject$description, "edited")
    expect_true(S7::S7_inherits(
      sampleObject$type[[1L]],
      sampleTargetType
    ))
  })
})

test_that("RDML editor opens the bundled RDES amplification example", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("shinythemes")
  skip_if_not_installed("plotly")
  skip_if_not_installed("DT")
  skip_if_not_installed("chipPCR")

  suppressWarnings(
    suppressPackageStartupMessages(
      library("shiny", character.only = TRUE)
    )
  )

  appDir <- system.file("RDMLedit", package = "RDML7")
  rdesFile <- system.file(
    "extdata",
    "RDES_v1_0_example_amplification.tsv",
    package = "RDML7"
  )
  uploadFile <- tempfile()
  expect_true(file.copy(rdesFile, uploadFile))
  helperEnv <- new.env(parent = globalenv())
  sys.source(
    file.path(appDir, "helpers.R"),
    envir = helperEnv
  )
  sys.source(
    file.path(appDir, "analysis-helpers.R"),
    envir = helperEnv
  )

  shiny::testServer(appDir, {
    session$setInputs(
      cqMethod = "none",
      autoThLevel = TRUE,
      hookMethod = "none",
      preprocessqPCR = FALSE,
      smoothqPCRmethod = "none",
      baselineqPCR = FALSE,
      colorqPCRby = "none",
      shapeqPCRby = "none",
      showCq = "none",
      logScale = FALSE,
      preprocessMelting = FALSE,
      colorMeltingBy = "none",
      shapeMeltingBy = "none"
    )
    session$setInputs(
      rdmlFiles = data.frame(
        name = basename(rdesFile),
        size = file.info(rdesFile)$size,
        type = "text/tab-separated-values",
        datapath = uploadFile
      )
    )
    session$flushReact()

    expect_length(values$RDMLs, 1L)
    expect_s7_class(values$rdml, rdmlType)
    expect_equal(nrow(qPCRCatalog()), 90L)
    expect_true(any(nchar(output$qPCRPlateUI) > 0L))
    expect_true(any(nchar(output$qPCRPlot) > 0L))

    session$setInputs(
      cqMethod = "th",
      autoThLevel = FALSE,
      preprocessqPCR = TRUE,
      smoothqPCRmethod = "savgol",
      normqPCRmethod = "none",
      `thLevel_Exon.1` = 500,
      recalcQpcrBtn = 1
    )
    session$flushReact()

    expect_identical(values$thresholds[["Exon 1"]], 500)
    expect_match(
      paste(output$thLevelsUI, collapse = ""),
      "value=\\\"500\\\""
    )

    a12 <- qPCRCatalog()
    a12 <- a12[a12$reactId == "A12", , drop = FALSE]
    a12Points <- helperEnv$editor_get_curve_points(
      values$rdml,
      a12$expId[[1L]],
      a12$runId[[1L]],
      a12$reactId[[1L]],
      a12$target[[1L]],
      "adp"
    )
    a12Data <- helperEnv$editor_get_data(
      values$rdml,
      a12$expId[[1L]],
      a12$runId[[1L]],
      a12$reactId[[1L]],
      a12$target[[1L]]
    )
    expect_lt(max(a12Points$fluor), 500)
    expect_true(is.na(a12Data$cq))

    originalTypes <- values$rdml$sample[["gDNA"]]$type
    expect_length(originalTypes, 5L)

    session$setInputs(
      sampleSlct = "gDNA",
      sampleIdText = "gDNA",
      sampleDescriptionText = "edited RDES sample",
      sampleTypeSlct = "unkn",
      sampleInterRunCalibratorChk = FALSE,
      sampleCalibratorSampleChk = FALSE,
      saveSampleBtn = 1
    )
    session$flushReact()

    editedSample <- values$rdml$sample[["gDNA"]]
    expect_identical(editedSample$description, "edited RDES sample")
    expect_length(editedSample$type, 5L)
    expect_identical(
      vapply(editedSample$type, function(type) type$targetId$id, character(1)),
      vapply(originalTypes, function(type) type$targetId$id, character(1))
    )
  })
})
