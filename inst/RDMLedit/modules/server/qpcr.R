rdml7QpcrServer <- function(serverEnv) {
  if (!is.environment(serverEnv)) {
    stop("serverEnv must be an environment.", call. = FALSE)
  }

  evalq({
# ---------------------------------------------------------------------------
# qPCR
# ---------------------------------------------------------------------------

qPCRCatalog <- reactive({
  req(
    values$rdml
  )

  catalog <- editor_curve_catalog(
    values$rdml,
    "adp"
  )

  if (
    length(values$hookResults) &&
    nrow(catalog)
  ) {
    catalog$hook <- vapply(
      catalog$curveKey,
      function(key) {
        result <- values$hookResults[[key]]

        if (is.null(result)) {
          NA_character_
        } else {
          as.character(
            result$hook
          )
        }
      },
      character(1)
    )
  } else {
    catalog$hook <- rep(
      NA_character_,
      nrow(catalog)
    )
  }

  editor_curve_table_stats(
    catalog
  )
})

observe({
  catalog <- qPCRCatalog()

  experiments <- unique(
    catalog$expId
  )

  current_exp <- isolate(
    input$showqPCRExperiment
  )

  if (
    !length(current_exp) ||
    !(current_exp %in% experiments)
  ) {
    current_exp <- if (
      length(experiments)
    ) {
      experiments[[1L]]
    } else {
      ""
    }
  }

  updateSelectInput(
    session,
    "showqPCRExperiment",
    choices = experiments,
    selected = current_exp
  )
})

observe({
  catalog <- qPCRCatalog()

  exp_id <- input$showqPCRExperiment

  runs <- unique(
    catalog$runId[
      catalog$expId %in% exp_id
    ]
  )

  current_run <- isolate(
    input$showqPCRRun
  )

  if (
    !length(current_run) ||
    !(current_run %in% runs)
  ) {
    current_run <- if (
      length(runs)
    ) {
      runs[[1L]]
    } else {
      ""
    }
  }

  updateSelectInput(
    session,
    "showqPCRRun",
    choices = runs,
    selected = current_run
  )
})

observe({
  catalog <- qPCRCatalog()

  filtered <- editor_subset_catalog(
    catalog,
    experiment = input$showqPCRExperiment,
    run = input$showqPCRRun
  )

  targets <- unique(
    filtered$target
  )
  updateSelectInput(
    session,
    "showTargetsadp",
    choices = targets,
    selected = {
      selected <- isolate(
        input$showTargetsadp
      )

      if (
        is.null(selected) ||
        !length(selected)
      ) {
        targets
      } else {
        intersect(
          selected,
          targets
        )
      }
    }
  )
})

output$thLevelsUI <- renderUI({
  catalog <- qPCRCatalog()

  targets <- unique(
    catalog$target
  )

  if (!length(targets)) {
    return(NULL)
  }

  scale_text <- if (
    isTRUE(input$logScale)
  ) {
    " (log scale)"
  } else {
    ""
  }

  tagList(
    lapply(
      targets,
      function(target) {
        id <- paste0(
          "thLevel_",
          make.names(
            target
          )
        )
        current_value <- isolate(input[[id]])

        if (is.null(current_value) || !length(current_value)) {
          current_value <- values$thresholds[[target]]
        }

        if (is.null(current_value) || !length(current_value)) {
          current_value <- 0
        }

        numericInput(
          id,
          HTML(
            sprintf(
              "Threshold <b>%s</b>%s",
              target,
              scale_text
            )
          ),
          value = current_value,
          step = 0.01
        )
      }
    )
  )
})

thresholdForTarget <- function(target) {
  id <- paste0(
    "thLevel_",
    make.names(
      target
    )
  )

  value <- input[[id]]

  if (
    is.null(value) ||
    !length(value)
  ) {
    value <- values$thresholds[[target]]
  }

  if (is.null(value) || !length(value)) {
    value <- 0
  }

  values$thresholds[[target]] <- as.numeric(value)

  if (isTRUE(input$logScale)) {
    10 ^ as.numeric(
      value
    )
  } else {
    as.numeric(
      value
    )
  }
}

preprocessQpcr <- function() {
    req(
      values$rdml
    )

    catalog <- editor_curve_catalog(
      values$rdml,
      "adp"
    )

    if (!nrow(catalog)) {
      return()
    }

    if (!isTRUE(input$preprocessqPCR)) {
      restoreCurves(
        "adp"
      )
      return()
    }

    withProgress(
      message = "Preprocessing qPCR curves",
      value = 0,
      {
        for (i in seq_len(nrow(catalog))) {
          meta <- catalog[i, , drop = FALSE]
          backupCurve(
            meta,
            "adp"
          )

          raw_store <- activeRawStore(
            "adp"
          )

          raw_points <- raw_store[[meta$curveKey[[1L]]]]

          processed <- runSafe(
            editor_preprocess_adp(
              raw_points,
              smoothing = input$smoothqPCRmethod != "none",
              smoothing_method = if (
                input$smoothqPCRmethod == "none"
              ) {
                "savgol"
              } else {
                input$smoothqPCRmethod
              },
              normalization_method = input$normqPCRmethod
            )
          )

          if (!is.null(processed)) {
            values$rdml <- editor_set_curve_points(
              values$rdml,
              meta$expId[[1L]],
              meta$runId[[1L]],
              meta$reactId[[1L]],
              meta$target[[1L]],
              "adp",
              processed
            )
          }

          incProgress(
            1 / nrow(catalog)
          )
        }
      }
    )

    commitActive()
}

observeEvent(
  input$restoreRawAdpBtn,
  {
    restoreCurves(
      "adp"
    )

    updateCheckboxInput(
      session,
      "preprocessqPCR",
      value = FALSE
    )
  }
)

calculateQpcrCq <- function(thresholds = NULL) {
    req(
      values$rdml
    )

    if (
      is.null(input$cqMethod) ||
      input$cqMethod == "none"
    ) {
      return()
    }

    catalog <- editor_curve_catalog(
      values$rdml,
      "adp"
    )

    if (!nrow(catalog)) {
      return()
    }

    withProgress(
      message = "Calculating Cq",
      value = 0,
      {
        for (i in seq_len(nrow(catalog))) {
          meta <- catalog[i, , drop = FALSE]

          points <- editor_get_curve_points(
            values$rdml,
            meta$expId[[1L]],
            meta$runId[[1L]],
            meta$reactId[[1L]],
            meta$target[[1L]],
            "adp"
          )

          result <- runSafe(
            editor_calc_cq(
              points,
              method = input$cqMethod,
              threshold = if (
                !is.null(thresholds) &&
                meta$target[[1L]] %in% names(thresholds)
              ) {
                thresholds[[meta$target[[1L]]]]
              } else {
                thresholdForTarget(meta$target[[1L]])
              },
              auto_threshold = isTRUE(
                input$autoThLevel
              )
            )
          )

          if (!is.null(result)) {
            values$rdml <- editor_set_data_values(
              values$rdml,
              meta$expId[[1L]],
              meta$runId[[1L]],
              meta$reactId[[1L]],
              meta$target[[1L]],
              list(
                cq = result$cq,
                quantFluor = result$quantFluor
              )
            )
          }

          incProgress(
            1 / nrow(catalog)
          )
        }
      }
    )

    commitActive()
}

detectQpcrHook <- function() {
    req(
      values$rdml
    )

    values$hookResults <- list()

    if (
      is.null(input$hookMethod) ||
      input$hookMethod == "none"
    ) {
      return()
    }

    catalog <- editor_curve_catalog(
      values$rdml,
      "adp"
    )

    withProgress(
      message = "Detecting hook effect",
      value = 0,
      {
        for (i in seq_len(nrow(catalog))) {
          meta <- catalog[i, , drop = FALSE]

          points <- editor_get_curve_points(
            values$rdml,
            meta$expId[[1L]],
            meta$runId[[1L]],
            meta$reactId[[1L]],
            meta$target[[1L]],
            "adp"
          )

          result <- runSafe(
            editor_detect_hook(
              points,
              input$hookMethod
            )
          )

          if (!is.null(result)) {
            values$hookResults[[meta$curveKey[[1L]]]] <- result
          }

          incProgress(
            1 / max(
              1,
              nrow(catalog)
            )
          )
        }
      }
    )
}

observeEvent(
  input$recalcQpcrBtn,
  {
    req(values$rdml)

    catalog <- editor_curve_catalog(values$rdml, "adp")
    thresholds <- setNames(
      vapply(
        unique(catalog$target),
        thresholdForTarget,
        numeric(1)
      ),
      unique(catalog$target)
    )

    preprocessQpcr()
    detectQpcrHook()
    calculateQpcrCq(thresholds)
  },
  ignoreInit = TRUE
)


qPCRSelectedPositions <- reactive({
  selected <- input$showqPCRPositionsFallback

  if (
    is.null(selected) ||
    !length(selected)
  ) {
    character()
  } else {
    as.character(selected)
  }
})

meltingSelectedPositions <- reactive({
  selected <- input$showMeltingPositionsFallback

  if (
    is.null(selected) ||
    !length(selected)
  ) {
    character()
  } else {
    as.character(selected)
  }
})

output$qPCRPlateUI <- renderUI({
  catalog <- qPCRCatalog()

  filtered <- editor_subset_catalog(
    catalog,
    experiment = input$showqPCRExperiment,
    run = input$showqPCRRun
  )

  if (!nrow(filtered)) {
    return(NULL)
  }

  selectInput(
    "showqPCRPositionsFallback",
    "Plate positions",
    choices = unique(filtered$position),
    multiple = TRUE
  )
})

output$meltingPlateUI <- renderUI({
  catalog <- meltingCatalog()

  filtered <- editor_subset_catalog(
    catalog,
    experiment = input$showMeltingExperiment,
    run = input$showMeltingRun
  )

  if (!nrow(filtered)) {
    return(NULL)
  }

  selectInput(
    "showMeltingPositionsFallback",
    "Plate positions",
    choices = unique(filtered$position),
    multiple = TRUE
  )
})

qPCRFilteredCatalog <- reactive({
  catalog <- qPCRCatalog()

  editor_subset_catalog(
    catalog,
    experiment = input$showqPCRExperiment,
    run = input$showqPCRRun,
    targets = input$showTargetsadp,
    positions = qPCRSelectedPositions()
  )
})

qPCRLong <- reactive({
  catalog <- qPCRFilteredCatalog()

  if (!nrow(catalog)) {
    return(
      data.frame()
    )
  }

  editor_curve_long_from_catalog(
    values$rdml,
    catalog,
    "adp"
  )
})

output$qPCRPlot <- plotly::renderPlotly({
  data <- qPCRLong()

  req(
    nrow(data) > 0L
  )

  data$curve <- paste(
    data$position,
    data$target,
    sep = " / "
  )

  y <- data$fluor

  if (isTRUE(input$logScale)) {
    y[
      !is.finite(y) |
        y <= 0
    ] <- NA_real_
    y <- log10(y)
  }

  data$plotFluor <- y

  color_by <- input$colorqPCRby

  color_formula <- if (
    !is.null(color_by) &&
    color_by != "none" &&
    color_by %in% names(data)
  ) {
    stats::as.formula(
      paste0(
        "~",
        color_by
      )
    )
  } else {
    NULL
  }

  dash_by <- input$shapeqPCRby

  dash_formula <- if (
    !is.null(dash_by) &&
    dash_by != "none" &&
    dash_by %in% names(data)
  ) {
    stats::as.formula(
      paste0(
        "~",
        dash_by
      )
    )
  } else {
    NULL
  }

  p <- plotly::plot_ly(
    data = data,
    x = ~cyc,
    y = ~plotFluor,
    split = ~curve,
    color = color_formula,
    linetype = dash_formula,
    type = "scatter",
    mode = "lines",
    text = ~paste0(
      "Experiment: ",
      expId,
      "<br>Run: ",
      runId,
      "<br>Position: ",
      position,
      "<br>Sample: ",
      sample,
      "<br>Target: ",
      target,
      "<br>Cq: ",
      cq
    ),
    hoverinfo = "text"
  )

  catalog <- qPCRFilteredCatalog()

  if (
    input$showCq %in% c(
      "yes",
      "mean"
    ) &&
    nrow(catalog)
  ) {
    cq_values <- if (
      input$showCq == "yes"
    ) {
      catalog$cq
    } else {
      catalog$cqMean
    }

    markers <- data.frame(
      x = cq_values,
      y = catalog$quantFluor,
      label = paste(
        catalog$sample,
        catalog$target,
        sep = " / "
      )
    )

    markers <- markers[
      is.finite(markers$x) &
        is.finite(markers$y),
      ,
      drop = FALSE
    ]

    if (isTRUE(input$logScale)) {
      markers$y[
        markers$y <= 0
      ] <- NA_real_
      markers$y <- log10(
        markers$y
      )
      markers <- markers[
        is.finite(markers$y),
        ,
        drop = FALSE
      ]
    }

    if (nrow(markers)) {
      p <- plotly::add_markers(
        p,
        data = markers,
        x = ~x,
        y = ~y,
        text = ~label,
        inherit = FALSE,
        name = "Cq"
      )
    }
  }

  p <- plotly::layout(
    p,
    xaxis = list(
      title = "Cycle"
    ),
    yaxis = list(
      title = if (
        isTRUE(input$logScale)
      ) {
        "log10 fluorescence"
      } else {
        "Fluorescence"
      }
    )
  )

  editor_compact_plotly_legend(
    p,
    color_by = color_by,
    line_by = dash_by
  )
})

output$qPCRDt <- DT::renderDT({
  catalog <- qPCRFilteredCatalog()

  DT::datatable(
    catalog[
      ,
      setdiff(
        names(catalog),
        "curveKey"
      ),
      drop = FALSE
    ],
    rownames = FALSE,
    filter = "top",
    selection = "multiple",
    options = list(
      pageLength = 25,
      scrollX = TRUE
    )
  )
})


  }, envir = serverEnv)

  invisible(NULL)
}
