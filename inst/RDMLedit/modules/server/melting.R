rdml7MeltingServer <- function(serverEnv) {
  if (!is.environment(serverEnv)) {
    stop("serverEnv must be an environment.", call. = FALSE)
  }

  evalq({
# ---------------------------------------------------------------------------
# Melting
# ---------------------------------------------------------------------------

meltingCatalog <- reactive({
  req(
    values$rdml
  )

  editor_curve_catalog(
    values$rdml,
    "mdp"
  )
})

observe({
  catalog <- meltingCatalog()

  experiments <- unique(
    catalog$expId
  )

  current_exp <- isolate(
    input$showMeltingExperiment
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
    "showMeltingExperiment",
    choices = experiments,
    selected = current_exp
  )
})

observe({
  catalog <- meltingCatalog()

  runs <- unique(
    catalog$runId[
      catalog$expId %in% input$showMeltingExperiment
    ]
  )

  current_run <- isolate(
    input$showMeltingRun
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
    "showMeltingRun",
    choices = runs,
    selected = current_run
  )
})

observe({
  catalog <- meltingCatalog()

  filtered <- editor_subset_catalog(
    catalog,
    experiment = input$showMeltingExperiment,
    run = input$showMeltingRun
  )

  targets <- unique(
    filtered$target
  )
  updateSelectInput(
    session,
    "showTargetsmdp",
    choices = targets,
    selected = {
      selected <- isolate(
        input$showTargetsmdp
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

meltingProcessingTrigger <- reactive({
  list(
    preprocess = input$preprocessMelting,
    background = input$bgAdjMelting,
    range = input$bgRangeMelting,
    minmax = input$minMaxMelting,
    df = input$dfFactMelting
  )
})

observeEvent(
  meltingProcessingTrigger(),
  {
    req(
      values$rdml
    )

    catalog <- editor_curve_catalog(
      values$rdml,
      "mdp"
    )

    if (!nrow(catalog)) {
      return()
    }

    if (!isTRUE(input$preprocessMelting)) {
      restoreCurves(
        "mdp"
      )
      return()
    }

    withProgress(
      message = "Preprocessing melting curves",
      value = 0,
      {
        for (i in seq_len(nrow(catalog))) {
          meta <- catalog[i, , drop = FALSE]

          backupCurve(
            meta,
            "mdp"
          )

          raw_store <- activeRawStore(
            "mdp"
          )

          raw_points <- raw_store[[meta$curveKey[[1L]]]]

          processed <- runSafe(
            editor_preprocess_mdp(
              raw_points,
              background_adjust = isTRUE(
                input$bgAdjMelting
              ),
              background_range = input$bgRangeMelting,
              min_max = isTRUE(
                input$minMaxMelting
              ),
              df_fact = input$dfFactMelting
            )
          )

          if (!is.null(processed)) {
            values$rdml <- editor_set_curve_points(
              values$rdml,
              meta$expId[[1L]],
              meta$runId[[1L]],
              meta$reactId[[1L]],
              meta$target[[1L]],
              "mdp",
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
  },
  ignoreInit = TRUE
)

observeEvent(
  input$restoreRawMdpBtn,
  {
    restoreCurves(
      "mdp"
    )

    updateCheckboxInput(
      session,
      "preprocessMelting",
      value = FALSE
    )
  }
)

meltingFilteredCatalog <- reactive({
  catalog <- meltingCatalog()

  editor_subset_catalog(
    catalog,
    experiment = input$showMeltingExperiment,
    run = input$showMeltingRun,
    targets = input$showTargetsmdp,
    positions = meltingSelectedPositions()
  )
})

meltingLong <- reactive({
  catalog <- meltingFilteredCatalog()

  if (!nrow(catalog)) {
    return(
      data.frame()
    )
  }

  data <- editor_curve_long_from_catalog(
    values$rdml,
    catalog,
    "mdp"
  )

  if (!nrow(data)) {
    return(data)
  }

  data$curve <- paste(
    data$position,
    data$target,
    sep = " / "
  )

  split_data <- split(
    data,
    data$curve
  )

  split_data <- lapply(
    split_data,
    function(part) {
      part$fluorDeriv <- editor_melting_derivative(
        data.frame(
          tmp = part$tmp,
          fluor = part$fluor
        )
      )
      part
    }
  )

  do.call(
    rbind,
    split_data
  )
})

output$meltingPlot <- plotly::renderPlotly({
  data <- meltingLong()

  req(
    nrow(data) > 0L
  )

  color_by <- input$colorMeltingBy

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

  shape_by <- input$shapeMeltingBy

  shape_formula <- if (
    !is.null(shape_by) &&
    shape_by != "none" &&
    shape_by %in% names(data)
  ) {
    stats::as.formula(
      paste0(
        "~",
        shape_by
      )
    )
  } else {
    NULL
  }

  p1 <- plotly::plot_ly(
    data = data,
    x = ~tmp,
    y = ~fluor,
    split = ~curve,
    color = color_formula,
    linetype = shape_formula,
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
      target
    ),
    hoverinfo = "text"
  )

  p2 <- plotly::plot_ly(
    data = data,
    x = ~tmp,
    y = ~fluorDeriv,
    split = ~curve,
    color = color_formula,
    linetype = shape_formula,
    type = "scatter",
    mode = "lines",
    showlegend = FALSE
  )

  p <- plotly::subplot(
    p1,
    p2,
    nrows = 2,
    shareX = TRUE,
    titleY = TRUE
  )

  editor_compact_plotly_legend(
    p,
    color_by = color_by,
    line_by = shape_by,
    keep_names = character()
  )
})

output$meltingDt <- DT::renderDT({
  catalog <- meltingFilteredCatalog()

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
