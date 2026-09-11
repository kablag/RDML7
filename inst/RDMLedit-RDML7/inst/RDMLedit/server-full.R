rdml7ServerFull <- function(serverEnv) {
  if (!is.environment(serverEnv)) {
    stop("serverEnv must be an environment.", call. = FALSE)
  }

  evalq({
# ---------------------------------------------------------------------------
# Experiment / Run / React / Data editor
# ---------------------------------------------------------------------------

refreshExperimentSelectors <- function() {
  if (is.null(values$rdml)) {
    return(
      invisible(NULL)
    )
  }

  experiments <- editor_collection_names(
    values$rdml,
    "experiment"
  )

  updateSelectizeInput(
    session,
    "experimentSlct",
    choices = experiments,
    server = TRUE
  )

  updateSelectInput(
    session,
    "experimentDocumentationSlct",
    choices = editor_collection_names(
      values$rdml,
      "documentation"
    )
  )

  updateSelectInput(
    session,
    "runDocumentationSlct",
    choices = editor_collection_names(
      values$rdml,
      "documentation"
    )
  )

  updateSelectInput(
    session,
    "runExperimenterSlct",
    choices = editor_collection_names(
      values$rdml,
      "experimenter"
    )
  )

  updateSelectInput(
    session,
    "runTccSlct",
    choices = c(
      "",
      editor_collection_names(
        values$rdml,
        "thermalCyclingConditions"
      )
    )
  )

  updateSelectInput(
    session,
    "reactSampleSlct",
    choices = c(
      "",
      editor_collection_names(
        values$rdml,
        "sample"
      )
    )
  )

  updateSelectInput(
    session,
    "dataTarSlct",
    choices = c(
      "",
      editor_collection_names(
        values$rdml,
        "target"
      )
    )
  )

  invisible(NULL)
}

observe({
  values$rdml

  if (!is.null(values$rdml)) {
    refreshExperimentSelectors()
  }
})

observeEvent(
  input$experimentSlct,
  {
    req(
      values$rdml
    )

    exp_id <- input$experimentSlct

    experiment <- editor_collection_get(
      values$rdml,
      "experiment",
      exp_id
    )

    if (is.null(experiment)) {
      updateTextInput(
        session,
        "experimentIdText",
        value = exp_id
      )
      updateTextInput(
        session,
        "experimentDescriptionText",
        value = ""
      )
      updateSelectizeInput(
        session,
        "runSlct",
        choices = character(),
        server = TRUE
      )
      return()
    }

    updateTextInput(
      session,
      "experimentIdText",
      value = editor_id_chr(
        editor_prop(
          experiment,
          "id"
        )
      )
    )

    updateTextInput(
      session,
      "experimentDescriptionText",
      value = editor_display(
        editor_prop(
          experiment,
          "description"
        )
      )
    )

    documentation <- editor_list_prop(
      experiment,
      "documentation"
    )

    updateSelectInput(
      session,
      "experimentDocumentationSlct",
      selected = vapply(
        documentation,
        editor_id_chr,
        character(1)
      )
    )

    updateSelectizeInput(
      session,
      "runSlct",
      choices = names(
        editor_list_prop(
          experiment,
          "run"
        )
      ),
      server = TRUE
    )
  },
  ignoreInit = TRUE
)

observeEvent(
  input$runSlct,
  {
    req(
      values$rdml,
      input$experimentSlct
    )

    run <- editor_get_run(
      values$rdml,
      input$experimentSlct,
      input$runSlct
    )

    if (is.null(run)) {
      updateTextInput(
        session,
        "runIdText",
        value = input$runSlct
      )
      updateSelectizeInput(
        session,
        "reactSlct",
        choices = character(),
        server = TRUE
      )
      return()
    }

    updateTextInput(
      session,
      "runIdText",
      value = editor_id_chr(
        editor_prop(
          run,
          "id"
        )
      )
    )
    updateTextInput(
      session,
      "runDescriptionText",
      value = editor_display(
        editor_prop(
          run,
          "description"
        )
      )
    )
    updateTextInput(
      session,
      "runInstrumentText",
      value = editor_display(
        editor_prop(
          run,
          "instrument"
        )
      )
    )

    software <- editor_prop(
      run,
      "dataCollectionSoftware"
    )

    updateTextInput(
      session,
      "runDataCollectionSoftwareNameText",
      value = editor_display(
        editor_prop(
          software,
          "name"
        )
      )
    )
    updateTextInput(
      session,
      "runDataCollectionSoftwareVersionText",
      value = editor_display(
        editor_prop(
          software,
          "version"
        )
      )
    )
    updateTextInput(
      session,
      "runBackgroundDeterminationMethodText",
      value = editor_display(
        editor_prop(
          run,
          "backgroundDeterminationMethod"
        )
      )
    )
    updateTextInput(
      session,
      "runCqDetectionMethodText",
      value = editor_value_chr(
        editor_prop(
          run,
          "cqDetectionMethod"
        )
      )
    )

    tcc <- editor_prop(
      run,
      "thermalCyclingConditions"
    )

    updateSelectInput(
      session,
      "runTccSlct",
      selected = editor_id_chr(
        tcc
      )
    )

    format <- editor_prop(
      run,
      "pcrFormat"
    )

    updateNumericInput(
      session,
      "runRowsText",
      value = {
        value <- editor_prop(
          format,
          "rows"
        )

        if (
          is.null(value) ||
          !length(value) ||
          is.na(value[[1L]])
        ) {
          8
        } else {
          value[[1L]]
        }
      }
    )

    updateNumericInput(
      session,
      "runColumnsText",
      value = {
        value <- editor_prop(
          format,
          "columns"
        )

        if (
          is.null(value) ||
          !length(value) ||
          is.na(value[[1L]])
        ) {
          12
        } else {
          value[[1L]]
        }
      }
    )

    updateSelectInput(
      session,
      "runRowLabelSlct",
      selected = editor_value_chr(
        editor_prop(
          format,
          "rowLabel"
        ),
        "ABC"
      )
    )

    updateSelectInput(
      session,
      "runColumnLabelSlct",
      selected = editor_value_chr(
        editor_prop(
          format,
          "columnLabel"
        ),
        "123"
      )
    )

    updateTextInput(
      session,
      "runDateText",
      value = editor_display(
        editor_prop(
          run,
          "runDate"
        )
      )
    )

    updateSelectInput(
      session,
      "runDocumentationSlct",
      selected = vapply(
        editor_list_prop(
          run,
          "documentation"
        ),
        editor_id_chr,
        character(1)
      )
    )

    updateSelectInput(
      session,
      "runExperimenterSlct",
      selected = vapply(
        editor_list_prop(
          run,
          "experimenter"
        ),
        editor_id_chr,
        character(1)
      )
    )

    updateSelectizeInput(
      session,
      "reactSlct",
      choices = names(
        editor_list_prop(
          run,
          "react"
        )
      ),
      server = TRUE
    )
  },
  ignoreInit = TRUE
)

observeEvent(
  input$reactSlct,
  {
    req(
      values$rdml,
      input$experimentSlct,
      input$runSlct
    )

    react <- editor_get_react(
      values$rdml,
      input$experimentSlct,
      input$runSlct,
      input$reactSlct
    )

    if (is.null(react)) {
      updateTextInput(
        session,
        "reactIdText",
        value = input$reactSlct
      )
      updateSelectizeInput(
        session,
        "dataSlct",
        choices = character(),
        server = TRUE
      )
      return()
    }

    updateTextInput(
      session,
      "reactIdText",
      value = {
        id <- editor_id_chr(
          editor_prop(
            react,
            "id"
          )
        )

        if (nzchar(id)) {
          id
        } else {
          input$reactSlct
        }
      }
    )

    updateSelectInput(
      session,
      "reactSampleSlct",
      selected = editor_id_chr(
        editor_prop(
          react,
          "sample"
        )
      )
    )

    updateSelectizeInput(
      session,
      "dataSlct",
      choices = names(
        editor_list_prop(
          react,
          "data"
        )
      ),
      server = TRUE
    )
  },
  ignoreInit = TRUE
)

observeEvent(
  input$dataSlct,
  {
    req(
      values$rdml,
      input$experimentSlct,
      input$runSlct,
      input$reactSlct
    )

    data_obj <- editor_get_data(
      values$rdml,
      input$experimentSlct,
      input$runSlct,
      input$reactSlct,
      input$dataSlct
    )

    if (is.null(data_obj)) {
      updateSelectInput(
        session,
        "dataTarSlct",
        selected = input$dataSlct
      )
      return()
    }

    updateSelectInput(
      session,
      "dataTarSlct",
      selected = {
        target <- editor_id_chr(
          editor_prop(
            data_obj,
            "targetId"
          )
        )

        if (nzchar(target)) {
          target
        } else {
          input$dataSlct
        }
      }
    )

    updateTextInput(
      session,
      "dataCqText",
      value = editor_display(
        editor_prop(
          data_obj,
          "cq"
        )
      )
    )

    updateTextInput(
      session,
      "dataMeltTempText",
      value = editor_display(
        editor_prop(
          data_obj,
          "meltTemp"
        )
      )
    )

    updateTextInput(
      session,
      "dataExclText",
      value = editor_display(
        editor_prop(
          data_obj,
          "excl"
        )
      )
    )

    updateTextInput(
      session,
      "dataEndPtText",
      value = editor_display(
        editor_prop(
          data_obj,
          "endPt"
        )
      )
    )

    updateTextInput(
      session,
      "dataBgFluorText",
      value = editor_display(
        editor_prop(
          data_obj,
          "bgFluor"
        )
      )
    )

    updateTextInput(
      session,
      "dataBgFluorSlpText",
      value = editor_display(
        editor_prop(
          data_obj,
          "bgFluorSlp"
        )
      )
    )

    updateTextInput(
      session,
      "dataQuantFluorText",
      value = editor_display(
        editor_prop(
          data_obj,
          "quantFluor"
        )
      )
    )
  },
  ignoreInit = TRUE
)

observeEvent(
  input$saveExperimentBtn,
  {
    req(
      values$rdml
    )

    old_id <- isolate(
      input$experimentSlct
    )
    new_id <- editor_empty_to_null(
      input$experimentIdText
    )
    req(
      new_id
    )

    existing <- editor_collection_get(
      values$rdml,
      "experiment",
      old_id
    )

    values_to_set <- list(
      id = RDML7::idType(
        new_id
      ),
      description = editor_text_value(
        input$experimentDescriptionText
      ),
      documentation = editor_make_id_refs(
        input$experimentDocumentationSlct
      )
    )

    if (is.null(existing)) {
      values_to_set$run <- list()
    }

    object <- runSafe(
      editor_construct_or_update(
        existing,
        RDML7::experimentType,
        values_to_set
      )
    )

    if (!is.null(object)) {
      values$rdml <- editor_set_experiment(
        values$rdml,
        old_id,
        new_id,
        object
      )
      commitActive()
      refreshExperimentSelectors()
      updateSelectizeInput(
        session,
        "experimentSlct",
        selected = new_id
      )
    }
  }
)

observeEvent(
  input$saveRunBtn,
  {
    req(
      values$rdml,
      input$experimentSlct
    )

    old_id <- isolate(
      input$runSlct
    )
    new_id <- editor_empty_to_null(
      input$runIdText
    )
    req(
      new_id
    )

    existing <- editor_get_run(
      values$rdml,
      input$experimentSlct,
      old_id
    )

    software_name <- editor_text_value(
      input$runDataCollectionSoftwareNameText
    )
    software_version <- editor_text_value(
      input$runDataCollectionSoftwareVersionText
    )

    software <- NULL

    if (
      !is.na(software_name) ||
      !is.na(software_version)
    ) {
      software <- RDML7::dataCollectionSoftwareType(
        name = software_name,
        version = software_version
      )
    }

    cq_method <- editor_empty_to_null(
      input$runCqDetectionMethodText
    )

    tcc_id <- editor_empty_to_null(
      input$runTccSlct
    )

    pcr_format <- RDML7::pcrFormatType(
      rows = editor_optional_integer(
        input$runRowsText
      ),
      columns = editor_optional_integer(
        input$runColumnsText
      ),
      rowLabel = RDML7::labelFormatType(
        input$runRowLabelSlct
      ),
      columnLabel = RDML7::labelFormatType(
        input$runColumnLabelSlct
      )
    )

    values_to_set <- list(
      id = RDML7::idType(
        new_id
      ),
      description = editor_text_value(
        input$runDescriptionText
      ),
      documentation = editor_make_id_refs(
        input$runDocumentationSlct
      ),
      experimenter = editor_make_id_refs(
        input$runExperimenterSlct
      ),
      instrument = editor_text_value(
        input$runInstrumentText
      ),
      dataCollectionSoftware = software,
      backgroundDeterminationMethod = editor_text_value(
        input$runBackgroundDeterminationMethodText
      ),
      cqDetectionMethod = if (
        is.null(cq_method)
      ) {
        NULL
      } else {
        RDML7::cqDetectionMethodType(
          cq_method
        )
      },
      thermalCyclingConditions = if (
        is.null(tcc_id)
      ) {
        NULL
      } else {
        RDML7::idReferenceType(
          tcc_id
        )
      },
      pcrFormat = pcr_format,
      runDate = editor_text_value(
        input$runDateText
      )
    )

    if (is.null(existing)) {
      values_to_set$react <- list()
    }

    object <- runSafe(
      editor_construct_or_update(
        existing,
        RDML7::runType,
        values_to_set
      )
    )

    if (!is.null(object)) {
      values$rdml <- editor_set_run(
        values$rdml,
        input$experimentSlct,
        old_id,
        new_id,
        object
      )
      commitActive()
      refreshExperimentSelectors()

      updateSelectizeInput(
        session,
        "runSlct",
        selected = new_id
      )
    }
  }
)

observeEvent(
  input$saveReactBtn,
  {
    req(
      values$rdml,
      input$experimentSlct,
      input$runSlct
    )

    old_id <- isolate(
      input$reactSlct
    )
    new_id <- editor_empty_to_null(
      input$reactIdText
    )
    sample_id <- editor_empty_to_null(
      input$reactSampleSlct
    )

    req(
      new_id,
      sample_id
    )

    existing <- editor_get_react(
      values$rdml,
      input$experimentSlct,
      input$runSlct,
      old_id
    )

    values_to_set <- list(
      id = editor_construct_react_id(
        new_id
      ),
      sample = RDML7::idReferenceType(
        sample_id
      )
    )

    if (is.null(existing)) {
      values_to_set$data <- list()
    }

    object <- runSafe(
      editor_construct_or_update(
        existing,
        RDML7::reactType,
        values_to_set
      )
    )

    if (!is.null(object)) {
      values$rdml <- editor_set_react(
        values$rdml,
        input$experimentSlct,
        input$runSlct,
        old_id,
        new_id,
        object
      )
      commitActive()

      updateSelectizeInput(
        session,
        "reactSlct",
        selected = new_id
      )
    }
  }
)

observeEvent(
  input$saveDataBtn,
  {
    req(
      values$rdml,
      input$experimentSlct,
      input$runSlct,
      input$reactSlct
    )

    old_target <- isolate(
      input$dataSlct
    )
    new_target <- editor_empty_to_null(
      input$dataTarSlct
    )
    req(
      new_target
    )

    existing <- editor_get_data(
      values$rdml,
      input$experimentSlct,
      input$runSlct,
      input$reactSlct,
      old_target
    )

    values_to_set <- list(
      targetId = RDML7::idReferenceType(
        new_target
      ),
      cq = editor_optional_numeric(
        input$dataCqText
      ),
      meltTemp = editor_numeric_vector(
        input$dataMeltTempText
      ),
      excl = editor_text_value(
        input$dataExclText
      ),
      endPt = editor_optional_numeric(
        input$dataEndPtText
      ),
      bgFluor = editor_optional_numeric(
        input$dataBgFluorText
      ),
      bgFluorSlp = editor_optional_numeric(
        input$dataBgFluorSlpText
      ),
      quantFluor = editor_optional_numeric(
        input$dataQuantFluorText
      )
    )

    object <- runSafe(
      editor_construct_or_update(
        existing,
        RDML7::dataType,
        values_to_set
      )
    )

    if (!is.null(object)) {
      values$rdml <- editor_set_data(
        values$rdml,
        input$experimentSlct,
        input$runSlct,
        input$reactSlct,
        old_target,
        new_target,
        object
      )
      commitActive()

      updateSelectizeInput(
        session,
        "dataSlct",
        selected = new_target
      )
    }
  }
)

observeEvent(
  input$removeExperimentBtn,
  {
    req(
      values$rdml,
      input$experimentSlct
    )

    values$rdml <- editor_remove_experiment(
      values$rdml,
      input$experimentSlct
    )
    commitActive()
    refreshExperimentSelectors()
  }
)

observeEvent(
  input$removeRunBtn,
  {
    req(
      values$rdml,
      input$experimentSlct,
      input$runSlct
    )

    values$rdml <- editor_remove_run(
      values$rdml,
      input$experimentSlct,
      input$runSlct
    )
    commitActive()
    refreshExperimentSelectors()
  }
)

observeEvent(
  input$removeReactBtn,
  {
    req(
      values$rdml,
      input$experimentSlct,
      input$runSlct,
      input$reactSlct
    )

    values$rdml <- editor_remove_react(
      values$rdml,
      input$experimentSlct,
      input$runSlct,
      input$reactSlct
    )
    commitActive()
  }
)

observeEvent(
  input$removeDataBtn,
  {
    req(
      values$rdml,
      input$experimentSlct,
      input$runSlct,
      input$reactSlct,
      input$dataSlct
    )

    values$rdml <- editor_remove_data(
      values$rdml,
      input$experimentSlct,
      input$runSlct,
      input$reactSlct,
      input$dataSlct
    )
    commitActive()
  }
)

output$experimentPathPreview <- renderPrint({
  req(
    values$rdml
  )

  list(
    experiment = input$experimentSlct,
    run = input$runSlct,
    react = input$reactSlct,
    data = input$dataSlct
  )
})


# ---------------------------------------------------------------------------
# qPCR and melting shared state
# ---------------------------------------------------------------------------

activeRawStore <- function(dp_type) {
  document <- isolate(
    input$rdmlFileSlct
  )

  if (
    is.null(document) ||
    !nzchar(document)
  ) {
    document <- ".active"
  }

  if (is.null(values$rawCurves[[document]])) {
    values$rawCurves[[document]] <- list(
      adp = list(),
      mdp = list()
    )
  }

  if (is.null(values$rawCurves[[document]][[dp_type]])) {
    values$rawCurves[[document]][[dp_type]] <- list()
  }

  values$rawCurves[[document]][[dp_type]]
}

setActiveRawStore <- function(dp_type, store) {
  document <- isolate(
    input$rdmlFileSlct
  )

  if (
    is.null(document) ||
    !nzchar(document)
  ) {
    document <- ".active"
  }

  if (is.null(values$rawCurves[[document]])) {
    values$rawCurves[[document]] <- list(
      adp = list(),
      mdp = list()
    )
  }

  values$rawCurves[[document]][[dp_type]] <- store
  invisible(NULL)
}

backupCurve <- function(meta, dp_type) {
  store <- activeRawStore(
    dp_type
  )

  key <- meta$curveKey[[1L]]

  if (is.null(store[[key]])) {
    store[[key]] <- editor_get_curve_points(
      values$rdml,
      meta$expId[[1L]],
      meta$runId[[1L]],
      meta$reactId[[1L]],
      meta$target[[1L]],
      dp_type
    )

    setActiveRawStore(
      dp_type,
      store
    )
  }

  invisible(NULL)
}

restoreCurves <- function(dp_type) {
  store <- activeRawStore(
    dp_type
  )

  if (!length(store)) {
    return(
      invisible(NULL)
    )
  }

  catalog <- editor_curve_catalog(
    values$rdml,
    dp_type
  )

  for (i in seq_len(nrow(catalog))) {
    meta <- catalog[i, , drop = FALSE]
    points <- store[[meta$curveKey[[1L]]]]

    if (is.null(points)) {
      next
    }

    values$rdml <- editor_set_curve_points(
      values$rdml,
      meta$expId[[1L]],
      meta$runId[[1L]],
      meta$reactId[[1L]],
      meta$target[[1L]],
      dp_type,
      points
    )
  }

  commitActive()
  invisible(NULL)
}


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

        numericInput(
          id,
          HTML(
            sprintf(
              "Threshold <b>%s</b>%s",
              target,
              scale_text
            )
          ),
          value = 0,
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
    value <- 0
  }

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

qPCRProcessingTrigger <- reactive({
  list(
    preprocess = input$preprocessqPCR,
    smooth = input$smoothqPCRmethod,
    normalize = input$normqPCRmethod
  )
})

observeEvent(
  qPCRProcessingTrigger(),
  {
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
  },
  ignoreInit = TRUE
)

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

cqTrigger <- reactive({
  catalog <- qPCRCatalog()

  thresholds <- if (
    input$cqMethod == "th" &&
    !isTRUE(input$autoThLevel)
  ) {
    setNames(
      lapply(
        unique(
          catalog$target
        ),
        thresholdForTarget
      ),
      unique(
        catalog$target
      )
    )
  } else {
    NULL
  }

  list(
    method = input$cqMethod,
    auto = input$autoThLevel,
    thresholds = thresholds,
    preprocessing = qPCRProcessingTrigger()
  )
})

observeEvent(
  cqTrigger(),
  {
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
              threshold = thresholdForTarget(
                meta$target[[1L]]
              ),
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
  },
  ignoreInit = TRUE
)

observeEvent(
  input$hookMethod,
  {
    req(
      values$rdml
    )

    if (
      is.null(input$hookMethod) ||
      input$hookMethod == "none"
    ) {
      values$hookResults <- list()
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
  },
  ignoreInit = TRUE
)


editorPlateDescription <- function(catalog) {
  if (!nrow(catalog)) {
    return(
      data.frame(
        position = character(),
        sample = character(),
        target = character(),
        targets = character(),
        sampleType = character(),
        cq = numeric(),
        stringsAsFactors = FALSE
      )
    )
  }

  positions <- unique(
    catalog$position
  )

  rows <- lapply(
    positions,
    function(position) {
      part <- catalog[
        catalog$position == position,
        ,
        drop = FALSE
      ]

      data.frame(
        position = position,
        react.id = part$reactId[[1L]],
        sample = part$sample[[1L]],
        sampleType = part$sampleType[[1L]],
        target = paste(
          unique(part$target),
          collapse = ", "
        ),
        targets = paste(
          unique(part$target),
          collapse = ", "
        ),
        cq = if (
          all(is.na(part$cq))
        ) {
          NA_real_
        } else {
          part$cq[
            which(!is.na(part$cq))[[1L]]
          ]
        },
        stringsAsFactors = FALSE
      )
    }
  )

  do.call(
    rbind,
    rows
  )
}

editorPcrFormat <- function(
    exp_id,
    run_id,
    positions = character()) {

  run <- editor_get_run(
    values$rdml,
    exp_id,
    run_id
  )

  rdml7_format <- editor_prop(
    run,
    "pcrFormat"
  )

  editor_shinyMolBio_pcr_format(
    rdml7_format,
    positions
  )
}

qPCRSelectedPositions <- reactive({
  if (
    editor_can_use_shinyMolBio()
  ) {
    selected <- input$mainPcrPlateQpcr
  } else {
    selected <- input$showqPCRPositionsFallback
  }

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
  if (
    editor_can_use_shinyMolBio()
  ) {
    selected <- input$mainPcrPlateMelting
  } else {
    selected <- input$showMeltingPositionsFallback
  }

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

    plate_description <- editorPlateDescription(
      filtered
    )

    pcr_format <- editorPcrFormat(
      input$showqPCRExperiment,
      input$showqPCRRun,
      filtered$position
    )

    return(
      shinyMolBio::pcrPlateInput(
        inputId = "mainPcrPlateQpcr",
        label = "",
        plateDescription = plate_description,
        pcrFormat = pcr_format,
        wellLabelTemplate = "{{sample}}",
        onHoverWellTextTemplate = "{{position}}\n{{sample}}\n{{targets}}",
        interactive = TRUE
      )
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

  if (
    editor_can_use_shinyMolBio()
  ) {
    plate_description <- editorPlateDescription(
      filtered
    )

    pcr_format <- editorPcrFormat(
      input$showMeltingExperiment,
      input$showMeltingRun,
      filtered$position
    )

    return(
      shinyMolBio::pcrPlateInput(
        inputId = "mainPcrPlateMelting",
        label = "",
        plateDescription = plate_description,
        pcrFormat = pcr_format,
        wellLabelTemplate = "{{sample}}",
        onHoverWellTextTemplate = "{{position}}\n{{sample}}\n{{targets}}",
        interactive = TRUE
      )
    )
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

  p
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

  plotly::subplot(
    p1,
    p2,
    nrows = 2,
    shareX = TRUE,
    titleY = TRUE
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
