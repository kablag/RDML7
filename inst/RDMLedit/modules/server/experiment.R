rdml7ExperimentServer <- function(serverEnv) {
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


  }, envir = serverEnv)

  invisible(NULL)
}
