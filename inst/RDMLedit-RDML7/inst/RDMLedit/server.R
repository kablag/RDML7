library(shiny)

source(
  "helpers.R",
  local = TRUE
)

source(
  "analysis-helpers.R",
  local = TRUE
)

if (!requireNamespace("RDML7", quietly = TRUE)) {
  stop(
    "RDML7 must be installed to run RDML7 Editor.",
    call. = FALSE
  )
}

if (!requireNamespace("S7", quietly = TRUE)) {
  stop(
    "S7 must be installed to run RDML7 Editor.",
    call. = FALSE
  )
}

shinyServer(function(input, output, session) {

  values <- reactiveValues(
    RDMLs = list(),
    rdml = NULL,
    log = character(),
    rawCurves = list(),
    hookResults = list(),
    thresholds = list()
  )

  updLog <- function(message) {
    message <- sprintf(
      "[%s] %s",
      format(
        Sys.time(),
        "%H:%M:%S"
      ),
      message
    )

    cat(
      message,
      "\n"
    )

    isolate({
      values$log <- c(
        values$log,
        message
      )
    })
  }

  runSafe <- function(expr) {
    tryCatch(
      expr,
      error = function(e) {
        updLog(
          conditionMessage(e)
        )
        NULL
      }
    )
  }

  activeName <- reactive({
    input$rdmlFileSlct
  })

  commitActive <- function() {
    name <- isolate(
      input$rdmlFileSlct
    )

    if (
      !is.null(values$rdml) &&
      !is.null(name) &&
      nzchar(name)
    ) {
      values$RDMLs[[name]] <- editor_clone(
        values$rdml
      )
    }

    invisible(NULL)
  }

  refreshFileSelectors <- function(selected = NULL) {
    choices <- names(
      values$RDMLs
    )

    if (
      is.null(selected) &&
      length(choices)
    ) {
      selected <- tail(
        choices,
        1L
      )
    }

    updateSelectizeInput(
      session,
      "rdmlFileSlct",
      choices = choices,
      selected = selected,
      server = TRUE
    )

    current <- isolate(
      input$rdmlFileSlct
    )

    updateSelectInput(
      session,
      "mergeRdmlsSlct",
      choices = setdiff(
        choices,
        current
      )
    )
  }

  refreshMetadataSelectors <- function() {
    if (is.null(values$rdml)) {
      return(invisible(NULL))
    }

    updateSelectizeInput(
      session,
      "idSlct",
      choices = editor_collection_names(
        values$rdml,
        "id"
      ),
      server = TRUE
    )

    updateSelectizeInput(
      session,
      "experimenterSlct",
      choices = editor_collection_names(
        values$rdml,
        "experimenter"
      ),
      server = TRUE
    )

    updateSelectizeInput(
      session,
      "documentationSlct",
      choices = editor_collection_names(
        values$rdml,
        "documentation"
      ),
      server = TRUE
    )

    dyes <- editor_collection_names(
      values$rdml,
      "dye"
    )

    updateSelectizeInput(
      session,
      "dyeSlct",
      choices = dyes,
      server = TRUE
    )

    updateSelectInput(
      session,
      "targetDyeIdSlct",
      choices = c(
        "",
        dyes
      )
    )

    updateSelectizeInput(
      session,
      "sampleSlct",
      choices = editor_collection_names(
        values$rdml,
        "sample"
      ),
      server = TRUE
    )

    updateSelectizeInput(
      session,
      "targetSlct",
      choices = editor_collection_names(
        values$rdml,
        "target"
      ),
      server = TRUE
    )

    invisible(NULL)
  }

  loadActive <- function(name) {
    if (
      is.null(name) ||
      !nzchar(name) ||
      !(name %in% names(values$RDMLs))
    ) {
      return(invisible(NULL))
    }

    values$rdml <- editor_clone(
      values$RDMLs[[name]]
    )

    updateTextInput(
      session,
      "dateMadeText",
      value = editor_display(
        values$rdml$dateMade
      )
    )

    updateTextInput(
      session,
      "dateUpdatedText",
      value = editor_display(
        values$rdml$dateUpdated
      )
    )

    refreshMetadataSelectors()
    invisible(NULL)
  }


  # Files -------------------------------------------------------------------

  observeEvent(
    input$rdmlFiles,
    {
      req(
        input$rdmlFiles
      )

      withProgress(
        message = "Reading files",
        value = 0,
        {
          n <- nrow(
            input$rdmlFiles
          )
          last_key <- NULL

          for (i in seq_len(n)) {
            original_name <- input$rdmlFiles$name[[i]]
            extension <- tools::file_ext(
              original_name
            )

            read_path <- input$rdmlFiles$datapath[[i]]

            if (nzchar(extension)) {
              preserved_path <- paste0(
                read_path,
                ".",
                extension
              )

              ok <- file.copy(
                read_path,
                preserved_path,
                overwrite = TRUE
              )

              if (ok) {
                read_path <- preserved_path
              }
            }

            object <- runSafe(
              RDML7::readRDML(
                read_path
              )
            )

            if (!is.null(object)) {
              key <- paste0(
                tools::file_path_sans_ext(
                  basename(
                    original_name
                  )
                ),
                "@",
                format(
                  Sys.time(),
                  "%H%M%S"
                )
              )

              values$RDMLs[[key]] <- object
              last_key <- key
            }

            incProgress(
              1 / n
            )
          }

          refreshFileSelectors(
            selected = last_key
          )

          if (!is.null(last_key)) {
            loadActive(last_key)
          }
        }
      )
    },
    ignoreInit = TRUE
  )

  observeEvent(
    input$newRDMLBtn,
    {
      name <- paste0(
        "RDML@",
        format(
          Sys.time(),
          "%H%M%S"
        )
      )

      object <- runSafe(
        RDML7::rdmlType(
          version = "1.3"
        )
      )

      if (!is.null(object)) {
        values$RDMLs[[name]] <- object
        refreshFileSelectors(
          selected = name
        )
        loadActive(
          name
        )
      }
    }
  )

  observeEvent(
    input$rdmlFileSlct,
    {
      loadActive(
        input$rdmlFileSlct
      )

      updateSelectInput(
        session,
        "mergeRdmlsSlct",
        choices = setdiff(
          names(values$RDMLs),
          input$rdmlFileSlct
        )
      )
    },
    ignoreInit = TRUE
  )

  observeEvent(
    input$removeRDMLBtn,
    {
      name <- isolate(
        input$rdmlFileSlct
      )

      if (
        !is.null(name) &&
        nzchar(name) &&
        name %in% names(values$RDMLs)
      ) {
        values$RDMLs[[name]] <- NULL
        values$rdml <- NULL
        refreshFileSelectors()
      }
      refreshMetadataSelectors()
    }
  )

  observeEvent(
    input$createSubversionRDMLBtn,
    {
      req(
        values$rdml
      )

      commitActive()

      old_name <- isolate(
        input$rdmlFileSlct
      )

      base <- sub(
        "@.*$",
        "",
        old_name
      )

      new_name <- paste0(
        base,
        "@",
        format(
          Sys.time(),
          "%H%M%S"
        )
      )

      values$RDMLs[[new_name]] <- editor_clone(
        values$rdml
      )

      refreshFileSelectors(
        selected = new_name
      )
    }
  )

  observeEvent(
    input$mergeBtn,
    {
      req(
        values$rdml
      )

      selected <- input$mergeRdmlsSlct

      if (
        is.null(selected) ||
        !length(selected)
      ) {
        return()
      }

      objects <- c(
        list(
          values$rdml
        ),
        values$RDMLs[selected]
      )

      merged <- runSafe(
        RDML7::mergeRDMLs(
          objects
        )
      )

      if (!is.null(merged)) {
        values$rdml <- merged
        commitActive()
        refreshMetadataSelectors()
      }
    }
  )

  observeEvent(
    input$updateTopLevelBtn,
    {
      req(
        values$rdml
      )

      updated <- runSafe({
        S7::set_props(
          values$rdml,
          dateMade = editor_text_value(
            input$dateMadeText
          ),
          dateUpdated = editor_text_value(
            input$dateUpdatedText
          )
        )
      })

      if (!is.null(updated)) {
        values$rdml <- updated
        commitActive()
      }
    }
  )

  output$dendroRDMLplot <- renderPlot({
    req(
      values$rdml
    )

    runSafe(
      RDML7::asDendrogram(
        values$rdml,
        plotDendrogram = TRUE
      )
    )
  })


  # RDML ID -----------------------------------------------------------------

  observeEvent(
    input$idSlct,
    {
      key <- input$idSlct

      object <- editor_get_collection_item(
        values$rdml,
        "id",
        key
      )

      if (is.null(object)) {
        updateTextInput(
          session,
          "idPublisherText",
          value = key
        )
        updateTextInput(
          session,
          "idSerialNumberText",
          value = ""
        )
        updateTextInput(
          session,
          "idMD5HashText",
          value = ""
        )
        return()
      }

      updateTextInput(
        session,
        "idPublisherText",
        value = editor_display(
          object$publisher
        )
      )

      updateTextInput(
        session,
        "idSerialNumberText",
        value = editor_display(
          object$serialNumber
        )
      )

      updateTextInput(
        session,
        "idMD5HashText",
        value = editor_display(
          object$MD5Hash
        )
      )
    },
    ignoreInit = TRUE
  )

  observeEvent(
    input$saveIDBtn,
    {
      req(
        values$rdml
      )

      new_key <- editor_empty_to_null(
        input$idPublisherText
      )

      req(
        new_key
      )

      old_key <- isolate(
        input$idSlct
      )

      existing <- editor_get_collection_item(
        values$rdml,
        "id",
        old_key
      )

      object <- runSafe(
        editor_construct_or_update(
          existing,
          RDML7::rdmlIdType,
          list(
            publisher = new_key,
            serialNumber = editor_text_value(
              input$idSerialNumberText
            ),
            MD5Hash = editor_text_value(
              input$idMD5HashText
            )
          )
        )
      )

      if (!is.null(object)) {
        values$rdml <- editor_set_collection_item(
          values$rdml,
          "id",
          old_key,
          new_key,
          object
        )

        commitActive()
        refreshMetadataSelectors()

        updateSelectizeInput(
          session,
          "idSlct",
          selected = new_key
        )
      }
    }
  )

  observeEvent(
    input$removeIDBtn,
    {
      req(
        values$rdml
      )

      values$rdml <- editor_remove_collection_item(
        values$rdml,
        "id",
        input$idSlct
      )

      commitActive()
      refreshMetadataSelectors()
    }
  )

  output$idPreview <- renderPrint({
    object <- editor_get_collection_item(
      values$rdml,
      "id",
      input$idSlct
    )

    if (!is.null(object)) {
      print(
        object
      )
    }
  })


  # Experimenter ------------------------------------------------------------

  observeEvent(
    input$experimenterSlct,
    {
      key <- input$experimenterSlct

      object <- editor_get_collection_item(
        values$rdml,
        "experimenter",
        key
      )

      if (is.null(object)) {
        updateTextInput(
          session,
          "experimenterIdText",
          value = key
        )

        for (id in c(
          "experimenterFirstNameText",
          "experimenterLastNameText",
          "experimenterEmailText",
          "experimenterLabNameText",
          "experimenterLabAddressText"
        )) {
          updateTextInput(
            session,
            id,
            value = ""
          )
        }

        return()
      }

      updateTextInput(
        session,
        "experimenterIdText",
        value = editor_id_chr(
          object$id
        )
      )
      updateTextInput(
        session,
        "experimenterFirstNameText",
        value = editor_display(
          object$firstName
        )
      )
      updateTextInput(
        session,
        "experimenterLastNameText",
        value = editor_display(
          object$lastName
        )
      )
      updateTextInput(
        session,
        "experimenterEmailText",
        value = editor_display(
          object$email
        )
      )
      updateTextInput(
        session,
        "experimenterLabNameText",
        value = editor_display(
          object$labName
        )
      )
      updateTextInput(
        session,
        "experimenterLabAddressText",
        value = editor_display(
          object$labAddress
        )
      )
    },
    ignoreInit = TRUE
  )

  observeEvent(
    input$saveExperimenterBtn,
    {
      req(
        values$rdml
      )

      new_key <- editor_empty_to_null(
        input$experimenterIdText
      )

      req(
        new_key
      )

      old_key <- isolate(
        input$experimenterSlct
      )

      existing <- editor_get_collection_item(
        values$rdml,
        "experimenter",
        old_key
      )

      object <- runSafe(
        editor_construct_or_update(
          existing,
          RDML7::experimenterType,
          list(
            id = RDML7::idType(
              new_key
            ),
            firstName = editor_text_value(
              input$experimenterFirstNameText
            ),
            lastName = editor_text_value(
              input$experimenterLastNameText
            ),
            email = editor_text_value(
              input$experimenterEmailText
            ),
            labName = editor_text_value(
              input$experimenterLabNameText
            ),
            labAddress = editor_text_value(
              input$experimenterLabAddressText
            )
          )
        )
      )

      if (!is.null(object)) {
        values$rdml <- editor_set_collection_item(
          values$rdml,
          "experimenter",
          old_key,
          new_key,
          object
        )

        commitActive()
        refreshMetadataSelectors()

        updateSelectizeInput(
          session,
          "experimenterSlct",
          selected = new_key
        )
      }
    }
  )

  observeEvent(
    input$removeExperimenterBtn,
    {
      req(
        values$rdml
      )

      values$rdml <- editor_remove_collection_item(
        values$rdml,
        "experimenter",
        input$experimenterSlct
      )

      commitActive()
      refreshMetadataSelectors()
    }
  )

  output$experimenterPreview <- renderPrint({
    object <- editor_get_collection_item(
      values$rdml,
      "experimenter",
      input$experimenterSlct
    )

    if (!is.null(object)) {
      print(
        object
      )
    }
  })


  # Documentation -----------------------------------------------------------

  observeEvent(
    input$documentationSlct,
    {
      key <- input$documentationSlct

      object <- editor_get_collection_item(
        values$rdml,
        "documentation",
        key
      )

      if (is.null(object)) {
        updateTextInput(
          session,
          "documentationIdText",
          value = key
        )
        updateTextAreaInput(
          session,
          "documentationTextText",
          value = ""
        )
        return()
      }

      updateTextInput(
        session,
        "documentationIdText",
        value = editor_id_chr(
          object$id
        )
      )

      updateTextAreaInput(
        session,
        "documentationTextText",
        value = editor_display(
          object$text
        )
      )
    },
    ignoreInit = TRUE
  )

  observeEvent(
    input$saveDocumentationBtn,
    {
      req(
        values$rdml
      )

      new_key <- editor_empty_to_null(
        input$documentationIdText
      )

      req(
        new_key
      )

      old_key <- isolate(
        input$documentationSlct
      )

      existing <- editor_get_collection_item(
        values$rdml,
        "documentation",
        old_key
      )

      object <- runSafe(
        editor_construct_or_update(
          existing,
          RDML7::documentationType,
          list(
            id = RDML7::idType(
              new_key
            ),
            text = editor_text_value(
              input$documentationTextText
            )
          )
        )
      )

      if (!is.null(object)) {
        values$rdml <- editor_set_collection_item(
          values$rdml,
          "documentation",
          old_key,
          new_key,
          object
        )

        commitActive()
        refreshMetadataSelectors()

        updateSelectizeInput(
          session,
          "documentationSlct",
          selected = new_key
        )
      }
    }
  )

  observeEvent(
    input$removeDocumentationBtn,
    {
      req(
        values$rdml
      )

      values$rdml <- editor_remove_collection_item(
        values$rdml,
        "documentation",
        input$documentationSlct
      )

      commitActive()
      refreshMetadataSelectors()
    }
  )

  output$documentationPreview <- renderPrint({
    object <- editor_get_collection_item(
      values$rdml,
      "documentation",
      input$documentationSlct
    )

    if (!is.null(object)) {
      print(
        object
      )
    }
  })


  # Dye ---------------------------------------------------------------------

  observeEvent(
    input$dyeSlct,
    {
      key <- input$dyeSlct

      object <- editor_get_collection_item(
        values$rdml,
        "dye",
        key
      )

      if (is.null(object)) {
        updateTextInput(
          session,
          "dyeIdText",
          value = key
        )
        updateTextInput(
          session,
          "dyeDescriptionText",
          value = ""
        )
        return()
      }

      updateTextInput(
        session,
        "dyeIdText",
        value = editor_id_chr(
          object$id
        )
      )

      updateTextInput(
        session,
        "dyeDescriptionText",
        value = editor_display(
          object$description
        )
      )
    },
    ignoreInit = TRUE
  )

  observeEvent(
    input$saveDyeBtn,
    {
      req(
        values$rdml
      )

      new_key <- editor_empty_to_null(
        input$dyeIdText
      )

      req(
        new_key
      )

      old_key <- isolate(
        input$dyeSlct
      )

      existing <- editor_get_collection_item(
        values$rdml,
        "dye",
        old_key
      )

      object <- runSafe(
        editor_construct_or_update(
          existing,
          RDML7::dyeType,
          list(
            id = RDML7::idType(
              new_key
            ),
            description = editor_text_value(
              input$dyeDescriptionText
            )
          )
        )
      )

      if (!is.null(object)) {
        values$rdml <- editor_set_collection_item(
          values$rdml,
          "dye",
          old_key,
          new_key,
          object
        )

        commitActive()
        refreshMetadataSelectors()

        updateSelectizeInput(
          session,
          "dyeSlct",
          selected = new_key
        )
      }
    }
  )

  observeEvent(
    input$removeDyeBtn,
    {
      req(
        values$rdml
      )

      values$rdml <- editor_remove_collection_item(
        values$rdml,
        "dye",
        input$dyeSlct
      )

      commitActive()
      refreshMetadataSelectors()
    }
  )

  output$dyePreview <- renderPrint({
    object <- editor_get_collection_item(
      values$rdml,
      "dye",
      input$dyeSlct
    )

    if (!is.null(object)) {
      print(
        object
      )
    }
  })


  # Sample ------------------------------------------------------------------

  observeEvent(
    input$sampleSlct,
    {
      key <- input$sampleSlct

      object <- editor_get_collection_item(
        values$rdml,
        "sample",
        key
      )

      if (is.null(object)) {
        updateTextInput(
          session,
          "sampleIdText",
          value = key
        )
        updateTextInput(
          session,
          "sampleDescriptionText",
          value = ""
        )
        updateSelectInput(
          session,
          "sampleTypeSlct",
          selected = ""
        )
        updateCheckboxInput(
          session,
          "sampleInterRunCalibratorChk",
          value = FALSE
        )
        updateCheckboxInput(
          session,
          "sampleCalibratorSampleChk",
          value = FALSE
        )
        return()
      }

      updateTextInput(
        session,
        "sampleIdText",
        value = editor_id_chr(
          object$id
        )
      )
      updateTextInput(
        session,
        "sampleDescriptionText",
        value = editor_display(
          object$description
        )
      )
      updateSelectInput(
        session,
        "sampleTypeSlct",
        selected = editor_value_chr(
          object$type
        )
      )
      updateCheckboxInput(
        session,
        "sampleInterRunCalibratorChk",
        value = isTRUE(
          object$interRunCalibrator
        )
      )
      updateCheckboxInput(
        session,
        "sampleCalibratorSampleChk",
        value = isTRUE(
          object$calibratorSample
        )
      )
    },
    ignoreInit = TRUE
  )

  observeEvent(
    input$saveSampleBtn,
    {
      req(
        values$rdml
      )

      new_key <- editor_empty_to_null(
        input$sampleIdText
      )
      req(
        new_key
      )

      old_key <- isolate(
        input$sampleSlct
      )

      existing <- editor_get_collection_item(
        values$rdml,
        "sample",
        old_key
      )

      sample_type <- editor_empty_to_null(
        input$sampleTypeSlct
      )

      values_to_set <- list(
        id = RDML7::idType(
          new_key
        ),
        description = editor_text_value(
          input$sampleDescriptionText
        ),
        interRunCalibrator = isTRUE(
          input$sampleInterRunCalibratorChk
        ),
        calibratorSample = isTRUE(
          input$sampleCalibratorSampleChk
        )
      )

      if (!is.null(sample_type)) {
        sample_type_value <- RDML7::sampleTypeType(
          sample_type
        )
        existing_types <- editor_list_prop(
          existing,
          "type"
        )

        if (length(existing_types)) {
          values_to_set$type <- lapply(
            existing_types,
            function(type) {
              editor_construct_or_update(
                type,
                RDML7::sampleTargetType,
                list(sampleType = sample_type_value)
              )
            }
          )
        } else {
          values_to_set$type <- list(
            RDML7::sampleTargetType(
              sampleType = sample_type_value
            )
          )
        }
      }

      object <- runSafe(
        editor_construct_or_update(
          existing,
          RDML7::sampleType,
          values_to_set
        )
      )

      if (!is.null(object)) {
        values$rdml <- editor_set_collection_item(
          values$rdml,
          "sample",
          old_key,
          new_key,
          object
        )

        commitActive()
        refreshMetadataSelectors()

        updateSelectizeInput(
          session,
          "sampleSlct",
          selected = new_key
        )
      }
    }
  )

  observeEvent(
    input$removeSampleBtn,
    {
      req(
        values$rdml
      )

      values$rdml <- editor_remove_collection_item(
        values$rdml,
        "sample",
        input$sampleSlct
      )

      commitActive()
      refreshMetadataSelectors()
    }
  )

  output$samplePreview <- renderPrint({
    object <- editor_get_collection_item(
      values$rdml,
      "sample",
      input$sampleSlct
    )

    if (!is.null(object)) {
      print(
        object
      )
    }
  })


  # Target ------------------------------------------------------------------

  observeEvent(
    input$targetSlct,
    {
      key <- input$targetSlct

      object <- editor_get_collection_item(
        values$rdml,
        "target",
        key
      )

      if (is.null(object)) {
        updateTextInput(
          session,
          "targetIdText",
          value = key
        )
        updateTextInput(
          session,
          "targetDescriptionText",
          value = ""
        )
        updateSelectInput(
          session,
          "targetTypeSlct",
          selected = ""
        )
        updateSelectInput(
          session,
          "targetDyeIdSlct",
          selected = ""
        )
        return()
      }

      updateTextInput(
        session,
        "targetIdText",
        value = editor_id_chr(
          object$id
        )
      )
      updateTextInput(
        session,
        "targetDescriptionText",
        value = editor_display(
          object$description
        )
      )
      updateSelectInput(
        session,
        "targetTypeSlct",
        selected = editor_display(
          object$type
        )
      )
      updateSelectInput(
        session,
        "targetDyeIdSlct",
        selected = editor_id_chr(
          object$dyeId
        )
      )
    },
    ignoreInit = TRUE
  )

  observeEvent(
    input$saveTargetBtn,
    {
      req(
        values$rdml
      )

      new_key <- editor_empty_to_null(
        input$targetIdText
      )
      req(
        new_key
      )

      old_key <- isolate(
        input$targetSlct
      )

      existing <- editor_get_collection_item(
        values$rdml,
        "target",
        old_key
      )

      target_type <- editor_empty_to_null(
        input$targetTypeSlct
      )
      dye_id <- editor_empty_to_null(
        input$targetDyeIdSlct
      )

      values_to_set <- list(
        id = RDML7::idType(
          new_key
        ),
        description = editor_text_value(
          input$targetDescriptionText
        )
      )

      if (!is.null(target_type)) {
        values_to_set$type <- RDML7::targetTypeType(
          target_type
        )
      }

      if (!is.null(dye_id)) {
        values_to_set$dyeId <- RDML7::idReferenceType(
          dye_id
        )
      }

      object <- runSafe(
        editor_construct_or_update(
          existing,
          RDML7::targetType,
          values_to_set
        )
      )

      if (!is.null(object)) {
        values$rdml <- editor_set_collection_item(
          values$rdml,
          "target",
          old_key,
          new_key,
          object
        )

        commitActive()
        refreshMetadataSelectors()

        updateSelectizeInput(
          session,
          "targetSlct",
          selected = new_key
        )
      }
    }
  )

  observeEvent(
    input$removeTargetBtn,
    {
      req(
        values$rdml
      )

      values$rdml <- editor_remove_collection_item(
        values$rdml,
        "target",
        input$targetSlct
      )

      commitActive()
      refreshMetadataSelectors()
    }
  )

  output$targetPreview <- renderPrint({
    object <- editor_get_collection_item(
      values$rdml,
      "target",
      input$targetSlct
    )

    if (!is.null(object)) {
      print(
        object
      )
    }
  })


  # Generic S7 view ---------------------------------------------------------

  output$rdmlObjectTree <- renderText({
    req(
      values$rdml
    )

    editor_path_text(
      values$rdml
    )
  })


  source(
    "server-full.R",
    local = TRUE
  )

  rdml7ServerFull(environment())


  # Store ------------------------------------------------------------------

  output$downloadRDML <- downloadHandler(
    filename = function() {
      name <- isolate(
        input$rdmlFileSlct
      )

      if (
        is.null(name) ||
        !nzchar(name)
      ) {
        name <- "RDML"
      }

      paste0(
        sub(
          "@.*$",
          "",
          name
        ),
        ".rdml"
      )
    },
    content = function(file) {
      req(
        values$rdml
      )

      commitActive()

      editor_save_rdml(
        values$rdml,
        file
      )
    }
  )


  # Log --------------------------------------------------------------------

  observeEvent(
    input$clearLogBtn,
    {
      values$log <- character()
    }
  )

  output$logText <- renderText({
    paste(
      values$log,
      collapse = "\n"
    )
  })
})
