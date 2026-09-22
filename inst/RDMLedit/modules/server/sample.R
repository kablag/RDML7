rdml7SampleServer <- function(serverEnv) {
  if (!is.environment(serverEnv)) {
    stop("serverEnv must be an environment.", call. = FALSE)
  }

  evalq({
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


  }, envir = serverEnv)

  invisible(NULL)
}
