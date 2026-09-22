rdml7TargetServer <- function(serverEnv) {
  if (!is.environment(serverEnv)) {
    stop("serverEnv must be an environment.", call. = FALSE)
  }

  evalq({
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


  }, envir = serverEnv)

  invisible(NULL)
}
