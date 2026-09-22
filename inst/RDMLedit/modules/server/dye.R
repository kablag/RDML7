rdml7DyeServer <- function(serverEnv) {
  if (!is.environment(serverEnv)) {
    stop("serverEnv must be an environment.", call. = FALSE)
  }

  evalq({
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


  }, envir = serverEnv)

  invisible(NULL)
}
