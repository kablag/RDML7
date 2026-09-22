rdml7RdmlIdServer <- function(serverEnv) {
  if (!is.environment(serverEnv)) {
    stop("serverEnv must be an environment.", call. = FALSE)
  }

  evalq({
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


  }, envir = serverEnv)

  invisible(NULL)
}
