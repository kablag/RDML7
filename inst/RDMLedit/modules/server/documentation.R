rdml7DocumentationServer <- function(serverEnv) {
  if (!is.environment(serverEnv)) {
    stop("serverEnv must be an environment.", call. = FALSE)
  }

  evalq({
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


  }, envir = serverEnv)

  invisible(NULL)
}
