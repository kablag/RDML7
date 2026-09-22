rdml7ExperimenterServer <- function(serverEnv) {
  if (!is.environment(serverEnv)) {
    stop("serverEnv must be an environment.", call. = FALSE)
  }

  evalq({
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


  }, envir = serverEnv)

  invisible(NULL)
}
