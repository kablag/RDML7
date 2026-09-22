rdml7FilesServer <- function(serverEnv) {
  if (!is.environment(serverEnv)) {
    stop("serverEnv must be an environment.", call. = FALSE)
  }

  evalq({
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


  }, envir = serverEnv)

  invisible(NULL)
}
