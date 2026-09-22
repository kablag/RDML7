rdml7StoreServer <- function(serverEnv) {
  if (!is.environment(serverEnv)) {
    stop("serverEnv must be an environment.", call. = FALSE)
  }

  evalq({
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


  }, envir = serverEnv)

  invisible(NULL)
}
