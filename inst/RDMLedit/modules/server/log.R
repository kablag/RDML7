rdml7LogServer <- function(serverEnv) {
  if (!is.environment(serverEnv)) {
    stop("serverEnv must be an environment.", call. = FALSE)
  }

  evalq({
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
  }, envir = serverEnv)

  invisible(NULL)
}
