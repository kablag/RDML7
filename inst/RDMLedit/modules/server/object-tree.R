rdml7ObjectTreeServer <- function(serverEnv) {
  if (!is.environment(serverEnv)) {
    stop("serverEnv must be an environment.", call. = FALSE)
  }

  evalq({
  # Generic S7 view ---------------------------------------------------------

  output$rdmlObjectTree <- renderText({
    req(
      values$rdml
    )

    editor_path_text(
      values$rdml
    )
  })

  }, envir = serverEnv)

  invisible(NULL)
}
