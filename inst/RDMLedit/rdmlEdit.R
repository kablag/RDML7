#' RDML7 graphical editor
#'
#' Launches the RDML7-native Shiny editor for loading, inspecting, editing,
#' merging, plotting, and saving RDML data.
#'
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return The value returned invisibly by [shiny::runApp()].
#'
#' @export
rdmlEdit <- function(...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop(
      "Package 'shiny' is required for rdmlEdit().",
      call. = FALSE
    )
  }

  appDir <- system.file(
    "RDMLedit",
    package = "RDML7"
  )

  if (!nzchar(appDir)) {
    stop(
      "RDMLedit application files were not installed.",
      call. = FALSE
    )
  }

  shiny::runApp(
    appDir,
    ...
  )
}
