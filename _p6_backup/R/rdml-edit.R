#' RDML Editor Graphical User Interface
#'
#' Launches the bundled Shiny application for editing RDML metadata and
#' inspecting qPCR or melting curves.
#'
#' @export
rdmlEdit <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop(
      "Package 'shiny' is required for rdmlEdit().",
      call. = FALSE
    )
  }

  appDir <- system.file(
    "RDMLedit",
    package = "RDML"
  )

  if (!nzchar(appDir)) {
    stop(
      "Bundled RDMLedit application was not found in the installed package.",
      call. = FALSE
    )
  }

  shiny::runApp(appDir)
}
