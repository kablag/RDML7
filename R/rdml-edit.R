#' Launch the RDML editing interface
#'
#' Starts the optional Shiny-based editor when its GUI dependencies are
#' installed.
#'
#' @return Result of launching the application, invisibly where applicable.
#' @seealso `readRDML`, `writeRDML`
#' @export
editRDML <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop(
      "Package 'shiny' is required for editRDML().",
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
