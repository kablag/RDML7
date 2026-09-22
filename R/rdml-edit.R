#' Launch the RDML editing interface
#'
#' Starts the optional Shiny-based editor when its GUI dependencies are
#' installed.
#'
#' @return Result of launching the application, invisibly where applicable.
#' @seealso `readRDML`, `writeRDML`
#' @export
editRDML <- function() {
  requiredPackages <- c(
    "shiny",
    "shinythemes",
    "plotly",
    "DT"
  )
  missingPackages <- requiredPackages[
    !vapply(
      requiredPackages,
      requireNamespace,
      logical(1),
      quietly = TRUE
    )
  ]

  if (length(missingPackages)) {
    stop(
      "Package(s) required for editRDML() are not installed: ",
      paste(missingPackages, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  appDir <- system.file("RDMLedit", package = "RDML7")

  if (!nzchar(appDir)) {
    stop(
      "Bundled RDMLedit application was not found in the installed package.",
      call. = FALSE
    )
  }

  shiny::runApp(appDir)
}
