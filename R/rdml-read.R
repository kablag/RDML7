#' Read qPCR data and return an S7 rdmlType object
#'
#' Supported import formats mirror the active import branches in the upstream
#' PCRuniversum/RDML initializer: RDML/LC96, ABI `.eds`, Rotor-Gene `.rex`,
#' Excel `.xlsx`/`.xls`, DTprime `.r96`, CSV, and FQD-96a text export.
#'
#' @param filename Input file path.
#' @param show.progress Show import progress.
#' @param conditions.sep Optional Roche condition separator retained for
#'   compatibility with the original importer.
#' @param cluster Reserved for compatibility.
#' @param format Import format or `auto`.
#' @export
rdml_read <- function(
    filename,
    show.progress = TRUE,
    conditions.sep = NULL,
    cluster = NULL,
    format = "auto") {

  if (missing(filename)) stop("filename is required", call. = FALSE)
  checkmate::assertString(filename)
  checkmate::assertFlag(show.progress)

  if (identical(tolower(format), "auto")) {
    ext <- tolower(tools::file_ext(filename))
    format <- switch(
      ext,
      eds = "abi",
      rex = "rotorgene",
      xlsx = "excel",
      xls = "excel",
      csv = "csv",
      r96 = "dtprime",
      txt = "fqd",
      xml = "xml",
      rdml = "rdml",
      lc96p = "rdml",
      "rdml"
    )
  } else {
    format <- switch(
      tolower(format),
      abi = "abi",
      eds = "abi",
      rotorgene = "rotorgene",
      `rotor-gene` = "rotorgene",
      rex = "rotorgene",
      excel = "excel",
      xlsx = "excel",
      xls = "excel",
      csv = "csv",
      dtprime = "dtprime",
      r96 = "dtprime",
      fqd = "fqd",
      fqd96 = "fqd",
      txt = "fqd",
      xml = "xml",
      rdml = "rdml",
      lc96p = "rdml",
      stop("Unsupported import format: ", format, call. = FALSE)
    )
  }

  switch(
    format,
    abi = .rdml_import_abi(filename, show.progress),
    rotorgene = .rdml_import_rotorgene(filename, show.progress),
    excel = .rdml_import_excel(filename, show.progress),
    csv = .rdml_import_csv(filename, show.progress),
    dtprime = .rdml_import_dtprime(filename, show.progress),
    fqd = .rdml_import_fqd(filename, show.progress),
    xml = .rdml_import_rdml(
      filename, show.progress, conditions.sep, cluster, format = "xml"
    ),
    rdml = .rdml_import_rdml(
      filename, show.progress, conditions.sep, cluster, format = "rdml"
    ),
    stop("Unsupported import format: ", format, call. = FALSE)
  )
}
