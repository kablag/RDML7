# fromCSV importer -----------------------------------------------------------

.rdmlImportCsv <- function(fileName, showProgress = TRUE) {
  fromCSV <- function() {
    pcrdata <- utils::read.csv(
      fileName,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    if (ncol(pcrdata) < 2L) {
      stop("CSV must contain a coordinate column and fluorescence columns", call. = FALSE)
    }

    fdataNames <- names(pcrdata)[-1L]
    firstName <- tolower(names(pcrdata)[1L])
    dataType <- if (firstName %in% c("tmp", "temperature")) "mdp" else "adp"

    descr <- data.frame(
      fdataName = fdataNames,
      expId = "exp1",
      runId = "run1",
      reactId = seq_along(fdataNames),
      sample = fdataNames,
      target = "unkn",
      targetDyeId = "unkn",
      sampleType = "unkn",
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    x <- .rdmlNewImport()
    .rdmlsetFDataImport(x, pcrdata, descr, dataType)
  }

  fromCSV()
}
