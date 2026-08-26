# fromExcel importer -----------------------------------------------------------

.rdmlImportExcel <- function(fileName, showProgress = TRUE) {
  fromExcel <- function() {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package 'readxl' is required for Excel import", call. = FALSE)
    }

    descr <- as.data.frame(
      readxl::read_excel(fileName, sheet = "description"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    # Bio-Rad export layout.
    if ("Well" %in% names(descr)) {
      if (!("Starting Quantity (SQ)" %in% names(descr))) {
        descr[["Starting Quantity (SQ)"]] <- NA_real_
      }

      required <- c("Well", "Sample", "Content", "Target", "Fluor")
      miss <- setdiff(required, names(descr))
      if (length(miss)) {
        stop(
          "Bio-Rad Excel description is missing: ",
          paste(miss, collapse = ", "),
          call. = FALSE
        )
      }

      descr <- data.frame(
        fdataName = as.character(descr[["Well"]]),
        expId = "Exp1",
        runId = "Run1",
        reactId = seq_len(nrow(descr)),
        sample = as.character(descr[["Sample"]]),
        sampleType = tolower(as.character(descr[["Content"]])),
        target = as.character(descr[["Target"]]),
        targetDyeId = as.character(descr[["Fluor"]]),
        quantity = suppressWarnings(base::as.numeric(descr[["Starting Quantity (SQ)"]])),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      # Unkn-1 -> unkn, Std-2 -> std, etc.
      descr$sampleType <- sub("-.*$", "", descr$sampleType)
      # A01 -> A1.
      descr$fdataName <- sub(
        "^([A-Za-z]+)0+([1-9].*)$",
        "\\1\\2",
        descr$fdataName
      )
    }

    readNumericSheet <- function(sheet) {
      tryCatch({
        z <- as.data.frame(
          readxl::read_excel(fileName, sheet = sheet),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        z[] <- lapply(z, function(v) suppressWarnings(base::as.numeric(v)))
        z
      }, error = function(e) NULL)
    }

    adpData <- readNumericSheet("adp")
    mdpData <- readNumericSheet("mdp")

    if (is.null(adpData) && is.null(mdpData)) {
      stop("Excel file contains neither 'adp' nor 'mdp' sheet", call. = FALSE)
    }

    x <- .rdmlNewImport()
    if (!is.null(adpData)) {
      x <- .rdmlsetFDataImport(x, adpData, descr, "adp")
    }
    if (!is.null(mdpData)) {
      x <- .rdmlsetFDataImport(x, mdpData, descr, "mdp")
    }
    x
  }

  fromExcel()
}
