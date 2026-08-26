# fromFQDexport importer -----------------------------------------------------------

.rdmlImportFqd <- function(fileName, showProgress = TRUE) {
  fromFQDexport <- function() {
    if (!requireNamespace("data.table", quietly = TRUE)) {
      stop("Package 'data.table' is required for FQD-96a import", call. = FALSE)
    }

    content <- readChar(
      fileName,
      nchars = file.info(fileName)$size,
      useBytes = TRUE
    )
    inpstr <- strsplit(content, "Quan\\.", perl = TRUE)[[1L]]
    if (length(inpstr) < 4L) {
      stop("Unsupported FQD-96a text export", call. = FALSE)
    }

    description <- data.table::fread(
      input = inpstr[[4L]],
      fill = TRUE,
      sep = "\t",
      skip = 2,
      blank.lines.skip = TRUE,
      header = TRUE
    )
    description <- description[Well != "Well"]

    expectedDescNames <- c(
      "position", "sample.id", "sample", "sampleType",
      "target", "targetDyeId", "cq", "cq.mean", "cq.sd",
      "quantity", "quantity.avg", "quantity.sd", "V13", "fdataName"
    )
    if (ncol(description) != length(expectedDescNames)) {
      stop(
        "Unexpected FQD description column count: ", ncol(description),
        " (expected ", length(expectedDescNames), ")",
        call. = FALSE
      )
    }
    data.table::setnames(description, expectedDescNames)
    description[, fdataName := paste(position, target, sep = "_")]

    numCols <- c(
      "cq", "cq.mean", "cq.sd", "quantity", "quantity.avg", "quantity.sd"
    )
    description[, (numCols) := lapply(.SD, .rdmlAsNumeric), .SDcols = numCols]
    description[, `:=`(
      expId = "exp1",
      runId = "raw_data",
      sample = ifelse(sample == "", "unkn_s", sample),
      reactId = vapply(position, .fromPositionToId, numeric(1))
    )]

    typeMap <- c(
      Unknown = "unkn",
      Standard = "std",
      Negative = "ntc",
      Positive = "pos"
    )
    mapped <- unname(typeMap[description$sampleType])
    description$sampleType[!is.na(mapped)] <- mapped[!is.na(mapped)]

    makeFData <- function(txt) {
      z <- data.table::fread(
        input = txt,
        fill = TRUE,
        sep = "\t",
        skip = 2,
        blank.lines.skip = TRUE,
        header = TRUE
      )
      z <- z[Well != "Well"]
      drop <- c("Well", "Property", "Std. Con.", "Target", "Dye")
      keep <- setdiff(names(z), drop)
      if (!length(keep)) {
        stop("FQD fluorescence table has no cycle columns", call. = FALSE)
      }

      m <- t(as.matrix(z[, ..keep]))
      mNum <- matrix(
        suppressWarnings(base::as.numeric(m)),
        nrow = nrow(m),
        ncol = ncol(m),
        dimnames = dimnames(m)
      )
      if (ncol(mNum) != nrow(description)) {
        stop(
          "FQD fluorescence/description column mismatch",
          call. = FALSE
        )
      }

      out <- data.frame(cyc = seq_len(nrow(mNum)), check.names = FALSE)
      for (j in seq_len(ncol(mNum))) {
        out[[description$fdataName[[j]]]] <- mNum[, j]
      }
      out
    }

    rawfdata <- makeFData(inpstr[[2L]])
    processedfdata <- makeFData(inpstr[[3L]])

    x <- .rdmlNewImport()
    x <- .rdmlsetFDataImport(x, rawfdata, description, "adp")

    descriptionProcessed <- data.table::copy(description)
    descriptionProcessed[, runId := "processed_data"]
    x <- .rdmlsetFDataImport(x, processedfdata, descriptionProcessed, "adp")
    x
  }

  fromFQDexport()
}
