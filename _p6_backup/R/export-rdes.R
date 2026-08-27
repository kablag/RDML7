# RDES export ---------------------------------------------------------------
#
# RDES v1.0 stores one selected run and one curve type per text file.
# The content is always tab-separated UTF-8 with LF line endings, even when
# the chosen extension is .csv or .txt (both are permitted by the standard).

.rdmlRdesFormatNumber <- function(x) {
  if (
    length(x) != 1L ||
    is.na(x)
  ) {
    return("")
  }

  format(
    as.numeric(x),
    scientific = FALSE,
    trim = TRUE,
    digits = 15L,
    decimal.mark = "."
  )
}


.rdmlRdesCleanText <- function(x) {
  if (
    length(x) != 1L ||
    is.na(x)
  ) {
    return("")
  }

  x <- as.character(x)

  x <- gsub(
    "[\\t\\r\\n]+",
    " ",
    x,
    perl = TRUE
  )

  enc2utf8(x)
}


.rdmlRdesWellForExport <- function(
    react,
    pcrFormat) {

  well <- .rdmlReactPosition(
    react,
    pcrFormat
  )

  if (is.na(well)) {
    return(NA_character_)
  }

  # RDML helper uses A01 for normal plates; RDES examples and description use
  # A1. Remove only zero padding in the numeric well component.
  sub(
    "^([A-Z]+)0+([1-9][0-9]*)$",
    "\\1\\2",
    well,
    perl = TRUE
  )
}


.rdmlRdesSelectRun <- function(
    x,
    expId = NULL,
    runId = NULL) {

  experiments <- .rdmlPropKeyed(
    x,
    "experiment"
  )

  if (is.null(expId)) {
    pairs <- list()

    for (candidateExpId in names(experiments)) {
      runs <- .rdmlPropKeyed(
        experiments[[candidateExpId]],
        "run"
      )

      for (candidateRunId in names(runs)) {
        pairs[[length(pairs) + 1L]] <- list(
          expId = candidateExpId,
          runId = candidateRunId,
          run = runs[[candidateRunId]]
        )
      }
    }

    if (length(pairs) != 1L) {
      stop(
        "RDES export requires exactly one run or explicit `expId` and `runId`; ",
        "found ",
        length(pairs),
        " runs",
        call. = FALSE
      )
    }

    return(
      pairs[[1L]]
    )
  }

  checkmate::assertString(expId)

  experiment <- experiments[[expId]]

  if (is.null(experiment)) {
    stop(
      "Unknown experiment for RDES export: ",
      expId,
      call. = FALSE
    )
  }

  runs <- .rdmlPropKeyed(
    experiment,
    "run"
  )

  if (is.null(runId)) {
    if (length(runs) != 1L) {
      stop(
        "Experiment '",
        expId,
        "' contains ",
        length(runs),
        " runs; specify `runId` for RDES export",
        call. = FALSE
      )
    }

    runId <- names(runs)[[1L]]
  } else {
    checkmate::assertString(runId)
  }

  run <- runs[[runId]]

  if (is.null(run)) {
    stop(
      "Unknown run '",
      runId,
      "' in experiment '",
      expId,
      "'",
      call. = FALSE
    )
  }

  list(
    expId = expId,
    runId = runId,
    run = run
  )
}


.rdmlRdesRunTypes <- function(run) {
  hasAdp <- FALSE
  hasMdp <- FALSE

  for (react in .rdmlPropList(
    run,
    "react"
  )) {
    for (dataObj in .rdmlPropList(
      react,
      "data"
    )) {
      hasAdp <- hasAdp ||
        .rdmlPresent(dataObj$adp)

      hasMdp <- hasMdp ||
        .rdmlPresent(dataObj$mdp)
    }
  }

  c(
    adp = hasAdp,
    mdp = hasMdp
  )
}


.rdmlRdesCollectRecords <- function(
    x,
    run,
    rdesType) {

  samples <- .rdmlPropKeyed(
    x,
    "sample"
  )

  targets <- .rdmlPropKeyed(
    x,
    "target"
  )

  records <- list()

  for (react in .rdmlPropList(
    run,
    "react"
  )) {
    sampleId <- .rdmlIdChr(
      react$sample
    )

    sampleObj <- samples[[sampleId]]

    well <- .rdmlRdesWellForExport(
      react,
      run$pcrFormat
    )

    if (
      is.na(well) ||
      !nzchar(well)
    ) {
      stop(
        "Cannot derive RDES Well from react ",
        .rdmlIdChr(react$id),
        call. = FALSE
      )
    }

    for (dataObj in .rdmlPropList(
      react,
      "data"
    )) {
      curve <- if (
        identical(rdesType, "adp")
      ) {
        dataObj$adp
      } else {
        dataObj$mdp
      }

      if (.rdmlIsMissing(curve)) {
        next
      }

      targetId <- .rdmlIdChr(
        dataObj$targetId
      )

      targetObj <- targets[[targetId]]

      if (is.null(targetObj)) {
        stop(
          "RDES export requires target metadata for '",
          targetId,
          "'",
          call. = FALSE
        )
      }

      dyeId <- .rdmlIdChr(
        targetObj$dyeId
      )

      if (
        is.na(dyeId) ||
        !nzchar(dyeId)
      ) {
        stop(
          "RDES export requires Dye for target '",
          targetId,
          "'",
          call. = FALSE
        )
      }

      sampleTypeValue <- .rdmlSampleType(
        sampleObj,
        targetId
      )

      if (
        is.na(sampleTypeValue) ||
        !nzchar(sampleTypeValue)
      ) {
        sampleTypeValue <- "unkn"
      }

      targetTypeValue <- .rdmlEnumChr(
        targetObj$type
      )

      if (
        is.na(targetTypeValue) ||
        !nzchar(targetTypeValue)
      ) {
        targetTypeValue <- "toi"
      }

      points <- getFData(
        dataObj,
        dpType = rdesType
      )

      metric <- if (
        identical(rdesType, "adp")
      ) {
        dataObj$cq
      } else {
        dataObj$meltTemp
      }

      records[[length(records) + 1L]] <- list(
        well = well,
        sample = sampleId,
        sampleType = sampleTypeValue,
        target = targetId,
        targetType = targetTypeValue,
        dye = dyeId,
        metric = metric,
        points = points
      )
    }
  }

  if (!length(records)) {
    stop(
      "Selected run has no ",
      rdesType,
      " data",
      call. = FALSE
    )
  }

  metadata <- data.table::rbindlist(
    lapply(
      records,
      function(record) {
        data.table::data.table(
          reactId = record$well,
          sample = record$sample,
          sampleType = record$sampleType,
          target = record$target,
          targetType = record$targetType,
          targetDyeId = record$dye
        )
      }
    )
  )

  .rdmlRdesValidateMetadata(
    metadata
  )

  records
}


.rdmlRdesBuildLines <- function(
    x,
    run,
    rdesType) {

  records <- .rdmlRdesCollectRecords(
    x,
    run,
    rdesType
  )

  coordinateName <- if (
    identical(rdesType, "adp")
  ) {
    "cyc"
  } else {
    "tmp"
  }

  coordinates <- sort(
    unique(
      unlist(
        lapply(
          records,
          function(record) {
            as.numeric(
              record$points[[
                coordinateName
              ]]
            )
          }
        ),
        use.names = FALSE
      )
    )
  )

  if (!length(coordinates)) {
    stop(
      "RDES export found no fluorescence coordinates",
      call. = FALSE
    )
  }

  if (
    identical(rdesType, "adp") &&
    any(
      abs(
        coordinates -
          round(coordinates)
      ) > sqrt(.Machine$double.eps)
    )
  ) {
    stop(
      "RDES does not support fractional amplification cycles",
      call. = FALSE
    )
  }

  coordinateLabels <- if (
    identical(rdesType, "adp")
  ) {
    as.character(
      as.integer(
        round(coordinates)
      )
    )
  } else {
    vapply(
      coordinates,
      .rdmlRdesFormatNumber,
      character(1)
    )
  }

  if (anyDuplicated(coordinateLabels)) {
    stop(
      "RDES coordinate labels are not unique after formatting",
      call. = FALSE
    )
  }

  metricName <- if (
    identical(rdesType, "adp")
  ) {
    "Cq"
  } else {
    "Tm"
  }

  header <- c(
    "Well",
    "Sample",
    "Sample Type",
    "Target",
    "Target Type",
    "Dye",
    metricName,
    coordinateLabels
  )

  lines <- character(
    length(records) + 1L
  )

  lines[[1L]] <- paste(
    header,
    collapse = "\t"
  )

  for (i in seq_along(records)) {
    record <- records[[i]]
    points <- record$points

    currentCoordinate <- as.numeric(
      points[[coordinateName]]
    )

    currentLabels <- if (
      identical(rdesType, "adp")
    ) {
      as.character(
        as.integer(
          round(currentCoordinate)
        )
      )
    } else {
      vapply(
        currentCoordinate,
        .rdmlRdesFormatNumber,
        character(1)
      )
    }

    if (anyDuplicated(currentLabels)) {
      stop(
        "RDES curve contains duplicated coordinates for well ",
        record$well,
        ", target ",
        record$target,
        call. = FALSE
      )
    }

    fluorescence <- rep(
      "",
      length(coordinateLabels)
    )

    pos <- match(
      currentLabels,
      coordinateLabels
    )

    values <- as.numeric(
      points[["fluor"]]
    )

    fluorescence[pos] <- vapply(
      values,
      .rdmlRdesFormatNumber,
      character(1)
    )

    row <- c(
      .rdmlRdesCleanText(
        record$well
      ),
      .rdmlRdesCleanText(
        record$sample
      ),
      .rdmlRdesCleanText(
        record$sampleType
      ),
      .rdmlRdesCleanText(
        record$target
      ),
      .rdmlRdesCleanText(
        record$targetType
      ),
      .rdmlRdesCleanText(
        record$dye
      ),
      .rdmlRdesFormatNumber(
        record$metric
      ),
      fluorescence
    )

    lines[[i + 1L]] <- paste(
      row,
      collapse = "\t"
    )
  }

  lines
}


.rdmlRdesWriteLines <- function(
    lines,
    fileName,
    overwrite = FALSE) {

  fileName <- .rdmlOutputPath(
    fileName
  )

  if (
    file.exists(fileName) &&
    !overwrite
  ) {
    stop(
      "Output file already exists: ",
      fileName,
      call. = FALSE
    )
  }

  text <- paste0(
    paste(
      enc2utf8(lines),
      collapse = "\n"
    ),
    "\n"
  )

  con <- file(
    fileName,
    open = "wb"
  )

  on.exit(
    close(con),
    add = TRUE
  )

  writeBin(
    charToRaw(
      enc2utf8(text)
    ),
    con = con
  )

  invisible(fileName)
}


.rdmlRdesPairNames <- function(fileName) {
  ext <- tools::file_ext(
    fileName
  )

  base <- if (nzchar(ext)) {
    tools::file_path_sans_ext(
      fileName
    )
  } else {
    fileName
  }

  suffix <- if (nzchar(ext)) {
    paste0(
      ".",
      ext
    )
  } else {
    ".tsv"
  }

  c(
    amplification = paste0(
      base,
      "_amplification",
      suffix
    ),
    melting = paste0(
      base,
      "_melting",
      suffix
    )
  )
}


.rdmlWriteRdes <- function(
    x,
    fileName,
    overwrite = FALSE,
    expId = NULL,
    runId = NULL,
    rdesType = c(
      "auto",
      "adp",
      "mdp",
      "both"
    ),
    ...) {

  rdesType <- match.arg(
    rdesType
  )

  selected <- .rdmlRdesSelectRun(
    x,
    expId = expId,
    runId = runId
  )

  available <- .rdmlRdesRunTypes(
    selected$run
  )

  if (identical(rdesType, "auto")) {
    present <- names(
      available
    )[
      available
    ]

    if (!length(present)) {
      stop(
        "Selected run contains no amplification or melting data",
        call. = FALSE
      )
    }

    if (length(present) > 1L) {
      stop(
        "Selected run contains both amplification and melting data. ",
        "Use `rdesType = \"both\"`, `\"adp\"`, or `\"mdp\"`.",
        call. = FALSE
      )
    }

    rdesType <- present[[1L]]
  }

  if (
    identical(rdesType, "adp") &&
    !available[["adp"]]
  ) {
    stop(
      "Selected run contains no amplification data",
      call. = FALSE
    )
  }

  if (
    identical(rdesType, "mdp") &&
    !available[["mdp"]]
  ) {
    stop(
      "Selected run contains no melting data",
      call. = FALSE
    )
  }

  if (identical(rdesType, "both")) {
    if (
      !available[["adp"]] ||
      !available[["mdp"]]
    ) {
      stop(
        "`rdesType = \"both\"` requires both amplification and melting data",
        call. = FALSE
      )
    }

    paths <- .rdmlRdesPairNames(
      fileName
    )

    if (!overwrite) {
      existing <- paths[
        file.exists(paths)
      ]

      if (length(existing)) {
        stop(
          "RDES output file(s) already exist: ",
          paste(
            existing,
            collapse = ", "
          ),
          call. = FALSE
        )
      }
    }

    adpLines <- .rdmlRdesBuildLines(
      x,
      selected$run,
      "adp"
    )

    mdpLines <- .rdmlRdesBuildLines(
      x,
      selected$run,
      "mdp"
    )

    written <- c(
      amplification = .rdmlRdesWriteLines(
        adpLines,
        paths[["amplification"]],
        overwrite = overwrite
      ),
      melting = .rdmlRdesWriteLines(
        mdpLines,
        paths[["melting"]],
        overwrite = overwrite
      )
    )

    return(
      invisible(written)
    )
  }

  lines <- .rdmlRdesBuildLines(
    x,
    selected$run,
    rdesType
  )

  .rdmlRdesWriteLines(
    lines,
    fileName,
    overwrite = overwrite
  )
}
