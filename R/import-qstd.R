# Innova PCR Analyzer QSTD importer ---------------------------------------

.qstdAbort <- function(message, fileName = NULL) {
  if (!is.null(fileName)) {
    message <- paste0(message, ": ", basename(fileName))
  }
  stop(message, call. = FALSE)
}


.qstdUInt32 <- function(bytes, offset) {
  if (offset < 0L || offset + 4L > length(bytes)) {
    return(NA_real_)
  }

  sum(
    as.numeric(bytes[offset + seq_len(4L)]) * 256^(0:3)
  )
}


.qstdQString <- function(bytes, offset, maxChars = 4096L) {
  size <- .qstdUInt32(bytes, offset)

  if (
    is.na(size) ||
      size > maxChars ||
      offset + 4 + 2 * size > length(bytes)
  ) {
    return(NULL)
  }

  if (size == 0) {
    return("")
  }

  lo <- as.integer(
    bytes[offset + 4L + 2L * seq_len(size) - 1L]
  )
  hi <- as.integer(
    bytes[offset + 4L + 2L * seq_len(size)]
  )

  value <- tryCatch(
    intToUtf8(lo + 256L * hi),
    error = function(e) NA_character_
  )

  if (
    is.na(value) ||
      grepl("[[:cntrl:]]", value)
  ) {
    return(NULL)
  }

  value
}


.qstdStrings <- function(bytes, from = 0L, to = length(bytes) - 4L) {
  to <- min(as.integer(to), length(bytes) - 4L)
  from <- max(0L, as.integer(from))
  out <- vector("list", 0L)

  for (offset in seq.int(from, to)) {
    size <- .qstdUInt32(bytes, offset)
    if (is.na(size) || size < 1 || size > 256) next

    value <- .qstdQString(bytes, offset, maxChars = 256L)
    if (is.null(value) || !nzchar(value)) next

    out[[length(out) + 1L]] <- data.frame(
      offset = offset,
      size = as.integer(size),
      value = value,
      stringsAsFactors = FALSE
    )
  }

  if (!length(out)) {
    return(data.frame(
      offset = integer(),
      size = integer(),
      value = character(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, out)
}


.qstdSampleType <- function(sample) {
  if (grepl("(^|[^[:alnum:]])(ntc|negative|neg|\u043a-)", sample, ignore.case = TRUE)) {
    return("ntc")
  }
  if (grepl("(^|[^[:alnum:]])(positive|pos|\u043f\u043a\u043e)", sample, ignore.case = TRUE)) {
    return("pos")
  }
  "unkn"
}


.qstdFindCurveOffset <- function(
    bytes,
    from,
    wellCount,
    configuredDyes) {

  last <- length(bytes) - 32L
  offsets <- seq.int(from + ((4L - from %% 4L) %% 4L), last, by = 4L)

  for (offset in offsets) {
    header <- vapply(
      0:3,
      function(i) .qstdUInt32(bytes, offset + 4L * i),
      numeric(1)
    )

    cycles <- header[[4L]]
    if (
      header[[1L]] != 1 ||
        header[[2L]] != wellCount ||
        header[[3L]] != configuredDyes ||
        is.na(cycles) || cycles < 1 || cycles > 500
    ) next

    coordinates <- vapply(
      seq_len(cycles),
      function(i) .qstdUInt32(bytes, offset + 12L + 4L * i),
      numeric(1)
    )

    if (identical(coordinates, as.numeric(seq_len(cycles)))) {
      return(list(offset = offset, cycles = as.integer(cycles)))
    }
  }

  NULL
}


.qstdCurveChannelCount <- function(
    bytes,
    curveOffset,
    cycles,
    wellCount,
    configuredDyes) {

  blockSize <- 8L + 8L * cycles
  start <- curveOffset + 12L

  for (channels in seq_len(configuredDyes)) {
    nextOffset <- start + wellCount * channels * blockSize
    nextHeader <- vapply(
      0:2,
      function(i) .qstdUInt32(bytes, nextOffset + 4L * i),
      numeric(1)
    )
    if (identical(nextHeader, as.numeric(c(1L, wellCount, configuredDyes)))) {
      return(channels)
    }
  }

  NA_integer_
}


.qstdEnrichRDML <- function(x, parsed) {
  experiments <- .rdmlPropKeyed(x, "experiment")
  experiment <- experiments[[parsed[["expId"]]]]
  runs <- .rdmlPropKeyed(experiment, "run")
  run <- runs[[parsed[["runId"]]]]

  S7::prop(experiment, "description") <- parsed[["title"]]
  S7::prop(run, "description") <- paste(
    c(
      parsed[["title"]],
      if (nzchar(parsed[["operator"]])) paste0("Operator: ", parsed[["operator"]]),
      if (nzchar(parsed[["duration"]])) paste0("Duration: ", parsed[["duration"]]),
      if (nzchar(parsed[["sourceDirectory"]])) {
        paste0("Source directory: ", parsed[["sourceDirectory"]])
      }
    ),
    collapse = "; "
  )
  S7::prop(run, "instrument") <- parsed[["instrument"]]
  S7::prop(run, "dataCollectionSoftware") <- dataCollectionSoftwareType(
    name = "PCR Analyzer96",
    version = parsed[["softwareVersion"]]
  )

  operatorId <- NULL
  if (nzchar(parsed[["operator"]])) {
    operatorId <- make.names(parsed[["operator"]])
    S7::prop(run, "experimenter") <- list(idReferenceType(operatorId))
  }

  runDate <- suppressWarnings(
    as.POSIXct(parsed[["startDate"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  )
  if (!is.na(runDate)) {
    S7::prop(run, "runDate") <- runDate
  }

  runs[[parsed[["runId"]]]] <- run
  experiment <- .rdmlSetPropList(experiment, "run", runs)
  experiments[[parsed[["expId"]]]] <- experiment
  x <- .rdmlSetPropList(x, "experiment", experiments)

  if (!is.null(operatorId)) {
    experimenters <- .rdmlPropKeyed(x, "experimenter")
    experimenters[[operatorId]] <- experimenterType(
      id = idType(operatorId),
      firstName = parsed[["operator"]],
      lastName = "Innova operator"
    )
    x <- .rdmlSetPropList(x, "experimenter", experimenters)
  }

  x
}


.rdmlImportQstd <- function(fileName, showProgress = TRUE) {
  bytes <- readBin(
    fileName,
    what = "raw",
    n = file.size(fileName)
  )

  if (
    length(bytes) < 1024L ||
      !identical(as.integer(bytes[1:2]), c(0x61L, 0x61L))
  ) {
    .qstdAbort("Unsupported Innova QSTD header", fileName)
  }

  strings <- .qstdStrings(bytes, to = min(length(bytes) - 4L, 60000L))
  wellRows <- strings[grepl("^[A-Z]+[1-9][0-9]*$", strings[["value"]]), , drop = FALSE]
  wellRows <- wellRows[!duplicated(wellRows[["value"]]), , drop = FALSE]

  if (nrow(wellRows) < 2L) {
    .qstdAbort("QSTD plate layout was not found", fileName)
  }

  positions <- wellRows[["value"]]
  positionIds <- suppressWarnings(
    vapply(positions, .fromPositionToId, numeric(1))
  )
  orderIndex <- order(positionIds)
  wellRows <- wellRows[orderIndex, , drop = FALSE]
  positions <- wellRows[["value"]]
  positionIds <- positionIds[orderIndex]

  if (anyNA(positionIds) || anyDuplicated(positionIds)) {
    .qstdAbort("Invalid QSTD well identifiers", fileName)
  }

  headerStrings <- strings[strings[["offset"]] < min(wellRows[["offset"]]), , drop = FALSE]
  dateValues <- headerStrings[["value"]][
    grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}$", headerStrings[["value"]])
  ]
  title <- headerStrings[["value"]][[1L]]
  sourceDirectory <- headerStrings[["value"]][
    grepl("PCR Analyzer", headerStrings[["value"]], fixed = TRUE)
  ]
  instrumentValues <- headerStrings[["value"]][
    grepl("IRTP", headerStrings[["value"]], ignore.case = TRUE)
  ]
  serialValues <- headerStrings[["value"]][
    grepl("^[A-Z]{2}[0-9A-Z]{8,}$", headerStrings[["value"]])
  ]
  durationValues <- headerStrings[["value"]][
    grepl("minutes?|seconds?", headerStrings[["value"]], ignore.case = TRUE)
  ]
  pathIndex <- which(grepl("PCR Analyzer", headerStrings[["value"]], fixed = TRUE))
  operator <- if (
    length(pathIndex) && pathIndex[[1L]] < nrow(headerStrings)
  ) headerStrings[["value"]][[pathIndex[[1L]] + 1L]] else ""

  dyePattern <- paste0(
    "^(FAM|SYBR|EvaGreen|VIC|JOE|HEX|TET|ABY|NED|TAMRA|Cy3|JUN|ROX|Texas Red|Mustang Purple|Cy5|LIZ|Cy5\\.5)$"
  )
  firstRecordStrings <- strings[
    strings[["offset"]] >= wellRows[["offset"]][[1L]] &
      strings[["offset"]] < wellRows[["offset"]][[2L]],
    ,
    drop = FALSE
  ]
  configuredDyes <- unique(firstRecordStrings[["value"]][
    grepl(dyePattern, firstRecordStrings[["value"]], ignore.case = TRUE)
  ])

  protocolRows <- strings[strings[["value"]] == "Segment1", , drop = FALSE]
  metadataEnd <- if (nrow(protocolRows)) min(protocolRows[["offset"]]) else 60000L
  records <- vector("list", nrow(wellRows))
  lastMetadataOffset <- max(wellRows[["offset"]])

  for (i in seq_len(nrow(wellRows))) {
    start <- wellRows[["offset"]][[i]]
    end <- if (i < nrow(wellRows)) wellRows[["offset"]][[i + 1L]] else metadataEnd
    recordStrings <- strings[
      strings[["offset"]] >= start & strings[["offset"]] < end,
      ,
      drop = FALSE
    ]
    dyeRows <- recordStrings[grepl(dyePattern, recordStrings[["value"]], ignore.case = TRUE), , drop = FALSE]
    firstDye <- if (nrow(dyeRows)) min(dyeRows[["offset"]]) else end
    sampleCandidates <- recordStrings[["value"]][
      recordStrings[["offset"]] > start &
        recordStrings[["offset"]] < firstDye &
        !grepl("^[0-9]{4}-", recordStrings[["value"]])
    ]
    sample <- if (length(sampleCandidates)) sampleCandidates[[1L]] else ""

    targets <- vector("list", 0L)
    if (nrow(dyeRows)) {
      for (j in seq_len(nrow(dyeRows))) {
        targetOffset <- dyeRows[["offset"]][[j]] + 4L + 2L * dyeRows[["size"]][[j]]
        target <- .qstdQString(bytes, targetOffset, maxChars = 256L)
        if (!is.null(target) && nzchar(target)) {
          targets[[length(targets) + 1L]] <- data.frame(
            dye = dyeRows[["value"]][[j]],
            target = target,
            stringsAsFactors = FALSE
          )
        }
      }
    }

    records[[i]] <- list(
      position = positions[[i]],
      reactId = as.integer(positionIds[[i]]),
      sample = sample,
      targets = if (length(targets)) do.call(rbind, targets) else NULL
    )
    lastMetadataOffset <- max(lastMetadataOffset, start)
  }

  activeDyes <- configuredDyes[
    configuredDyes %in% unique(unlist(lapply(records, function(x) {
      if (is.null(x$targets)) character() else x$targets$dye
    })))
  ]
  if (!length(activeDyes)) {
    .qstdAbort("QSTD contains no assigned targets", fileName)
  }

  curveInfo <- .qstdFindCurveOffset(
    bytes,
    from = lastMetadataOffset,
    wellCount = nrow(wellRows),
    configuredDyes = length(configuredDyes)
  )
  if (is.null(curveInfo)) {
    .qstdAbort("QSTD amplification data block was not found", fileName)
  }

  curveChannelCount <- .qstdCurveChannelCount(
    bytes,
    curveOffset = curveInfo[["offset"]],
    cycles = curveInfo[["cycles"]],
    wellCount = nrow(wellRows),
    configuredDyes = length(configuredDyes)
  )
  if (is.na(curveChannelCount) || curveChannelCount < length(activeDyes)) {
    .qstdAbort("QSTD fluorescence channel layout is not supported", fileName)
  }

  cursor <- curveInfo[["offset"]] + 12L
  curveData <- vector("list", nrow(wellRows))
  for (wellIndex in seq_len(nrow(wellRows))) {
    curveData[[wellIndex]] <- vector("list", curveChannelCount)
    for (dyeIndex in seq_len(curveChannelCount)) {
      coordinateCount <- .qstdUInt32(bytes, cursor)
      if (coordinateCount != curveInfo[["cycles"]]) {
        .qstdAbort("Invalid QSTD cycle vector", fileName)
      }
      cursor <- cursor + 4L + 4L * coordinateCount

      fluorCount <- .qstdUInt32(bytes, cursor)
      if (fluorCount != curveInfo[["cycles"]]) {
        .qstdAbort("Invalid QSTD fluorescence vector", fileName)
      }
      fluor <- vapply(
        seq_len(fluorCount),
        function(k) .qstdUInt32(bytes, cursor + 4L * k),
        numeric(1)
      )
      cursor <- cursor + 4L + 4L * fluorCount
      curveData[[wellIndex]][[dyeIndex]] <- fluor
    }
  }

  expId <- tools::file_path_sans_ext(basename(fileName))
  runId <- if (length(dateValues)) dateValues[[1L]] else "Innova run"
  descriptionRows <- vector("list", 0L)
  fluorescence <- data.frame(cyc = seq_len(curveInfo[["cycles"]]), check.names = FALSE)

  for (i in seq_along(records)) {
    record <- records[[i]]
    if (is.null(record$targets) || !nrow(record$targets)) next
    sample <- if (nzchar(record$sample)) record$sample else paste0("Well ", record$position)

    for (j in seq_len(nrow(record$targets))) {
      dye <- record$targets$dye[[j]]
      target <- record$targets$target[[j]]
      dyeIndex <- match(dye, activeDyes)
      if (is.na(dyeIndex)) next

      fdataName <- make.unique(
        c(names(fluorescence), paste(record$position, target, dye, sep = "_"))
      )[[ncol(fluorescence) + 1L]]
      fluorescence[[fdataName]] <- curveData[[i]][[dyeIndex]]
      descriptionRows[[length(descriptionRows) + 1L]] <- data.frame(
        fdataName = fdataName,
        expId = expId,
        runId = runId,
        reactId = record$reactId,
        position = record$position,
        sample = sample,
        sampleType = .qstdSampleType(sample),
        target = target,
        targetDyeId = dye,
        targetType = "toi",
        stringsAsFactors = FALSE
      )
    }
  }

  if (!length(descriptionRows)) {
    .qstdAbort("QSTD contains no importable reactions", fileName)
  }

  description <- do.call(rbind, descriptionRows)
  versionMatch <- regmatches(
    sourceDirectory[[1L]],
    regexpr("[0-9]+(\\.[0-9]+)+", sourceDirectory[[1L]])
  )
  parsed <- list(
    expId = expId,
    runId = runId,
    title = title,
    operator = operator,
    duration = if (length(durationValues)) durationValues[[1L]] else "",
    sourceDirectory = if (length(sourceDirectory)) sourceDirectory[[1L]] else "",
    instrument = if (length(instrumentValues)) instrumentValues[[1L]] else "INNOVA IRTP 96",
    serialNumber = if (length(serialValues)) serialValues[[1L]] else "unknown",
    softwareVersion = if (length(versionMatch) && nzchar(versionMatch)) versionMatch else "unknown",
    startDate = if (length(dateValues)) dateValues[[1L]] else ""
  )

  importData <- newRDMLImportData(
    series = list(
      newRDMLImportSeries(
        fdataType = "adp",
        fdata = data.table::as.data.table(fluorescence),
        description = data.table::as.data.table(description)
      )
    ),
    publisher = "INNOVA Bio-Meditech",
    serialNumber = parsed[["serialNumber"]],
    format = "innova-qstd",
    metadata = parsed
  )

  x <- buildRDMLImport(importData, loss = "allow")
  .qstdEnrichRDML(x, parsed)
}
