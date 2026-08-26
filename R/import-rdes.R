# RDES import ---------------------------------------------------------------
#
# Real-time PCR Data Essential Spreadsheet Format (RDES) v1.0.
# One file contains one run and one curve type: amplification (Cq) OR melting
# (Tm). A companion file may be supplied to import both types into the same run.

.rdmlRdesAllowedSampleTypes <- c(
  "unkn", "ntc", "nac", "std",
  "ntp", "nrt", "pos", "opt"
)

.rdmlRdesAllowedTargetTypes <- c(
  "toi", "ref"
)


.rdmlRdesReadUtf8 <- function(
    fileName,
    strict = FALSE) {
  
  checkmate::assertString(fileName)
  checkmate::assertFlag(strict)
  
  size <- file.info(fileName)$size
  
  if (
    length(size) != 1L ||
    is.na(size) ||
    size <= 0
  ) {
    stop(
      "RDES file is empty: ",
      fileName,
      call. = FALSE
    )
  }
  
  raw <- readBin(
    fileName,
    what = "raw",
    n = size
  )
  
  text <- rawToChar(raw)
  
  if (!validUTF8(text)) {
    stop(
      "RDES requires UTF-8 encoding: ",
      fileName,
      call. = FALSE
    )
  }
  
  if (grepl("\r", text, fixed = TRUE)) {
    message <- paste0(
      "RDES requires Linux LF newlines; CR/CRLF found in ",
      fileName
    )
    
    if (strict) {
      stop(
        message,
        call. = FALSE
      )
    }
    
    warning(
      message,
      "; accepting and normalizing it",
      call. = FALSE
    )
    
    text <- gsub(
      "\r\n",
      "\n",
      text,
      fixed = TRUE
    )
    
    text <- gsub(
      "\r",
      "\n",
      text,
      fixed = TRUE
    )
  }
  
  enc2utf8(text)
}


.rdmlRdesParseNumeric <- function(
    x,
    field,
    allowEmpty = TRUE) {
  
  x <- as.character(x)
  empty <- is.na(x) | x == ""
  
  if (!allowEmpty && any(empty)) {
    stop(
      "RDES field '",
      field,
      "' contains empty values",
      call. = FALSE
    )
  }
  
  out <- rep(
    NA_real_,
    length(x)
  )
  
  idx <- which(!empty)
  
  if (length(idx)) {
    parsed <- suppressWarnings(
      base::as.numeric(
        x[idx]
      )
    )
    
    bad <- is.na(parsed)
    
    if (any(bad)) {
      stop(
        "RDES field '",
        field,
        "' contains non-numeric value(s): ",
        paste(
          unique(x[idx][bad]),
          collapse = ", "
        ),
        call. = FALSE
      )
    }
    
    out[idx] <- parsed
  }
  
  out
}


.rdmlRdesParseTm <- function(
    x,
    strict = FALSE) {
  
  checkmate::assertFlag(strict)
  
  x <- as.character(x)
  
  lapply(
    x,
    function(value) {
      if (
        is.na(value) ||
        !nzchar(value)
      ) {
        return(numeric())
      }
      
      if (
        strict &&
        grepl(
          "\\s",
          value,
          perl = TRUE
        )
      ) {
        stop(
          "RDES Tm values must be separated by ';' without spaces: ",
          value,
          call. = FALSE
        )
      }
      
      pieces <- strsplit(
        gsub("\\s+", "", value, perl = TRUE),
        ";",
        fixed = TRUE
      )[[1L]]
      
      parsed <- suppressWarnings(
        base::as.numeric(pieces)
      )
      
      if (
        anyNA(parsed) ||
        !length(parsed)
      ) {
        stop(
          "Invalid RDES Tm value: ",
          value,
          call. = FALSE
        )
      }
      
      parsed
    }
  )
}


.rdmlRdesDefaultRunId <- function(fileName) {
  stem <- tools::file_path_sans_ext(
    basename(fileName)
  )
  
  stem <- sub(
    "([_-]?(amplification|amp|melting|melt))$",
    "",
    stem,
    ignore.case = TRUE,
    perl = TRUE
  )
  
  if (!nzchar(stem)) {
    "run1"
  } else {
    stem
  }
}


.rdmlRdesValidateMetadata <- function(
    description,
    allowDuplicateData = FALSE) {
  
  checkmate::assertFlag(
    allowDuplicateData
  )
  
  if (!nrow(description)) {
    stop(
      "RDES file contains no data rows",
      call. = FALSE
    )
  }
  
  # Same sample name must always have the same sample type.
  sampleConsistency <- description[
    ,
    .(
      nTypes = data.table::uniqueN(sampleType)
    ),
    by = sample
  ]
  
  badSamples <- sampleConsistency[
    nTypes > 1L,
    sample
  ]
  
  if (length(badSamples)) {
    stop(
      "RDES sample(s) have inconsistent Sample Type: ",
      paste(
        badSamples,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  # Same target name must always have same target type and dye.
  targetConsistency <- description[
    ,
    .(
      nTypes = data.table::uniqueN(targetType),
      nDyes = data.table::uniqueN(targetDyeId)
    ),
    by = target
  ]
  
  badTargets <- targetConsistency[
    nTypes > 1L | nDyes > 1L,
    target
  ]
  
  if (length(badTargets)) {
    stop(
      "RDES target(s) have inconsistent Target Type or Dye: ",
      paste(
        badTargets,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  # Multiplex rows sharing one well must keep columns 1-3 identical.
  wellConsistency <- description[
    ,
    .(
      nSamples = data.table::uniqueN(sample),
      nTypes = data.table::uniqueN(sampleType)
    ),
    by = reactId
  ]
  
  badWells <- wellConsistency[
    nSamples > 1L | nTypes > 1L,
    reactId
  ]
  
  if (length(badWells)) {
    stop(
      "RDES well(s) contain inconsistent Sample / Sample Type: ",
      paste(
        badWells,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  if (!allowDuplicateData) {
    duplicateData <- description[
      ,
      .N,
      by = .(
        reactId,
        target
      )
    ][
      N > 1L
    ]
    
    if (nrow(duplicateData)) {
      stop(
        "RDES contains duplicate Well + Target rows; ",
        "RDML7 stores one dataType per target in each reaction",
        call. = FALSE
      )
    }
  }
  
  invisible(TRUE)
}


.rdmlRdesParseFile <- function(
    fileName,
    expId,
    runId,
    strict = FALSE) {
  
  text <- .rdmlRdesReadUtf8(
    fileName,
    strict = strict
  )
  
  # Parse the TSV ourselves instead of asking read.delim() to infer table
  # width and header names. Some valid RDES writers leave a trailing TAB at
  # the end of each line. read.table/read.delim can then create an artificial
  # unnamed column, which later looks like an invalid raw-data coordinate.
  #
  # Adding a sentinel before strsplit() preserves trailing empty fields.
  splitRdesLine <- function(line) {
    fields <- strsplit(
      paste0(
        line,
        "\t.__RDML7_END__"
      ),
      "\t",
      fixed = TRUE
    )[[1L]]
    
    fields[
      -length(fields)
    ]
  }
  
  lines <- strsplit(
    text,
    "\n",
    fixed = TRUE
  )[[1L]]
  
  # A final LF creates a final empty string. It is not a table row.
  while (
    length(lines) &&
    !nzchar(
      lines[[length(lines)]]
    )
  ) {
    lines <- lines[
      -length(lines)
    ]
  }
  
  if (length(lines) < 2L) {
    stop(
      "RDES requires one header row and at least one data row",
      call. = FALSE
    )
  }
  
  header <- splitRdesLine(
    lines[[1L]]
  )
  
  # Accept an optional UTF-8 BOM in the first cell.
  header[[1L]] <- sub(
    "^\ufeff",
    "",
    header[[1L]]
  )
  
  # Ignore only truly empty trailing columns. They commonly arise from a
  # trailing TAB and are not part of the RDES table. Empty cells inside the
  # header remain invalid and are checked below.
  while (
    length(header) > 7L &&
    !nzchar(
      header[[length(header)]]
    )
  ) {
    header <- header[
      -length(header)
    ]
  }
  
  if (length(header) < 8L) {
    stop(
      "RDES requires seven metadata columns and at least one raw-data column",
      call. = FALSE
    )
  }
  
  nColumns <- length(header)
  
  rows <- lapply(
    lines[-1L],
    splitRdesLine
  )
  
  for (i in seq_along(rows)) {
    row <- rows[[i]]
    
    if (length(row) > nColumns) {
      extra <- row[
        seq.int(
          nColumns + 1L,
          length(row)
        )
      ]
      
      # Extra empty fields are another representation of trailing TABs.
      if (all(!nzchar(extra))) {
        row <- row[
          seq_len(nColumns)
        ]
      } else {
        stop(
          "RDES row ",
          i + 1L,
          " contains ",
          length(row),
          " fields, but the header contains ",
          nColumns,
          call. = FALSE
        )
      }
    }
    
    if (length(row) < nColumns) {
      row <- c(
        row,
        rep(
          "",
          nColumns - length(row)
        )
      )
    }
    
    rows[[i]] <- row
  }
  
  matrixData <- do.call(
    rbind,
    rows
  )
  
  tab <- as.data.frame(
    matrixData,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  names(tab) <- header
  
  expectedFirstSix <- c(
    "Well",
    "Sample",
    "Sample Type",
    "Target",
    "Target Type",
    "Dye"
  )
  
  if (!identical(
    header[1:6],
    expectedFirstSix
  )) {
    stop(
      "Invalid RDES header. First six columns must be: ",
      paste(
        expectedFirstSix,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  metricName <- header[[7L]]
  
  if (!metricName %in% c("Cq", "Tm")) {
    stop(
      "RDES column 7 must be Cq or Tm, not '",
      metricName,
      "'",
      call. = FALSE
    )
  }
  
  fdataType <- if (
    identical(metricName, "Cq")
  ) {
    "adp"
  } else {
    "mdp"
  }
  
  coordinateNames <- header[-seq_len(7L)]
  
  if (
    any(!nzchar(coordinateNames)) ||
    anyDuplicated(coordinateNames)
  ) {
    stop(
      "RDES raw-data coordinate column names must be non-empty and unique",
      call. = FALSE
    )
  }
  
  coordinate <- .rdmlRdesParseNumeric(
    coordinateNames,
    field = if (
      identical(fdataType, "adp")
    ) {
      "cycle header"
    } else {
      "temperature header"
    },
    allowEmpty = FALSE
  )
  
  if (
    identical(fdataType, "adp") &&
    any(
      abs(
        coordinate -
        round(coordinate)
      ) > sqrt(.Machine$double.eps)
    )
  ) {
    stop(
      "RDES amplification cycle headers must be integers",
      call. = FALSE
    )
  }
  
  # Normalize and validate well labels.
  wells <- as.character(tab[["Well"]])
  
  if (any(!nzchar(wells))) {
    stop(
      "RDES Well values must not be empty",
      call. = FALSE
    )
  }
  
  upperWells <- toupper(wells)
  
  if (!identical(wells, upperWells)) {
    if (strict) {
      stop(
        "RDES Well values must use upper-case letters",
        call. = FALSE
      )
    }
    
    warning(
      "RDES Well values were converted to upper case",
      call. = FALSE
    )
    
    wells <- upperWells
  }
  
  validWell <- grepl(
    "^(?:[A-Z]+[1-9][0-9]*|[1-9][0-9]*)$",
    wells,
    perl = TRUE
  )
  
  if (!all(validWell)) {
    stop(
      "Invalid RDES Well value(s): ",
      paste(
        unique(wells[!validWell]),
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  sample <- as.character(
    tab[["Sample"]]
  )
  
  target <- as.character(
    tab[["Target"]]
  )
  
  dye <- as.character(
    tab[["Dye"]]
  )
  
  if (any(!nzchar(sample))) {
    stop(
      "RDES Sample values must not be empty",
      call. = FALSE
    )
  }
  
  if (any(!nzchar(target))) {
    stop(
      "RDES Target values must not be empty",
      call. = FALSE
    )
  }
  
  if (any(!nzchar(dye))) {
    stop(
      "RDES Dye values must not be empty",
      call. = FALSE
    )
  }
  
  sampleType <- tolower(
    as.character(
      tab[["Sample Type"]]
    )
  )
  
  sampleType[
    !nzchar(sampleType)
  ] <- "unkn"
  
  badSampleType <- !sampleType %in%
    .rdmlRdesAllowedSampleTypes
  
  if (any(badSampleType)) {
    stop(
      "Invalid RDES Sample Type value(s): ",
      paste(
        unique(sampleType[badSampleType]),
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  targetType <- tolower(
    as.character(
      tab[["Target Type"]]
    )
  )
  
  targetType[
    !nzchar(targetType)
  ] <- "toi"
  
  badTargetType <- !targetType %in%
    .rdmlRdesAllowedTargetTypes
  
  if (any(badTargetType)) {
    stop(
      "Invalid RDES Target Type value(s): ",
      paste(
        unique(targetType[badTargetType]),
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  # Parse raw fluorescence matrix without accepting comma decimals or other
  # non-RDES number formats.
  fluorescenceColumns <- lapply(
    tab[-seq_len(7L)],
    .rdmlRdesParseNumeric,
    field = "raw fluorescence",
    allowEmpty = TRUE
  )
  
  fluorescence <- do.call(
    cbind,
    fluorescenceColumns
  )
  
  if (!is.matrix(fluorescence)) {
    fluorescence <- matrix(
      fluorescence,
      nrow = nrow(tab)
    )
  }
  
  fdataName <- make.unique(
    paste(
      "rdes",
      wells,
      target,
      dye,
      sep = "_"
    ),
    sep = "_"
  )
  
  fdata <- data.table::data.table(
    coordinate = coordinate
  )
  
  data.table::setnames(
    fdata,
    "coordinate",
    if (
      identical(fdataType, "adp")
    ) {
      "cyc"
    } else {
      "tmp"
    }
  )
  
  for (i in seq_len(nrow(tab))) {
    fdata[[
      fdataName[[i]]
    ]] <- as.numeric(
      fluorescence[i, ]
    )
  }
  
  description <- data.table::data.table(
    fdataName = fdataName,
    expId = rep(
      expId,
      nrow(tab)
    ),
    runId = rep(
      runId,
      nrow(tab)
    ),
    reactId = wells,
    sample = sample,
    sampleType = sampleType,
    target = target,
    targetType = targetType,
    targetDyeId = dye
  )
  
  tmValues <- vector(
    "list",
    nrow(tab)
  )
  
  if (identical(fdataType, "adp")) {
    cq <- .rdmlRdesParseNumeric(
      tab[["Cq"]],
      field = "Cq",
      allowEmpty = TRUE
    )
    
    invalidCq <- !is.na(cq) &
      cq < 0 &
      cq != -1
    
    if (any(invalidCq)) {
      stop(
        "RDES Cq must be non-negative, empty, or -1.0 for failed Cq calculation",
        call. = FALSE
      )
    }
    
    description[
      ,
      cq := cq
    ]
  } else {
    tmValues <- .rdmlRdesParseTm(
      tab[["Tm"]],
      strict = strict
    )
    
    multipleTm <- lengths(
      tmValues
    ) > 1L
    
    if (any(multipleTm)) {
      warning(
        sum(multipleTm),
        " RDES row(s) contain multiple Tm values. ",
        "Current rdmlType stores a single meltTemp; only the first Tm is retained.",
        call. = FALSE
      )
    }
    
    firstTm <- vapply(
      tmValues,
      function(value) {
        if (length(value)) {
          value[[1L]]
        } else {
          NA_real_
        }
      },
      numeric(1)
    )
    
    description[
      ,
      meltTemp := firstTm
    ]
  }
  
  .rdmlRdesValidateMetadata(
    description
  )
  
  list(
    fdataType = fdataType,
    fdata = fdata,
    description = description,
    tmValues = tmValues
  )
}


.rdmlRdesApplyMetadata <- function(
    x,
    description) {
  
  targets <- .rdmlPropKeyed(
    x,
    "target"
  )
  
  experiments <- .rdmlPropKeyed(
    x,
    "experiment"
  )
  
  for (i in seq_len(nrow(description))) {
    row <- description[i]
    
    targetId <- as.character(
      row[["target"]][[1L]]
    )
    
    targetTypeValue <- as.character(
      row[["targetType"]][[1L]]
    )
    
    targetObj <- targets[[targetId]]
    
    if (!is.null(targetObj)) {
      S7::prop(
        targetObj,
        "type"
      ) <- targetTypeType(
        targetTypeValue
      )
      
      targets[[targetId]] <- targetObj
    }
    
    if (
      "meltTemp" %in% names(row) &&
      !is.na(
        row[["meltTemp"]][[1L]]
      )
    ) {
      expId <- as.character(
        row[["expId"]][[1L]]
      )
      
      runId <- as.character(
        row[["runId"]][[1L]]
      )
      
      reactId <- as.character(
        row[["reactId"]][[1L]]
      )
      
      experiment <- experiments[[expId]]
      runs <- .rdmlPropKeyed(
        experiment,
        "run"
      )
      run <- runs[[runId]]
      reacts <- .rdmlPropKeyed(
        run,
        "react"
      )
      react <- reacts[[reactId]]
      dataList <- .rdmlPropKeyed(
        react,
        "data"
      )
      dataObj <- dataList[[targetId]]
      
      S7::prop(
        dataObj,
        "meltTemp"
      ) <- as.numeric(
        row[["meltTemp"]][[1L]]
      )
      
      dataList[[targetId]] <- dataObj
      react <- .rdmlSetPropList(
        react,
        "data",
        dataList
      )
      
      reacts[[reactId]] <- react
      run <- .rdmlSetPropList(
        run,
        "react",
        reacts
      )
      
      runs[[runId]] <- run
      experiment <- .rdmlSetPropList(
        experiment,
        "run",
        runs
      )
      
      experiments[[expId]] <- experiment
    }
  }
  
  x <- .rdmlSetPropList(
    x,
    "target",
    targets
  )
  
  x <- .rdmlSetPropList(
    x,
    "experiment",
    experiments
  )
  
  x
}


.rdmlImportRdes <- function(
    fileName,
    showProgress = TRUE,
    companionFile = NULL,
    expId = "RDES",
    runId = NULL,
    strict = FALSE,
    ...) {
  
  checkmate::assertString(fileName)
  checkmate::assertFlag(showProgress)
  checkmate::assertString(expId)
  checkmate::assertFlag(strict)
  
  if (!is.null(companionFile)) {
    checkmate::assertString(companionFile)
    
    if (!file.exists(companionFile)) {
      stop(
        "RDES companion file does not exist: ",
        companionFile,
        call. = FALSE
      )
    }
  }
  
  if (is.null(runId)) {
    runId <- .rdmlRdesDefaultRunId(
      fileName
    )
  } else {
    checkmate::assertString(runId)
  }
  
  primary <- .rdmlRdesParseFile(
    fileName,
    expId = expId,
    runId = runId,
    strict = strict
  )
  
  parsed <- list(
    primary
  )
  
  if (!is.null(companionFile)) {
    companion <- .rdmlRdesParseFile(
      companionFile,
      expId = expId,
      runId = runId,
      strict = strict
    )
    
    if (identical(
      primary$fdataType,
      companion$fdataType
    )) {
      stop(
        "RDES primary and companion files both contain ",
        primary$fdataType,
        " data; one must be amplification and the other melting",
        call. = FALSE
      )
    }
    
    combinedDescription <- data.table::rbindlist(
      list(
        primary$description,
        companion$description
      ),
      use.names = TRUE,
      fill = TRUE
    )
    
    .rdmlRdesValidateMetadata(
      combinedDescription,
      allowDuplicateData = TRUE
    )
    
    parsed[[2L]] <- companion
  }
  
  x <- .rdmlNewImport(
    publisher = "RDES",
    serialNumber = "1"
  )
  
  for (item in parsed) {
    x <- setFData(
      x,
      item$fdata,
      item$description,
      fdataType = item$fdataType
    )
    
    x <- .rdmlRdesApplyMetadata(
      x,
      item$description
    )
  }
  
  # RDES contains Well labels but does not contain a complete PCR plate-format
  # definition. setFData() creates a default pcrFormat while constructing a run;
  # remove it so numeric rotor positions such as "1" remain "1" instead of
  # being reinterpreted as A1 by .rdmlReactPosition().
  experiments <- .rdmlPropKeyed(
    x,
    "experiment"
  )
  
  experiment <- experiments[[expId]]
  runs <- .rdmlPropKeyed(
    experiment,
    "run"
  )
  run <- runs[[runId]]
  
  S7::prop(
    run,
    "pcrFormat"
  ) <- NA
  
  runs[[runId]] <- run
  experiment <- .rdmlSetPropList(
    experiment,
    "run",
    runs
  )
  experiments[[expId]] <- experiment
  x <- .rdmlSetPropList(
    x,
    "experiment",
    experiments
  )
  
  if (showProgress) {
    types <- vapply(
      parsed,
      function(item) item$fdataType,
      character(1)
    )
    
    cat(
      sprintf(
        "\nRDES: imported run '%s' (%s)\n",
        runId,
        paste(
          types,
          collapse = " + "
        )
      )
    )
  }
  
  x
}
