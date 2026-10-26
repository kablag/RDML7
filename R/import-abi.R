# fromABI importer -----------------------------------------------------------

.rdmlImportAbi <- function(fileName, showProgress = TRUE) {
  fromABI <- function() {
    if (!requireNamespace("stringr", quietly = TRUE)) {
      stop("Package 'stringr' is required for ABI .eds import", call. = FALSE)
    }
    if (!requireNamespace("data.table", quietly = TRUE)) {
      stop("Package 'data.table' is required for ABI .eds import", call. = FALSE)
    }

    uniqFolder <- tempfile("rdml-abi-")
    dir.create(uniqFolder, recursive = TRUE)
    on.exit(unlink(uniqFolder, recursive = TRUE), add = TRUE)

    requiredFiles <- c(
      "apldbio/sds/multicomponent_data.txt",
      "apldbio/sds/plate_setup.xml"
    )
    archiveFiles <- utils::unzip(fileName, list = TRUE)$Name
    normalized <- chartr("\\", "/", archiveFiles)
    selected <- archiveFiles[match(tolower(requiredFiles), tolower(normalized))]
    if (anyNA(selected)) {
      stop(
        "Not a supported ABI .eds archive: apldbio/sds data files are missing",
        call. = FALSE
      )
    }
    utils::unzip(fileName, files = selected, exdir = uniqFolder)

    dataFile <- file.path(
      uniqFolder, "apldbio", "sds", "multicomponent_data.txt"
    )
    plateFile <- file.path(
      uniqFolder, "apldbio", "sds", "plate_setup.xml"
    )

    if (!file.exists(dataFile) || !file.exists(plateFile)) {
      stop(
        "Not a supported ABI .eds archive: apldbio/sds data files are missing",
        call. = FALSE
      )
    }

    txt <- readChar(
      dataFile,
      nchars = file.info(dataFile)$size,
      useBytes = TRUE
    )

    m <- stringr::str_match_all(
      txt,
      "([0-9]+)\\t([0-9]+)\\t([A-Z]+)\\t(?:Infinity)?(?:NaN)?[0-9E\\-]*\\.?[0-9E\\-]*\\t([0-9E\\-]+\\.?[0-9E\\-]*)"
    )[[1L]]

    if (!nrow(m)) {
      stop("No fluorescence data found in ABI multicomponent_data.txt", call. = FALSE)
    }

    multicomponentData <- data.table::data.table(
      well = base::as.numeric(m[, 2L]),
      cyc = base::as.numeric(m[, 3L]),
      dye = m[, 4L],
      fluor = base::as.numeric(m[, 5L])
    )
    data.table::setkey(multicomponentData, well, dye)

    plateSetup <- xml2::read_xml(plateFile)
    rdmlEnv$ns <- xml2::xml_ns(plateSetup)

    snames <- .getTextVector(
      plateSetup,
      "/Plate/FeatureMap/Feature[Id='sample']/../FeatureValue/FeatureItem/Sample/Name"
    )
    sidx <- .getIntegerVector(
      plateSetup,
      "/Plate/FeatureMap/Feature[Id='sample']/../FeatureValue/Index"
    ) + 1L
    names(snames) <- as.character(sidx)

    detectorNodes <- xml2::xml_find_all(
      plateSetup,
      "/Plate/FeatureMap/Feature[Id='detector-task']/../FeatureValue"
    )

    rows <- list()
    rowN <- 0L

    for (elI in seq_along(detectorNodes)) {
      el <- detectorNodes[[elI]]
      index <- .getIntegerValue(el, "Index") + 1L

      taskRaw <- .getTextValue(el, "FeatureItem/DetectorTaskList/*[1]/Task")
      taskMap <- c(UNKNOWN = "unkn", NTC = "ntc", STANDARD = "std")
      task <- unname(taskMap[taskRaw])
      if (!length(task) || is.na(task)) task <- "unkn"

      sampleName <- unname(snames[as.character(index)])
      if (!length(sampleName) || is.na(sampleName) || !nzchar(sampleName)) {
        sampleName <- "unnamed"
      }

      taskLists <- xml2::xml_find_all(el, "FeatureItem/DetectorTaskList")
      for (tlI in seq_along(taskLists)) {
        tl <- taskLists[[tlI]]
        reporters <- .getTextVector(tl, "DetectorTask/Detector/Reporter")
        targets <- .getTextVector(tl, "DetectorTask/Detector/Name")
        quantities <- .getNumericVector(tl, "DetectorTask/Concentration")

        if (!length(reporters) || !length(targets)) next
        n <- max(length(reporters), length(targets))

        if (!length(quantities)) quantities <- rep(NA_real_, n)
        if (length(quantities) < n) quantities <- rep(quantities, length.out = n)
        if (length(reporters) < n) reporters <- rep(reporters, length.out = n)
        if (length(targets) < n) targets <- rep(targets, length.out = n)

        for (j in seq_len(n)) {
          rowN <- rowN + 1L
          rows[[rowN]] <- data.frame(
            fdataName = paste(index, reporters[[j]]),
            expId = "exp1",
            runId = "run1",
            reactId = index,
            sample = sampleName,
            sampleType = task,
            target = targets[[j]],
            targetDyeId = reporters[[j]],
            quantity = quantities[[j]],
            IsOmit = FALSE,
            stringsAsFactors = FALSE
          )
        }
      }
    }

    if (!length(rows)) {
      stop("No detector tasks found in ABI plate_setup.xml", call. = FALSE)
    }

    description <- data.table::as.data.table(do.call(rbind, rows))

    omittedI <- .getIntegerVector(
      plateSetup,
      "/Plate/Wells/Well[IsOmit='true']/Index"
    ) + 1L
    if (length(omittedI)) {
      description[reactId %in% omittedI, IsOmit := TRUE]
    }
    description <- description[IsOmit == FALSE]

    cycle0 <- sort(unique(multicomponentData$cyc))
    fdata <- list(cyc = cycle0 + 1)

    for (j in seq_len(nrow(description))) {
      r <- description[j]
      sub <- multicomponentData[list(
        base::as.integer(r$reactId) - 1L,
        as.character(r$targetDyeId)
      )]
      vals <- sub$fluor[match(cycle0, sub$cyc)]
      fdata[[as.character(r$fdataName)]] <- vals
    }
    fdata <- as.data.frame(fdata, check.names = FALSE, stringsAsFactors = FALSE)

    newRDMLImportData(
      series = list(
        newRDMLImportSeries(
          fdataType = "adp",
          fdata = fdata,
          description = description
        )
      ),
      publisher = "ABI",
      serialNumber = "1",
      format = "abi"
    )
  }

  fromABI()
}
