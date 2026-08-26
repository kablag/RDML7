# fromABI importer -----------------------------------------------------------

.rdml_import_abi <- function(filename, show.progress = TRUE) {
  fromABI <- function() {
    if (!requireNamespace("stringr", quietly = TRUE)) {
      stop("Package 'stringr' is required for ABI .eds import", call. = FALSE)
    }
    if (!requireNamespace("data.table", quietly = TRUE)) {
      stop("Package 'data.table' is required for ABI .eds import", call. = FALSE)
    }

    uniq.folder <- tempfile("rdml-abi-")
    dir.create(uniq.folder, recursive = TRUE)
    on.exit(unlink(uniq.folder, recursive = TRUE), add = TRUE)

    utils::unzip(filename, exdir = uniq.folder)

    data.file <- file.path(
      uniq.folder, "apldbio", "sds", "multicomponent_data.txt"
    )
    plate.file <- file.path(
      uniq.folder, "apldbio", "sds", "plate_setup.xml"
    )

    if (!file.exists(data.file) || !file.exists(plate.file)) {
      stop(
        "Not a supported ABI .eds archive: apldbio/sds data files are missing",
        call. = FALSE
      )
    }

    txt <- readChar(
      data.file,
      nchars = file.info(data.file)$size,
      useBytes = TRUE
    )

    m <- stringr::str_match_all(
      txt,
      "([0-9]+)\\t([0-9]+)\\t([A-Z]+)\\t(?:Infinity)?(?:NaN)?[0-9E\\-]*\\.?[0-9E\\-]*\\t([0-9E\\-]+\\.?[0-9E\\-]*)"
    )[[1L]]

    if (!nrow(m)) {
      stop("No fluorescence data found in ABI multicomponent_data.txt", call. = FALSE)
    }

    multicomponent.data <- data.table::data.table(
      well = base::as.numeric(m[, 2L]),
      cyc = base::as.numeric(m[, 3L]),
      dye = m[, 4L],
      fluor = base::as.numeric(m[, 5L])
    )

    plate.setup <- xml2::read_xml(plate.file)
    rdml.env$ns <- xml2::xml_ns(plate.setup)

    snames <- getTextVector(
      plate.setup,
      "/Plate/FeatureMap/Feature[Id='sample']/../FeatureValue/FeatureItem/Sample/Name"
    )
    sidx <- getIntegerVector(
      plate.setup,
      "/Plate/FeatureMap/Feature[Id='sample']/../FeatureValue/Index"
    ) + 1L
    names(snames) <- as.character(sidx)

    detector_nodes <- xml2::xml_find_all(
      plate.setup,
      "/Plate/FeatureMap/Feature[Id='detector-task']/../FeatureValue"
    )

    rows <- list()
    row_n <- 0L

    for (el_i in seq_along(detector_nodes)) {
      el <- detector_nodes[[el_i]]
      index <- getIntegerValue(el, "Index") + 1L

      task_raw <- getTextValue(el, "FeatureItem/DetectorTaskList/*[1]/Task")
      task_map <- c(UNKNOWN = "unkn", NTC = "ntc", STANDARD = "std")
      task <- unname(task_map[task_raw])
      if (!length(task) || is.na(task)) task <- "unkn"

      sample_name <- unname(snames[as.character(index)])
      if (!length(sample_name) || is.na(sample_name) || !nzchar(sample_name)) {
        sample_name <- "unnamed"
      }

      task_lists <- xml2::xml_find_all(el, "FeatureItem/DetectorTaskList")
      for (tl_i in seq_along(task_lists)) {
        tl <- task_lists[[tl_i]]
        reporters <- getTextVector(tl, "DetectorTask/Detector/Reporter")
        targets <- getTextVector(tl, "DetectorTask/Detector/Name")
        quantities <- getNumericVector(tl, "DetectorTask/Concentration")

        if (!length(reporters) || !length(targets)) next
        n <- max(length(reporters), length(targets))

        if (!length(quantities)) quantities <- rep(NA_real_, n)
        if (length(quantities) < n) quantities <- rep(quantities, length.out = n)
        if (length(reporters) < n) reporters <- rep(reporters, length.out = n)
        if (length(targets) < n) targets <- rep(targets, length.out = n)

        for (j in seq_len(n)) {
          row_n <- row_n + 1L
          rows[[row_n]] <- data.frame(
            fdata.name = paste(index, reporters[[j]]),
            exp.id = "exp1",
            run.id = "run1",
            react.id = index,
            sample = sample_name,
            sample.type = task,
            target = targets[[j]],
            target.dyeId = reporters[[j]],
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

    omitted.i <- getIntegerVector(
      plate.setup,
      "/Plate/Wells/Well[IsOmit='true']/Index"
    ) + 1L
    if (length(omitted.i)) {
      description[react.id %in% omitted.i, IsOmit := TRUE]
    }
    description <- description[IsOmit == FALSE]

    cycle0 <- sort(unique(multicomponent.data$cyc))
    fdata <- data.frame(cyc = cycle0 + 1, check.names = FALSE)

    for (j in seq_len(nrow(description))) {
      r <- description[j]
      sub <- multicomponent.data[
        well == base::as.integer(r$react.id) - 1L &
          dye == as.character(r$target.dyeId)
      ]
      vals <- sub$fluor[match(cycle0, sub$cyc)]
      fdata[[as.character(r$fdata.name)]] <- vals
    }

    x <- .rdml_new_import("ABI", "1")
    .rdml_set_fdata_import(x, fdata, description, "adp")
  }

  fromABI()
}
