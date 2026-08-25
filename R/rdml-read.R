rdml.env <- new.env(parent = emptyenv())

# XML parsing helpers -------------------------------------------------------

.is_xml_missing <- function(x) inherits(x, "xml_missing")

.xml_nodes_apply <- function(nodes, FUN) {
  if (length(nodes) == 0L) return(list())
  lapply(seq_along(nodes), function(i) FUN(nodes[[i]]))
}

.compact <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

# Lookup for the temporary, physically unnamed lists used while parsing.
# The public rdmlType object will expose such collections as rdmlKeyedList.
.list_get_by_key <- function(x, key, key_property = "id") {
  if (!is.list(x) || length(x) == 0L) return(NULL)
  pos <- match(key, .get_keys(x, key_property))
  if (is.na(pos)) return(NULL)
  x[[pos]]
}

.list_set_by_key <- function(x, key, value, key_property = "id") {
  if (!is.list(x)) stop("`x` must be a list", call. = FALSE)

  keys <- if (length(x)) .get_keys(x, key_property) else character()
  pos <- match(key, keys)

  if (is.null(value)) {
    if (!is.na(pos)) x[[pos]] <- NULL
    return(x)
  }

  value_key <- .get_key(value, key_property)
  if (is.na(value_key) || !identical(value_key, key)) {
    stop(
      "replacement key ('", key, "') does not match object ",
      key_property, " ('", value_key, "')",
      call. = FALSE
    )
  }

  if (is.na(pos)) {
    x[[length(x) + 1L]] <- value
  } else {
    x[[pos]] <- value
  }
  x
}

getTextValue <- function(tree, path, ns = rdml.env$ns) {
  node <- xml2::xml_find_first(tree, path, ns)
  if (.is_xml_missing(node)) return(NA_character_)
  xml2::xml_text(node)
}

getTextVector <- function(tree, path, ns = rdml.env$ns) {
  xml2::xml_text(xml2::xml_find_all(tree, path, ns))
}

getLogicalValue <- function(tree, path, ns = rdml.env$ns) {
  node <- xml2::xml_find_first(tree, path, ns)
  if (.is_xml_missing(node)) return(NA)
  switch(
    tolower(xml2::xml_text(node)),
    "true" = TRUE,
    "false" = FALSE,
    as.logical(xml2::xml_text(node))
  )
}

getNumericValue <- function(tree, path, ns = rdml.env$ns) {
  node <- xml2::xml_find_first(tree, path, ns)
  if (.is_xml_missing(node)) return(NA_real_)
  out <- .rdml_as_numeric(xml2::xml_text(node))
  if (!length(out)) NA_real_ else out[[1]]
}

getNumericVector <- function(tree, path, ns = rdml.env$ns) {
  .rdml_as_numeric(xml2::xml_text(xml2::xml_find_all(tree, path, ns)))
}

getIntegerValue <- function(tree, path, ns = rdml.env$ns) {
  node <- xml2::xml_find_first(tree, path, ns)
  if (.is_xml_missing(node)) return(NA_integer_)
  xml2::xml_integer(node)
}

getIntegerVector <- function(tree, path, ns = rdml.env$ns) {
  xml2::xml_integer(xml2::xml_find_all(tree, path, ns))
}

genId <- function(node) {
  id <- xml2::xml_attr(node, "id")
  if (length(id) != 1L || is.na(id)) stop("XML node has no id attribute")
  idType(id = id)
}

genIdRef <- function(node) {
  id <- xml2::xml_attr(node, "id")
  if (length(id) != 1L || is.na(id)) stop("XML node has no id attribute")
  idReferenceType(id = id)
}

# Keep historical support for comma decimal separators without masking
# base::as.numeric globally outside this file.
.rdml_as_numeric <- function(val) {
  if (!length(val)) return(NULL)
  suppressWarnings({
    out <- base::as.numeric(val)
  })
  bad <- is.na(out) & !is.na(val)
  if (any(bad)) {
    out[bad] <- suppressWarnings(base::as.numeric(gsub(",", ".", val[bad])))
  }
  out
}

# Misc functions -----------------------------------------------------------

FromPositionToId <- function(
    react.id,
    pcrFormat = pcrFormatType(
      rows = 8L,
      columns = 12L,
      rowLabel = labelFormatType("ABC"),
      columnLabel = labelFormatType("123")
    )) {
  row <- which(LETTERS == gsub("([A-Z])[0-9]+", "\\1", react.id))
  col <- base::as.integer(gsub("[A-Z]([0-9]+)", "\\1", react.id))
  (row - 1L) * pcrFormat$columns + col
}

GetIds <- function(l) {
  unname(vapply(l, .get_id, character(1)))
}

# Roche helpers ------------------------------------------------------------

GetDilutionsRoche <- function(uniq.folder) {
  path <- file.path(uniq.folder, "calculated_data.xml")
  if (!file.exists(path)) return(NA)

  rdml.doc <- xml2::read_xml(path)
  if (length(xml2::xml_ns(rdml.doc)) != 9L) return(NULL)

  rdml.env$ns <- xml2::xml_ns_rename(
    xml2::xml_ns(rdml.doc),
    d1 = "calc", d2 = "analys", d3 = "quant"
  )

  concs <- getNumericVector(rdml.doc, "//quant:absQuantDataSource/quant:standard")
  if (length(concs) == 0L) {
    concs <- getNumericVector(rdml.doc, "//quant:relQuantDataSource/quant:standard")
    concs.guids <- getTextVector(
      rdml.doc,
      "//quant:relQuantDataSource/standard/../quant:graphId"
    )
  } else {
    concs.guids <- getTextVector(
      rdml.doc,
      "//quant:absQuantDataSource/quant:standard/../quant:graphId"
    )
  }

  if (is.null(concs)) return(NULL)

  names(concs) <- concs.guids
  concs <- sort(concs, decreasing = TRUE)

  positions <- getTextVector(
    rdml.doc,
    "//quant:standardPoints/quant:standardPoint/quant:position"
  )
  positions <- vapply(positions, FromPositionToId, numeric(1))

  dye.names <- getTextVector(
    rdml.doc,
    "//quant:standardPoints/quant:standardPoint/quant:dyeName"
  )
  positions.guids <- getTextVector(
    rdml.doc,
    "//quant:standardPoints/quant:standardPoint/quant:graphIds/quant:guid"
  )

  positions.table <- matrix(
    c(dye.names, positions),
    ncol = length(positions),
    nrow = 2L,
    byrow = TRUE,
    dimnames = list(c("dye.name", "position"), positions.guids)
  )
  positions.table <- positions.table[
    , order(match(colnames(positions.table), names(concs))), drop = FALSE
  ]
  positions.table <- rbind(positions.table, conc = concs)

  dyes <- unique(positions.table["dye.name", ])
  dilutions <- lapply(dyes, function(dye) {
    idx <- which(positions.table["dye.name", ] == dye)
    out <- concs[idx]
    names(out) <- positions.table["position", idx]
    out
  })
  names(dilutions) <- dyes

  if (!length(dilutions)) NULL else dilutions
}

GetConditionsRoche <- function(uniq.folder) {
  path <- file.path(uniq.folder, "app_data.xml")
  if (!file.exists(path)) return(NA)

  rdml.doc <- xml2::read_xml(path)
  rdml.env$ns <- xml2::xml_ns_rename(xml2::xml_ns(rdml.doc), d1 = "lc96")

  nodes <- xml2::xml_find_all(
    rdml.doc,
    "/lc96:rocheLC96AppExtension/lc96:experiment/lc96:run/lc96:react/lc96:condition/..",
    ns = rdml.env$ns
  )
  reacts <- xml2::xml_attr(nodes, "id")
  conditions <- getTextVector(nodes, "lc96:condition")
  if (!length(conditions)) return(NULL)
  names(conditions) <- reacts
  conditions
}

GetRefGenesRoche <- function(uniq.folder) {
  path <- file.path(uniq.folder, "module_data.xml")
  if (!file.exists(path)) return(NA)

  rdml.doc <- xml2::read_xml(path)
  rdml.env$ns <- xml2::xml_ns_rename(xml2::xml_ns(rdml.doc), d3 = "rel")
  ref <- xml2::xml_find_all(
    rdml.doc,
    "//rel:geneSettings/rel:relQuantGeneSettings",
    ns = rdml.env$ns
  )
  if (!length(ref)) NULL else ref
}

#' Read qPCR data and return an S7 rdmlType object
#'
#' Supported import formats mirror the active import branches in the upstream
#' PCRuniversum/RDML initializer: RDML/LC96, ABI `.eds`, Rotor-Gene `.rex`,
#' Excel `.xlsx`/`.xls`, DTprime `.r96`, CSV, and FQD-96a text export.
#' With `format = "auto"`, the importer is selected from the extension.
#'
#' @param filename Input file path.
#' @param show.progress Show RDML parsing progress.
#' @param conditions.sep Optional Roche condition separator retained for
#'   compatibility with the original importer.
#' @param cluster Reserved for compatibility.
#' @param format One of `auto`, `rdml`, `xml`, `abi`, `rotorgene`, `excel`,
#'   `dtprime`, `csv`, or `fqd`; extension aliases are also accepted.
#' @export
rdml_read <- function(filename,
                      show.progress = TRUE,
                      conditions.sep = NULL,
                      cluster = NULL,
                      format = "auto") {
  if (missing(filename)) stop("filename is required")
  checkmate::assertString(filename)

  rdml_obj <- list(
    version = NA_character_,
    dateMade = NA_character_,
    dateUpdated = NA_character_,
    id = list(),
    experimenter = list(),
    documentation = list(),
    dye = list(),
    sample = list(),
    target = list(),
    thermalCyclingConditions = list(),
    experiment = list()
  )

  # Non-RDML import helpers -------------------------------------------------
  # These importers are ports of the active import branches in
  # PCRuniversum/RDML::RDML.init.R. They create the current S7 rdmlType via
  # SetFData(), rather than mutating the old R6 object.

  .new_import_rdml <- function(publisher = NULL, serial_number = "1") {
    ids <- if (is.null(publisher)) {
      list()
    } else {
      list(rdmlIdType(
        publisher = publisher,
        serialNumber = serial_number,
        MD5Hash = NA_character_
      ))
    }

    rdmlType(
      version = "1.2",
      dateMade = NA_character_,
      dateUpdated = NA_character_,
      id = ids,
      experimenter = list(),
      documentation = list(),
      dye = list(),
      sample = list(),
      target = list(),
      thermalCyclingConditions = list(),
      experiment = list()
    )
  }

  .set_fdata_import <- function(x, fdata, description, fdata.type = "adp") {
    if (!exists("SetFData", mode = "function", inherits = TRUE)) {
      stop(
        "SetFData() is required for non-RDML imports; source/load RDML.SetFData.R",
        call. = FALSE
      )
    }
    SetFData(x, fdata, description, fdata.type = fdata.type)
  }

  .split_ws <- function(x) {
    strsplit(trimws(x), "\\s+", perl = TRUE)[[1L]]
  }

  .first_match_field <- function(x, field, value, return_field, default = NA_character_) {
    for (el in x) {
      if (length(el) >= max(field, return_field) && identical(el[[field]], value)) {
        return(el[[return_field]])
      }
    }
    default
  }

  # Applied Biosystems .eds -------------------------------------------------
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

    x <- .new_import_rdml("ABI", "1")
    .set_fdata_import(x, fdata, description, "adp")
  }

  # Rotor-Gene .rex --------------------------------------------------------
  fromRotorGene <- function() {
    dat <- xml2::read_xml(filename)
    rdml.env$ns <- xml2::xml_ns(dat)

    sample_nodes <- xml2::xml_find_all(
      dat,
      "/Experiment/Samples/Page/Sample[Name]"
    )

    if (!length(sample_nodes)) {
      stop("No samples found in Rotor-Gene .rex file", call. = FALSE)
    }

    description <- do.call(
      rbind,
      lapply(seq_along(sample_nodes), function(i) {
        el <- sample_nodes[[i]]
        type_raw <- getTextValue(el, "Type")
        type_map <- c("5" = "pos", "3" = "ntc", "1" = "std")
        sample_type <- unname(type_map[as.character(type_raw)])
        if (!length(sample_type) || is.na(sample_type)) sample_type <- "unkn"

        quantity <- .rdml_as_numeric(getTextValue(el, "GivenConc"))
        if (!length(quantity)) quantity <- NA_real_ else quantity <- quantity[[1L]]

        data.frame(
          fdata.name = getTextValue(el, "ID"),
          exp.id = "exp1",
          run.id = "run1",
          react.id = base::as.numeric(getTextValue(el, "TubePosition")),
          sample = getTextValue(el, "Name"),
          sample.type = sample_type,
          quantity = quantity,
          target = NA_character_,
          target.dyeId = NA_character_,
          stringsAsFactors = FALSE
        )
      })
    )

    groups <- xml2::xml_find_all(dat, "/Experiment/Samples/Groups/Group")
    for (i in seq_along(groups)) {
      group <- groups[[i]]
      group_name <- getTextValue(group, "Name")
      tube_nodes <- xml2::xml_find_all(group, ".//Tube")
      ids <- xml2::xml_text(tube_nodes)
      description$target[description$fdata.name %in% ids] <- group_name
    }

    original.targets <- description$target
    original.targets[is.na(original.targets) | !nzchar(original.targets)] <- "unkn"

    channels <- xml2::xml_find_all(dat, "/Experiment/RawChannels/RawChannel")
    if (!length(channels)) {
      stop("No raw channels found in Rotor-Gene .rex file", call. = FALSE)
    }

    x <- .new_import_rdml("RotorGene", "1")

    for (i in seq_along(channels)) {
      rawChannel <- channels[[i]]
      dye_id <- getTextValue(rawChannel, "Name")
      description$target.dyeId <- dye_id
      description$target <- paste(original.targets, dye_id, sep = "#")

      readings <- getTextVector(
        rawChannel,
        sprintf("Name[text()='%s']/../Reading", dye_id)
      )
      if (!length(readings)) {
        readings <- xml2::xml_text(xml2::xml_find_all(rawChannel, ".//Reading"))
      }

      ridx <- base::as.integer(description$react.id)
      selected <- readings[ridx]
      curves <- lapply(selected, function(z) {
        if (is.na(z) || !nzchar(z)) return(numeric())
        .rdml_as_numeric(.split_ws(z))
      })

      lens <- lengths(curves)
      if (!length(lens) || any(lens == 0L)) next
      if (length(unique(lens)) != 1L) {
        stop("Rotor-Gene fluorescence curves have unequal lengths", call. = FALSE)
      }

      mat <- do.call(cbind, curves)
      fdata <- data.frame(cyc = seq_len(nrow(mat)), check.names = FALSE)
      for (j in seq_len(ncol(mat))) {
        fdata[[as.character(description$fdata.name[[j]])]] <- mat[, j]
      }

      x <- .set_fdata_import(x, fdata, description, "adp")
    }

    x
  }

  # Generic/Bio-Rad Excel ---------------------------------------------------
  fromExcel <- function() {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package 'readxl' is required for Excel import", call. = FALSE)
    }

    descr <- as.data.frame(
      readxl::read_excel(filename, sheet = "description"),
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
        fdata.name = as.character(descr[["Well"]]),
        exp.id = "Exp1",
        run.id = "Run1",
        react.id = seq_len(nrow(descr)),
        sample = as.character(descr[["Sample"]]),
        sample.type = tolower(as.character(descr[["Content"]])),
        target = as.character(descr[["Target"]]),
        target.dyeId = as.character(descr[["Fluor"]]),
        quantity = suppressWarnings(base::as.numeric(descr[["Starting Quantity (SQ)"]])),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      # Unkn-1 -> unkn, Std-2 -> std, etc.
      descr$sample.type <- sub("-.*$", "", descr$sample.type)
      # A01 -> A1.
      descr$fdata.name <- sub(
        "^([A-Za-z]+)0+([1-9].*)$",
        "\\1\\2",
        descr$fdata.name
      )
    }

    read_numeric_sheet <- function(sheet) {
      tryCatch({
        z <- as.data.frame(
          readxl::read_excel(filename, sheet = sheet),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        z[] <- lapply(z, function(v) suppressWarnings(base::as.numeric(v)))
        z
      }, error = function(e) NULL)
    }

    adp_data <- read_numeric_sheet("adp")
    mdp_data <- read_numeric_sheet("mdp")

    if (is.null(adp_data) && is.null(mdp_data)) {
      stop("Excel file contains neither 'adp' nor 'mdp' sheet", call. = FALSE)
    }

    x <- .new_import_rdml()
    if (!is.null(adp_data)) {
      x <- .set_fdata_import(x, adp_data, descr, "adp")
    }
    if (!is.null(mdp_data)) {
      x <- .set_fdata_import(x, mdp_data, descr, "mdp")
    }
    x
  }

  # DTprime / DNA-Technology .r96 -----------------------------------------
  fromDTprime <- function() {
    if (!requireNamespace("data.table", quietly = TRUE)) {
      stop(
        "Package 'data.table' is required for DTprime .r96 import",
        call. = FALSE
      )
    }

    DT96_OVERLOAD_SIGNAL <- 15000

    # Read as raw bytes first. readLines(..., encoding = "Windows-1251")
    # is not reliable on all Windows/R builds: the returned strings may still
    # contain CP1251 bytes but be treated as UTF-8, causing trimws()/sub()
    # to fail on e.g. "Апрель".
    raw <- readBin(
      filename,
      what = "raw",
      n = file.info(filename)$size
    )

    txt <- rawToChar(raw)

    if (validUTF8(txt)) {
      txt <- enc2utf8(txt)
    } else {
      txt <- iconv(
        txt,
        from = "CP1251",
        to = "UTF-8",
        sub = NA_character_
      )

      if (is.na(txt)) {
        stop(
          "Cannot decode DTprime .r96 file as UTF-8 or CP1251",
          call. = FALSE
        )
      }
    }

    lns <- strsplit(
      txt,
      "\\r\\n|\\n|\\r",
      perl = TRUE
    )[[1L]]

    # Keep the original lines for section detection, but parse data rows with
    # whitespace normalization. Do NOT rely on empty fields created by
    # str_split(" ") in the old importer.
    trimmed <- trimws(lns)

    marker <- function(pattern, mode = c("exact", "prefix", "regex")) {
      mode <- match.arg(mode)

      z <- switch(
        mode,
        exact  = which(trimmed == pattern),
        prefix = which(startsWith(trimmed, pattern)),
        regex  = which(grepl(pattern, trimmed, perl = TRUE))
      )

      if (!length(z)) NA_integer_ else z[[1L]]
    }

    # Some DTprime versions append service fields to section headers.
    # In the supplied file, for example:
    #   "$Information about tubes:$ 00"
    # so this marker must be matched by prefix rather than exact equality.
    i_tubes   <- marker("$Information about tubes:$", mode = "prefix")
    i_samples <- marker("$Information about Samples:$", mode = "prefix")
    i_multi   <- marker("$MultiChannel:$", mode = "prefix")
    i_device  <- marker("^\\$Device", mode = "regex")
    i_tests   <- marker("$Information about TESTs:$", mode = "prefix")
    i_mut     <- marker("$Parameters MutationMC:$", mode = "prefix")
    i_results <- marker("$Results of optical measurements:$", mode = "prefix")

    required_markers <- c(
      tubes = i_tubes,
      multi = i_multi,
      device = i_device,
      tests = i_tests,
      mutation = i_mut,
      results = i_results
    )

    if (anyNA(required_markers)) {
      stop(
        "Unsupported or malformed DTprime .r96 file; missing section(s): ",
        paste(names(required_markers)[is.na(required_markers)], collapse = ", "),
        call. = FALSE
      )
    }

    # ---- Tube table ----------------------------------------------------
    # Current .r96 tube rows contain 8 meaningful whitespace-separated
    # fields. Example:
    #   0  2 100 1 120 c0 1 CD53_1
    # They used to become 10 tokens only because the old code replaced tabs
    # and split on a single space, preserving empty tokens.
    tube_end <- if (!is.na(i_samples) && i_samples > i_tubes) {
      i_samples - 1L
    } else {
      i_multi - 1L
    }

    tube_lines <- lns[(i_tubes + 1L):tube_end]
    tube_lines <- tube_lines[
      grepl("^\\s*[0-9]+\\s+", tube_lines)
    ]

    tubes.info <- lapply(tube_lines, .split_ws)
    tubes.info <- Filter(
      function(z) {
        length(z) >= 8L &&
          grepl("^[0-9]+$", z[[1L]])
      },
      tubes.info
    )

    if (!length(tubes.info)) {
      stop("No tube records found in DTprime .r96 file", call. = FALSE)
    }

    # ---- Test/kit table ------------------------------------------------
    kit_lines <- lns[(i_tests + 1L):(i_mut - 1L)]
    kits <- lapply(kit_lines, .split_ws)
    kits <- Filter(
      function(z) {
        length(z) >= 2L &&
          grepl("^[0-9]+$", z[[1L]])
      },
      kits
    )

    # ---- Optional concentration table ---------------------------------
    # Some DTprime files have no rows between MultiChannel and Device.
    concentrations <- list()

    if (i_device > i_multi + 1L) {
      conc_lines <- lns[(i_multi + 1L):(i_device - 1L)]
      conc_lines <- conc_lines[
        nzchar(trimws(conc_lines)) &
          grepl("^\\s*[0-9]+(?:\\s|$)", conc_lines)
      ]

      concentrations <- lapply(conc_lines, .split_ws)
    }

    # ---- Optical measurements -----------------------------------------
    measurement_lines <- lns[(i_results + 1L):length(lns)]
    measurement_lines <- measurement_lines[nzchar(trimws(measurement_lines))]

    parts <- lapply(measurement_lines, .split_ws)

    if (!length(parts)) {
      stop("DTprime file contains no optical measurements", call. = FALSE)
    }

    part_lengths <- vapply(parts, length, integer(1))

    if (length(unique(part_lengths)) != 1L) {
      stop(
        "DTprime optical-data rows have inconsistent field counts: ",
        paste(sort(unique(part_lengths)), collapse = ", "),
        call. = FALSE
      )
    }

    fraw <- data.table::as.data.table(
      data.table::transpose(parts, fill = NA_character_)
    )

    # 7 metadata fields + 96 tube fields = 103 meaningful fields.
    # Older RDML code expected a 104th '?' column solely because splitting
    # lines ending in whitespace produced a trailing empty string.
    raw_names_103 <- c(
      "dye",
      "x1",
      "x2",
      "x3",
      "cycle",
      "exposition",
      "background",
      paste0("tube_", 0:95)
    )

    if (ncol(fraw) == length(raw_names_103)) {
      data.table::setnames(fraw, raw_names_103)
    } else if (ncol(fraw) == length(raw_names_103) + 1L) {
      data.table::setnames(fraw, c(raw_names_103, "?"))
    } else {
      stop(
        "Unexpected DTprime optical-data column count: ",
        ncol(fraw),
        " (expected ",
        length(raw_names_103),
        " meaningful columns",
        " or ",
        length(raw_names_103) + 1L,
        " including a trailing empty field)",
        call. = FALSE
      )
    }

    n_raw <- nrow(fraw)

    # Each PCR cycle has 5 dyes x 2 exposure rows.
    if (n_raw %% 10L != 0L) {
      stop(
        "Unexpected DTprime optical-data row count: ",
        n_raw,
        " (must be divisible by 10: 5 dyes x 2 exposures)",
        call. = FALSE
      )
    }

    n_cycles <- n_raw %/% 10L
    fdata <- data.table::data.table(cyc = seq_len(n_cycles))

    descr_rows <- list()
    nr <- 0L
    dyes <- c("FAM", "HEX", "ROX", "Cy5", "Cy5.5")

    # Helpers for normalized DTprime tables -----------------------------
    kit_name_for <- function(kit_id) {
      for (kit in kits) {
        if (length(kit) >= 2L && identical(kit[[1L]], kit_id)) {
          return(kit[[2L]])
        }
      }
      "unkn"
    }

    concentration_for <- function(tube_id) {
      for (conc in concentrations) {
        # Normalized form corresponds to old conc[2] / conc[4].
        # With empty tokens removed these are normally fields 1 / 3.
        if (length(conc) >= 3L && identical(conc[[1L]], tube_id)) {
          return(suppressWarnings(base::as.numeric(conc[[3L]])))
        }
      }
      NA_real_
    }

    for (tube in tubes.info) {
      # Normalized tube layout:
      # 1 = zero-based tube id
      # 7 = TEST/kit id
      # 8 = sample/tube name
      tube_id   <- tube[[1L]]
      kit_id    <- tube[[7L]]
      tube.name <- tube[[8L]]

      if (identical(tube.name, "-")) {
        next
      }

      tube_col <- paste0("tube_", tube_id)

      if (!(tube_col %in% names(fraw))) {
        next
      }

      kit_name <- kit_name_for(kit_id)
      quantity <- concentration_for(tube_id)

      for (dye_i in seq_along(dyes)) {
        dye <- dyes[[dye_i]]

        first_idx <- (dye_i - 1L) * 2L + 1L
        second_idx <- first_idx + 1L

        if (second_idx > n_raw) {
          next
        }

        marker_value <- fraw[[tube_col]][[first_idx]]

        # DTprime stores "1" for channels/tubes that were not measured.
        if (is.na(marker_value) || identical(marker_value, "1")) {
          next
        }

        idx2000 <- seq(first_idx, n_raw, by = 10L)
        idx400  <- seq(second_idx, n_raw, by = 10L)

        if (
          length(idx2000) != n_cycles ||
          length(idx400) != n_cycles
        ) {
          stop(
            "Inconsistent DTprime exposure series for tube ",
            tube_id,
            ", dye ",
            dye,
            call. = FALSE
          )
        }

        sig2000 <-
          suppressWarnings(base::as.numeric(fraw[[tube_col]][idx2000])) -
          suppressWarnings(base::as.numeric(fraw$background[idx2000]))

        sig400 <-
          suppressWarnings(base::as.numeric(fraw[[tube_col]][idx400])) -
          suppressWarnings(base::as.numeric(fraw$background[idx400]))

        if (
          all(is.na(sig2000)) ||
          all(is.na(sig400))
        ) {
          next
        }

        sigcomb <- if (
          !any(sig2000 >= DT96_OVERLOAD_SIGNAL, na.rm = TRUE)
        ) {
          sig2000
        } else {
          sig400 * 5
        }

        # fdata.name is an internal fluorescence-series key and must be
        # unique. Sample names may repeat for technical replicates, therefore
        # include the zero-based DTprime tube id.
        nm2000 <- sprintf("tube_%s_%s_2000", tube_id, dye)
        nm400  <- sprintf("tube_%s_%s_400",  tube_id, dye)
        nmcomb <- sprintf("tube_%s_%s_comb", tube_id, dye)

        fdata[[nm2000]] <- sig2000
        fdata[[nm400]]  <- sig400
        fdata[[nmcomb]] <- sigcomb

        base_row <- list(
          run.id = "run1",
          react.id = base::as.integer(tube_id) + 1L,
          sample = tube.name,
          target = sprintf("%s#%s", kit_name, dye),
          target.dyeId = dye,
          sample.type = "unkn",
          quantity = quantity
        )

        nr <- nr + 1L
        descr_rows[[nr]] <- data.frame(
          fdata.name = nm2000,
          exp.id = "exp_2000",
          run.id = base_row$run.id,
          react.id = base_row$react.id,
          sample = base_row$sample,
          target = base_row$target,
          target.dyeId = base_row$target.dyeId,
          sample.type = base_row$sample.type,
          quantity = base_row$quantity,
          stringsAsFactors = FALSE
        )

        nr <- nr + 1L
        descr_rows[[nr]] <- data.frame(
          fdata.name = nm400,
          exp.id = "exp_400",
          run.id = base_row$run.id,
          react.id = base_row$react.id,
          sample = base_row$sample,
          target = base_row$target,
          target.dyeId = base_row$target.dyeId,
          sample.type = base_row$sample.type,
          quantity = base_row$quantity,
          stringsAsFactors = FALSE
        )

        nr <- nr + 1L
        descr_rows[[nr]] <- data.frame(
          fdata.name = nmcomb,
          exp.id = "combined",
          run.id = base_row$run.id,
          react.id = base_row$react.id,
          sample = base_row$sample,
          target = base_row$target,
          target.dyeId = base_row$target.dyeId,
          sample.type = base_row$sample.type,
          quantity = base_row$quantity,
          stringsAsFactors = FALSE
        )
      }
    }

    if (!length(descr_rows) || ncol(fdata) < 2L) {
      stop(
        "No usable fluorescence data found in DTprime .r96 file",
        call. = FALSE
      )
    }

    description <- data.table::rbindlist(
      descr_rows,
      use.names = TRUE,
      fill = TRUE
    )

    data.table::set(
      description,
      j = "react.id",
      value = base::as.integer(
        description[["react.id"]]
      )
    )
    data.table::set(
      description,
      j = "quantity",
      value = suppressWarnings(
        base::as.numeric(
          description[["quantity"]]
        )
      )
    )

    if (show.progress) {
      cat(
        sprintf(
          "\nDTprime: %d cycles, %d fluorescence series, %d active reactions\n",
          n_cycles,
          ncol(fdata) - 1L,
          data.table::uniqueN(description$react.id)
        )
      )
    }

    x <- .new_import_rdml("DTprime", "1")

    .set_fdata_import(
      x,
      fdata,
      description,
      "adp"
    )
  }

  # Simple CSV --------------------------------------------------------------
  fromCSV <- function() {
    pcrdata <- utils::read.csv(
      filename,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    if (ncol(pcrdata) < 2L) {
      stop("CSV must contain a coordinate column and fluorescence columns", call. = FALSE)
    }

    fdata.names <- names(pcrdata)[-1L]
    first_name <- tolower(names(pcrdata)[1L])
    data.type <- if (first_name %in% c("tmp", "temperature")) "mdp" else "adp"

    descr <- data.frame(
      fdata.name = fdata.names,
      exp.id = "exp1",
      run.id = "run1",
      react.id = seq_along(fdata.names),
      sample = fdata.names,
      target = "unkn",
      target.dyeId = "unkn",
      sample.type = "unkn",
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    x <- .new_import_rdml()
    .set_fdata_import(x, pcrdata, descr, data.type)
  }

  # FQD-96a text export -----------------------------------------------------
  fromFQDexport <- function() {
    if (!requireNamespace("data.table", quietly = TRUE)) {
      stop("Package 'data.table' is required for FQD-96a import", call. = FALSE)
    }

    content <- readChar(
      filename,
      nchars = file.info(filename)$size,
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

    expected_desc_names <- c(
      "position", "sample.id", "sample", "sample.type",
      "target", "target.dyeId", "cq", "cq.mean", "cq.sd",
      "quantity", "quantity.avg", "quantity.sd", "V13", "fdata.name"
    )
    if (ncol(description) != length(expected_desc_names)) {
      stop(
        "Unexpected FQD description column count: ", ncol(description),
        " (expected ", length(expected_desc_names), ")",
        call. = FALSE
      )
    }
    data.table::setnames(description, expected_desc_names)
    description[, fdata.name := paste(position, target, sep = "_")]

    num_cols <- c(
      "cq", "cq.mean", "cq.sd", "quantity", "quantity.avg", "quantity.sd"
    )
    description[, (num_cols) := lapply(.SD, .rdml_as_numeric), .SDcols = num_cols]
    description[, `:=`(
      exp.id = "exp1",
      run.id = "raw_data",
      sample = ifelse(sample == "", "unkn_s", sample),
      react.id = vapply(position, FromPositionToId, numeric(1))
    )]

    type_map <- c(
      Unknown = "unkn",
      Standard = "std",
      Negative = "ntc",
      Positive = "pos"
    )
    mapped <- unname(type_map[description$sample.type])
    description$sample.type[!is.na(mapped)] <- mapped[!is.na(mapped)]

    make_fdata <- function(txt) {
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
      m_num <- matrix(
        suppressWarnings(base::as.numeric(m)),
        nrow = nrow(m),
        ncol = ncol(m),
        dimnames = dimnames(m)
      )
      if (ncol(m_num) != nrow(description)) {
        stop(
          "FQD fluorescence/description column mismatch",
          call. = FALSE
        )
      }

      out <- data.frame(cyc = seq_len(nrow(m_num)), check.names = FALSE)
      for (j in seq_len(ncol(m_num))) {
        out[[description$fdata.name[[j]]]] <- m_num[, j]
      }
      out
    }

    rawfdata <- make_fdata(inpstr[[2L]])
    processedfdata <- make_fdata(inpstr[[3L]])

    x <- .new_import_rdml()
    x <- .set_fdata_import(x, rawfdata, description, "adp")

    description_processed <- data.table::copy(description)
    description_processed[, run.id := "processed_data"]
    x <- .set_fdata_import(x, processedfdata, description_processed, "adp")
    x
  }

  as_table_rdml <- function(rdml_obj) {
    rows <- list()
    n <- 0L

    for (exp in rdml_obj$experiment) {
      runs <- S7::prop(exp, "run")
      if (.is_single_na(runs) || !length(runs)) next

      for (run in runs) {
        reacts <- S7::prop(run, "react")
        if (.is_single_na(reacts) || !length(reacts)) next

        for (react in reacts) {
          n <- n + 1L
          rows[[n]] <- data.frame(
            react.id = suppressWarnings(base::as.integer(react@id@id)),
            sample = react@sample@id,
            stringsAsFactors = FALSE
          )
        }
      }
    }

    if (!length(rows)) {
      return(data.table::data.table(
        react.id = integer(),
        sample = character()
      ))
    }

    data.table::as.data.table(do.call(rbind, rows))
  }

  # Parse optional object only when the XML node exists.
  optional_node <- function(tree, path, FUN, ns = rdml.env$ns) {
    node <- xml2::xml_find_first(tree, path, ns)
    if (.is_xml_missing(node)) return(NA)
    FUN(node)
  }

  parse_id_refs <- function(tree, path) {
    .xml_nodes_apply(
      xml2::xml_find_all(tree, path, rdml.env$ns),
      genIdRef
    )
  }

  parse_xrefs <- function(tree, path = "rdml:xRef") {
    .xml_nodes_apply(
      xml2::xml_find_all(tree, path, rdml.env$ns),
      function(node) {
        xRefType(
          name = getTextValue(node, "rdml:name"),
          id = getTextValue(node, "rdml:id")
        )
      }
    )
  }

  parse_annotations <- function(sample) {
    .xml_nodes_apply(
      xml2::xml_find_all(sample, "rdml:annotation", rdml.env$ns),
      function(node) {
        annotationType(
          property = getTextValue(node, "rdml:property"),
          value = getTextValue(node, "rdml:value")
        )
      }
    )
  }

  parse_sample_types <- function(sample) {
    nodes <- xml2::xml_find_all(sample, "rdml:type", rdml.env$ns)
    .compact(.xml_nodes_apply(nodes, function(node) {
      target_id <- xml2::xml_attr(node, "targetId")
      if (is.na(target_id)) target_id <- xml2::xml_attr(node, "target")

      # Current sampleTargetType requires targetId. Older RDML files may
      # contain a target-independent <type>; that value cannot be represented
      # losslessly by the current types7.R schema, so leave it out.
      if (is.na(target_id) || !nzchar(target_id)) return(NULL)

      sampleTargetType(
        targetId = idReferenceType(target_id),
        sampleType = sampleTypeType(xml2::xml_text(node))
      )
    }))
  }

  parse_quantities <- function(sample) {
    nodes <- xml2::xml_find_all(sample, "rdml:quantity", rdml.env$ns)
    .compact(.xml_nodes_apply(nodes, function(node) {
      target_id <- xml2::xml_attr(node, "targetId")
      if (is.na(target_id)) target_id <- xml2::xml_attr(node, "target")
      if (is.na(target_id) || !nzchar(target_id)) return(NULL)

      quantityType(
        targetId = idReferenceType(target_id),
        value = getNumericValue(node, "rdml:value"),
        unit = quantityUnitType(getTextValue(node, "rdml:unit"))
      )
    }))
  }

  parse_oligo <- function(target, path) {
    node <- xml2::xml_find_first(target, path, rdml.env$ns)
    if (.is_xml_missing(node)) return(NA)

    seq <- getTextValue(node, "rdml:sequence")
    if (is.na(seq)) return(NA)

    oligoType(
      threePrimeTag = getTextValue(node, "rdml:threePrimeTag"),
      fivePrimeTag = getTextValue(node, "rdml:fivePrimeTag"),
      sequence = seq
    )
  }

  parse_sequences <- function(target) {
    node <- xml2::xml_find_first(target, "rdml:sequences", rdml.env$ns)
    if (.is_xml_missing(node)) return(NA)

    sequencesType(
      forwardPrimer = parse_oligo(target, "rdml:sequences/rdml:forwardPrimer"),
      reversePrimer = parse_oligo(target, "rdml:sequences/rdml:reversePrimer"),
      probe1 = parse_oligo(target, "rdml:sequences/rdml:probe1"),
      probe2 = parse_oligo(target, "rdml:sequences/rdml:probe2"),
      amplicon = parse_oligo(target, "rdml:sequences/rdml:amplicon")
    )
  }

  parse_measure <- function(tree, path) {
    txt <- getTextValue(tree, path)
    if (is.na(txt)) return(NA)
    measureType(txt)
  }

  parse_temperature <- function(step) {
    node <- xml2::xml_find_first(step, "rdml:temperature", rdml.env$ns)
    if (.is_xml_missing(node)) return(NA)

    temperatureType(
      temperature = getNumericValue(node, "rdml:temperature"),
      duration = getIntegerValue(node, "rdml:duration"),
      temperatureChange = getNumericValue(node, "rdml:temperatureChange"),
      durationChange = getIntegerValue(node, "rdml:durationChange"),
      measure = parse_measure(node, "rdml:measure"),
      ramp = getNumericValue(node, "rdml:ramp")
    )
  }

  parse_gradient <- function(step) {
    node <- xml2::xml_find_first(step, "rdml:gradient", rdml.env$ns)
    if (.is_xml_missing(node)) return(NA)

    gradientType(
      highTemperature = getNumericValue(node, "rdml:highTemperature"),
      lowTemperature = getNumericValue(node, "rdml:lowTemperature"),
      duration = getIntegerValue(node, "rdml:duration"),
      temperatureChange = getNumericValue(node, "rdml:temperatureChange"),
      durationChange = getIntegerValue(node, "rdml:durationChange"),
      measure = parse_measure(node, "rdml:measure"),
      ramp = getNumericValue(node, "rdml:ramp")
    )
  }

  parse_loop <- function(step) {
    node <- xml2::xml_find_first(step, "rdml:loop", rdml.env$ns)
    if (.is_xml_missing(node)) return(NA)

    do.call(
      loopType,
      list(
        goto = getIntegerValue(node, "rdml:goto"),
        "repeat" = getIntegerValue(node, "rdml:repeat")
      )
    )
  }

  parse_pause <- function(step) {
    node <- xml2::xml_find_first(step, "rdml:pause", rdml.env$ns)
    if (.is_xml_missing(node)) return(NA)
    pauseType(temperature = getNumericValue(node, "rdml:temperature"))
  }

  parse_lid_open <- function(step) {
    node <- xml2::xml_find_first(step, "rdml:lidOpen", rdml.env$ns)
    if (.is_xml_missing(node)) return(NA)
    lidOpenType()
  }

  parse_adp <- function(data) {
    nodes <- xml2::xml_find_all(data, "rdml:adp", rdml.env$ns)
    if (!length(nodes)) return(NA)

    cyc <- vapply(seq_along(nodes), function(i) {
      getNumericValue(nodes[[i]], "rdml:cyc")
    }, numeric(1))
    fluor <- vapply(seq_along(nodes), function(i) {
      getNumericValue(nodes[[i]], "rdml:fluor")
    }, numeric(1))
    tmp <- vapply(seq_along(nodes), function(i) {
      getNumericValue(nodes[[i]], "rdml:tmp")
    }, numeric(1))

    if (all(is.na(tmp))) {
      dpAmpCurveType(
        data.table::data.table(cyc = cyc, fluor = fluor)
      )
    } else {
      dpAmpCurveType(
        data.table::data.table(cyc = cyc, tmp = tmp, fluor = fluor)
      )
    }
  }

  parse_mdp <- function(data) {
    nodes <- xml2::xml_find_all(data, "rdml:mdp", rdml.env$ns)
    if (!length(nodes)) return(NA)

    tmp <- vapply(seq_along(nodes), function(i) {
      getNumericValue(nodes[[i]], "rdml:tmp")
    }, numeric(1))
    fluor <- vapply(seq_along(nodes), function(i) {
      getNumericValue(nodes[[i]], "rdml:fluor")
    }, numeric(1))

    dpMeltingCurveType(
      data.table::data.table(tmp = tmp, fluor = fluor)
    )
  }

  fromRDML <- function() {
    rdml.env$ns <- NULL

    dilutions.r <- NULL
    conditions.r <- NULL
    ref.genes.r <- NULL
    is_roche_archive <- FALSE

    if (identical(format, "xml")) {
      rdml.doc <- xml2::read_xml(filename)
      rdml.env$ns <- xml2::xml_ns(rdml.doc)
      if (!("rdml" %in% names(rdml.env$ns))) {
        rdml.env$ns <- xml2::xml_ns_rename(rdml.env$ns, d1 = "rdml")
      }
    } else {
      uniq.folder <- tempfile("rdml-")
      dir.create(uniq.folder, recursive = TRUE)
      on.exit(unlink(uniq.folder, recursive = TRUE), add = TRUE)

      unzipped.rdml <- utils::unzip(filename, exdir = uniq.folder)
      if (!length(unzipped.rdml)) stop("RDML archive is empty")

      is_roche_archive <- length(unzipped.rdml) > 1L &&
        file.exists(file.path(uniq.folder, "rdml_data.xml"))

      if (is_roche_archive) {
        rdml.doc <- xml2::read_xml(file.path(uniq.folder, "rdml_data.xml"))
        dilutions.r <- GetDilutionsRoche(uniq.folder)
        conditions.r <- GetConditionsRoche(uniq.folder)
        ref.genes.r <- GetRefGenesRoche(uniq.folder)
        rdml.env$ns <- xml2::xml_ns_rename(xml2::xml_ns(rdml.doc), d1 = "rdml")
      } else {
        rdml.doc <- xml2::read_xml(unzipped.rdml[[1]])
        rdml.env$ns <- xml2::xml_ns(rdml.doc)
        if (!("rdml" %in% names(rdml.env$ns))) {
          rdml.env$ns <- xml2::xml_ns_rename(rdml.env$ns, d1 = "rdml")
        }
      }
    }

    root <- xml2::xml_root(rdml.doc)
    rdml_obj$version <- xml2::xml_attr(root, "version")
    if (is.na(rdml_obj$version)) rdml_obj$version <- ""

    rdml_obj$dateMade <- getTextValue(rdml.doc, "/rdml:rdml/rdml:dateMade")
    rdml_obj$dateUpdated <- getTextValue(rdml.doc, "/rdml:rdml/rdml:dateUpdated")

    # id -------------------------------------------------------------------
    rdml_obj$id <- .xml_nodes_apply(
      xml2::xml_find_all(rdml.doc, "/rdml:rdml/rdml:id", rdml.env$ns),
      function(node) {
        rdmlIdType(
          publisher = getTextValue(node, "rdml:publisher"),
          serialNumber = getTextValue(node, "rdml:serialNumber"),
          MD5Hash = getTextValue(node, "rdml:MD5Hash")
        )
      }
    )

    publisher <- if (length(rdml_obj$id)) rdml_obj$id[[1]]$publisher else NA_character_
    is_roche <- identical(publisher, "Roche Diagnostics")

    # experimenter ---------------------------------------------------------
    rdml_obj$experimenter <- .xml_nodes_apply(
      xml2::xml_find_all(rdml.doc, "/rdml:rdml/rdml:experimenter", rdml.env$ns),
      function(node) {
        experimenterType(
          id = genId(node),
          firstName = getTextValue(node, "rdml:firstName"),
          lastName = getTextValue(node, "rdml:lastName"),
          email = getTextValue(node, "rdml:email"),
          labName = getTextValue(node, "rdml:labName"),
          labAddress = getTextValue(node, "rdml:labAddress")
        )
      }
    )

    # documentation --------------------------------------------------------
    rdml_obj$documentation <- .xml_nodes_apply(
      xml2::xml_find_all(rdml.doc, "/rdml:rdml/rdml:documentation", rdml.env$ns),
      function(node) {
        documentationType(
          id = genId(node),
          text = getTextValue(node, "rdml:text")
        )
      }
    )

    # dye ------------------------------------------------------------------
    rdml_obj$dye <- .xml_nodes_apply(
      xml2::xml_find_all(rdml.doc, "/rdml:rdml/rdml:dye", rdml.env$ns),
      function(node) {
        chemistry <- getTextValue(node, "rdml:dyeChemistry")
        dyeType(
          id = genId(node),
          description = getTextValue(node, "rdml:description"),
          dyeChemistry = if (is.na(chemistry)) NA else dyeChemistryType(chemistry)
        )
      }
    )

    # sample ---------------------------------------------------------------
    rdml_obj$sample <- .compact(.xml_nodes_apply(
      xml2::xml_find_all(rdml.doc, "/rdml:rdml/rdml:sample", rdml.env$ns),
      function(sample) {
        id <- xml2::xml_attr(sample, "id")
        raw_type <- getTextValue(sample, "rdml:type")

        # Roche uses ntp as an omitted/empty sample marker.
        if (identical(raw_type, "ntp")) return(NULL)

        cdna_node <- xml2::xml_find_first(sample, "rdml:cdnaSynthesisMethod", rdml.env$ns)
        cdna <- if (.is_xml_missing(cdna_node)) {
          NA
        } else {
          thermal_node <- xml2::xml_find_first(
            cdna_node,
            "rdml:thermalCyclingConditions",
            rdml.env$ns
          )
          priming_method <- getTextValue(
            cdna_node,
            "rdml:primingMethod"
          )

          cdnaSynthesisMethodType(
            enzyme = getTextValue(cdna_node, "rdml:enzyme"),
            primingMethod = if (is.na(priming_method)) {
              NA
            } else {
              primingMethodType(priming_method)
            },
            dnaseTreatment = getLogicalValue(cdna_node, "rdml:dnaseTreatment"),
            thermalCyclingConditions = if (.is_xml_missing(thermal_node)) {
              NA
            } else {
              genIdRef(thermal_node)
            }
          )
        }

        template_node <- xml2::xml_find_first(sample, "rdml:templateQuantity", rdml.env$ns)
        template_quantity <- if (.is_xml_missing(template_node)) {
          NA
        } else {
          templateQuantityType(
            conc = getNumericValue(template_node, "rdml:conc"),
            nucleotide = nucleotideType(getTextValue(template_node, "rdml:nucleotide"))
          )
        }

        sampleType(
          id = idType(id),
          description = getTextValue(sample, "rdml:description"),
          documentation = parse_id_refs(sample, "rdml:documentation"),
          xRef = parse_xrefs(sample),
          annotation = parse_annotations(sample),
          type = parse_sample_types(sample),
          interRunCalibrator = getLogicalValue(sample, "rdml:interRunCalibrator"),
          quantity = parse_quantities(sample),
          calibratorSample = getLogicalValue(sample, "rdml:calibratorSample"),
          cdnaSynthesisMethod = cdna,
          templateQuantity = template_quantity
        )
      }
    ))

    # target ---------------------------------------------------------------
    rdml_obj$target <- .xml_nodes_apply(
      xml2::xml_find_all(rdml.doc, "/rdml:rdml/rdml:target", rdml.env$ns),
      function(target) {
        target_id <- xml2::xml_attr(target, "id")
        if (is_roche && grepl("@", target_id, fixed = TRUE)) {
          target_id <- sub("^.*@", "", target_id)
        }

        dye_node <- xml2::xml_find_first(target, "rdml:dyeId", rdml.env$ns)
        dye_id <- if (.is_xml_missing(dye_node)) {
          stop("target '", target_id, "' has no dyeId")
        } else {
          id_attr <- xml2::xml_attr(dye_node, "id")
          if (!is.na(id_attr)) idReferenceType(id_attr) else idReferenceType(xml2::xml_text(dye_node))
        }

        commercial_node <- xml2::xml_find_first(target, "rdml:commercialAssay", rdml.env$ns)
        commercial <- if (.is_xml_missing(commercial_node)) {
          NA
        } else {
          commercialAssayType(
            company = getTextValue(commercial_node, "rdml:company"),
            orderNumber = getTextValue(commercial_node, "rdml:orderNumber")
          )
        }

        targetType(
          id = idType(target_id),
          description = getTextValue(target, "rdml:description"),
          documentation = parse_id_refs(target, "rdml:documentation"),
          xRef = parse_xrefs(target),
          type = targetTypeType(getTextValue(target, "rdml:type")),
          amplificationEfficiencyMethod = getTextValue(target, "rdml:amplificationEfficiencyMethod"),
          amplificationEfficiency = getNumericValue(target, "rdml:amplificationEfficiency"),
          amplificationEfficiencySE = getNumericValue(target, "rdml:amplificationEfficiencySE"),
          meltingTemperature = getNumericValue(target, "rdml:meltingTemperature"),
          detectionLimit = getNumericValue(target, "rdml:detectionLimit"),
          dyeId = dye_id,
          sequences = parse_sequences(target),
          commercialAssay = commercial
        )
      }
    )

    # thermalCyclingConditions ---------------------------------------------
    rdml_obj$thermalCyclingConditions <- .xml_nodes_apply(
      xml2::xml_find_all(
        rdml.doc,
        "/rdml:rdml/rdml:thermalCyclingConditions",
        rdml.env$ns
      ),
      function(tcc) {
        steps <- .xml_nodes_apply(
          xml2::xml_find_all(tcc, "rdml:step", rdml.env$ns),
          function(step) {
            stepType(
              nr = getIntegerValue(step, "rdml:nr"),
              description = getTextValue(step, "rdml:description"),
              temperature = parse_temperature(step),
              gradient = parse_gradient(step),
              loop = parse_loop(step),
              pause = parse_pause(step),
              lidOpen = parse_lid_open(step)
            )
          }
        )

        thermalCyclingConditionsType(
          id = genId(tcc),
          description = getTextValue(tcc, "rdml:description"),
          documentation = parse_id_refs(tcc, "rdml:documentation"),
          lidTemperature = getNumericValue(tcc, "rdml:lidTemperature"),
          experimenter = parse_id_refs(tcc, "rdml:experimenter"),
          step = steps
        )
      }
    )

    # data -----------------------------------------------------------------
    GetData <- function(data) {
      tar_node <- xml2::xml_find_first(data, "rdml:tar", rdml.env$ns)
      tar.id <- if (.is_xml_missing(tar_node)) NA_character_ else xml2::xml_attr(tar_node, "id")

      if (is_roche && !is.na(tar.id) && grepl("@", tar.id, fixed = TRUE)) {
        tar.id <- sub("^.*@", "", tar.id)
      }

      if (is.na(tar.id) || !nzchar(tar.id)) {
        stop("RDML data element has no target id")
      }

      dataType(
        targetId = idReferenceType(tar.id),
        cq = getNumericValue(data, "rdml:cq"),
        N0 = getNumericValue(data, "rdml:N0"),
        ampEffMet = getTextValue(data, "rdml:ampEffMet"),
        ampEff = getNumericValue(data, "rdml:ampEff"),
        ampEffSE = getNumericValue(data, "rdml:ampEffSE"),
        corrF = getNumericValue(data, "rdml:corrF"),
        corrP = getNumericValue(data, "rdml:corrP"),
        meltTemp = getNumericValue(data, "rdml:meltTemp"),
        excl = getTextValue(data, "rdml:excl"),
        note = getTextValue(data, "rdml:note"),
        adp = parse_adp(data),
        mdp = parse_mdp(data),
        endPt = getNumericValue(data, "rdml:endPt"),
        bgFluor = getNumericValue(data, "rdml:bgFluor"),
        bgFluorSlp = getNumericValue(data, "rdml:bgFluorSlp"),
        quantFluor = getNumericValue(data, "rdml:quantFluor")
      )
    }

    GetPartitionData <- function(data) {
      tar_node <- xml2::xml_find_first(data, "rdml:tar", rdml.env$ns)
      tar.id <- if (.is_xml_missing(tar_node)) NA_character_ else xml2::xml_attr(tar_node, "id")
      if (is.na(tar.id) || !nzchar(tar.id)) stop("partition data has no target id")

      partitionDataType(
        targetId = idReferenceType(tar.id),
        excluded = getTextValue(data, "rdml:excluded"),
        note = getTextValue(data, "rdml:note"),
        pos = getIntegerValue(data, "rdml:pos"),
        neg = getIntegerValue(data, "rdml:neg"),
        undef = getIntegerValue(data, "rdml:undef"),
        excl = getIntegerValue(data, "rdml:excl"),
        conc = getNumericValue(data, "rdml:conc")
      )
    }

    GetPartitions <- function(react) {
      node <- xml2::xml_find_first(react, "rdml:partitions", rdml.env$ns)
      if (.is_xml_missing(node)) return(NA)

      partitionsType(
        volume = getNumericValue(node, "rdml:volume"),
        endPtTable = getTextValue(node, "rdml:endPtTable"),
        data = .xml_nodes_apply(
          xml2::xml_find_all(node, "rdml:data", rdml.env$ns),
          GetPartitionData
        )
      )
    }

    # react ----------------------------------------------------------------
    GetReact <- function(
        react,
        pcrFormat = pcrFormatType(
          rows = 8L,
          columns = 12L,
          rowLabel = labelFormatType("ABC"),
          columnLabel = labelFormatType("123")
        )) {
      react.id <- xml2::xml_attr(react, "id")
      react.id.corrected <- suppressWarnings(base::as.integer(react.id))
      if (is.na(react.id.corrected)) {
        react.id.corrected <- FromPositionToId(react.id, pcrFormat)
      }

      sample_node <- xml2::xml_find_first(react, "rdml:sample", rdml.env$ns)
      if (.is_xml_missing(sample_node)) stop("react '", react.id, "' has no sample")
      sample_id <- xml2::xml_attr(sample_node, "id")

      if (is_roche) {
        sample_obj <- .list_get_by_key(rdml_obj$sample, sample_id, "id")
        if (is.null(sample_obj)) return(NULL)
        sample_id <- sample_obj$description
      }

      reactType(
        id = idType(as.character(react.id.corrected)),
        sample = idReferenceType(sample_id),
        data = .xml_nodes_apply(
          xml2::xml_find_all(react, "rdml:data", rdml.env$ns),
          GetData
        ),
        partitions = {
          partitions <- GetPartitions(react)
          if (.is_single_na(partitions)) list() else list(partitions)
        }
      )
    }

    # run ------------------------------------------------------------------
    GetRun <- function(run) {
      run.id <- xml2::xml_attr(run, "id")

      pcrFormat <- {
        pcrFormatStr <- getTextValue(run, "rdml:pcrFormat")
        if (!is.na(pcrFormatStr) && grepl("well", pcrFormatStr, ignore.case = TRUE)) {
          if (grepl("96-well", pcrFormatStr, ignore.case = TRUE)) {
            pcrFormatType(
              rows = 8L,
              columns = 12L,
              rowLabel = labelFormatType("ABC"),
              columnLabel = labelFormatType("123")
            )
          } else {
            pcrFormatType(
              rows = 16L,
              columns = 24L,
              rowLabel = labelFormatType("ABC"),
              columnLabel = labelFormatType("123")
            )
          }
        } else {
          rows <- getIntegerValue(run, "rdml:pcrFormat/rdml:rows")
          cols <- getIntegerValue(run, "rdml:pcrFormat/rdml:columns")
          if (!is.na(rows) && !is.na(cols)) {
            pcrFormatType(
              rows = rows,
              columns = cols,
              rowLabel = labelFormatType(getTextValue(run, "rdml:pcrFormat/rdml:rowLabel")),
              columnLabel = labelFormatType(getTextValue(run, "rdml:pcrFormat/rdml:columnLabel"))
            )
          } else {
            pcrFormatType(
              rows = 8L,
              columns = 12L,
              rowLabel = labelFormatType("ABC"),
              columnLabel = labelFormatType("123")
            )
          }
        }
      }

      if (show.progress) cat(sprintf("\n\trun: %s\n", run.id))

      software_node <- xml2::xml_find_first(run, "rdml:dataCollectionSoftware", rdml.env$ns)
      software <- if (.is_xml_missing(software_node)) {
        NA
      } else {
        tryCatch(
          dataCollectionSoftwareType(
            name = getTextValue(software_node, "rdml:name"),
            version = getTextValue(software_node, "rdml:version")
          ),
          error = function(e) NA
        )
      }

      cq_text <- getTextValue(run, "rdml:cqDetectionMethod")
      cq_method <- if (is.na(cq_text)) {
        NA
      } else {
        tryCatch(cqDetectionMethodType(cq_text), error = function(e) NA)
      }

      tcc_node <- xml2::xml_find_first(run, "rdml:thermalCyclingConditions", rdml.env$ns)
      tcc_ref <- if (.is_xml_missing(tcc_node)) NA else genIdRef(tcc_node)

      runType(
        id = idType(run.id),
        description = getTextValue(run, "rdml:description"),
        documentation = parse_id_refs(run, "rdml:documentation"),
        experimenter = parse_id_refs(run, "rdml:experimenter"),
        instrument = getTextValue(run, "rdml:instrument"),
        dataCollectionSoftware = software,
        backgroundDeterminationMethod = getTextValue(run, "rdml:backgroundDeterminationMethod"),
        cqDetectionMethod = cq_method,
        thermalCyclingConditions = tcc_ref,
        pcrFormat = pcrFormat,
        runDate = getTextValue(run, "rdml:runDate"),
        react = .compact(.xml_nodes_apply(
          xml2::xml_find_all(run, "rdml:react", rdml.env$ns),
          function(node) GetReact(node, pcrFormat)
        ))
      )
    }

    # experiment -----------------------------------------------------------
    GetExperiment <- function(experiment) {
      experiment.id <- xml2::xml_attr(experiment, "id")
      if (show.progress) cat(sprintf("\nLoading experiment: %s", experiment.id))

      experimentType(
        id = idType(experiment.id),
        description = getTextValue(experiment, "rdml:description"),
        documentation = parse_id_refs(experiment, "rdml:documentation"),
        run = .xml_nodes_apply(
          xml2::xml_find_all(experiment, "rdml:run", rdml.env$ns),
          GetRun
        )
      )
    }

    rdml_obj$experiment <- .xml_nodes_apply(
      xml2::xml_find_all(rdml.doc, "/rdml:rdml/rdml:experiment", rdml.env$ns),
      GetExperiment
    )

    # Combine CFX96 runs to one. Work on ordinary list properties so that
    # no physical list names are required at any stage.
    if (
      identical(publisher, "Bio-Rad Laboratories, Inc.") &&
      length(rdml_obj$experiment) >= 1L
    ) {
      exp1 <- rdml_obj$experiment[[1]]
      runs <- S7::prop(exp1, "run")

      if (is.list(runs) && length(runs) > 1L) {
        if (show.progress) cat("\nCombining Bio-Rad runs\n")
        first.run <- runs[[1]]
        first.reacts <- S7::prop(first.run, "react")

        for (run.i in 2:length(runs)) {
          current.run <- runs[[run.i]]
          current.reacts <- S7::prop(current.run, "react")

          for (react in current.reacts) {
            react.id <- .get_id(react)
            existing <- .list_get_by_key(first.reacts, react.id, "id")

            if (is.null(existing)) {
              first.reacts <- .list_set_by_key(first.reacts, react.id, react, "id")
            } else {
              existing_data <- S7::prop(existing, "data")
              new_data <- S7::prop(react, "data")
              if (.is_single_na(existing_data)) existing_data <- list()
              if (.is_single_na(new_data)) new_data <- list()
              S7::prop(existing, "data") <- c(existing_data, new_data)
              first.reacts <- .list_set_by_key(first.reacts, react.id, existing, "id")
            }
          }
        }

        S7::prop(first.run, "react") <- first.reacts
        S7::prop(first.run, "id") <- idType("Combined Run")
        S7::prop(exp1, "run") <- list(first.run)
        rdml_obj$experiment[[1]] <- exp1
      }
    }

    # Roche LC96 post-processing ------------------------------------------
    if (is_roche) {
      # Rename sample ids to descriptions. Lists stay physically unnamed;
      # later rdml$sample exposes them through rdmlKeyedList.
      for (i in seq_along(rdml_obj$sample)) {
        desc <- rdml_obj$sample[[i]]$description
        if (!is.na(desc) && nzchar(desc)) {
          rdml_obj$sample[[i]]$id <- idType(desc)
        }
      }

      # Add Roche reference-gene flags.
      if (!is.null(ref.genes.r) && length(ref.genes.r) != 0L && !.is_single_na(ref.genes.r)) {
        ns <- xml2::xml_ns_rename(xml2::xml_ns(ref.genes.r), d3 = "rel")
        for (i in seq_along(ref.genes.r)) {
          ref.gene <- ref.genes.r[[i]]
          geneName <- getTextValue(ref.gene, "rel:geneName", ns = ns)
          target_obj <- .list_get_by_key(rdml_obj$target, geneName, "id")
          if (!is.null(target_obj)) {
            is_ref <- getLogicalValue(ref.gene, "rel:isReference", ns = ns)
            target_obj$type <- targetTypeType(if (isTRUE(is_ref)) "ref" else "toi")
            rdml_obj$target <- .list_set_by_key(rdml_obj$target, geneName, target_obj, "id")
          }
        }
      }

      # Add Roche quantities using the target-aware quantityType schema.
      if (is.list(dilutions.r) && length(dilutions.r)) {
        tbl <- as_table_rdml(rdml_obj)
        data.table::setkey(tbl, react.id)

        for (target_id in names(dilutions.r)) {
          for (r.id in names(dilutions.r[[target_id]])) {
            sample.name <- tbl[react.id == base::as.integer(r.id), sample][1]
            if (is.na(sample.name)) next

            sample_obj <- .list_get_by_key(rdml_obj$sample, sample.name, "id")
            if (is.null(sample_obj)) next

            quantities <- S7::prop(sample_obj, "quantity")
            if (.is_single_na(quantities)) quantities <- list()
            quantities[[length(quantities) + 1L]] <- quantityType(
              targetId = idReferenceType(target_id),
              value = unname(dilutions.r[[target_id]][r.id]),
              unit = quantityUnitType("other")
            )
            S7::prop(sample_obj, "quantity") <- quantities
            rdml_obj$sample <- .list_set_by_key(rdml_obj$sample, sample.name, sample_obj, "id")
          }
        }
      }

      # Roche-derived conditions are kept separate for now. Native RDML
      # <annotation> elements are imported above; mapping Roche condition
      # metadata into annotations is intentionally deferred to a vendor-specific
      # cleanup pass so this refactor does not change import semantics.
      invisible(conditions.r)
    }

    rdml_obj
  }

  # File-format dispatch ----------------------------------------------------
  if (identical(tolower(format), "auto")) {
    ext <- tolower(tools::file_ext(filename))
    format <- switch(
      ext,
      "eds" = "abi",
      "rex" = "rotorgene",
      "xlsx" = "excel",
      "xls" = "excel",
      "csv" = "csv",
      "r96" = "dtprime",
      "txt" = "fqd",
      "xml" = "xml",
      "rdml" = "rdml",
      "lc96p" = "rdml",
      # Preserve the historical upstream fallback: unknown extensions are
      # attempted as RDML archives.
      "rdml"
    )
  } else {
    format <- switch(
      tolower(format),
      "abi" = "abi",
      "eds" = "abi",
      "rotorgene" = "rotorgene",
      "rotor-gene" = "rotorgene",
      "rex" = "rotorgene",
      "excel" = "excel",
      "xlsx" = "excel",
      "xls" = "excel",
      "csv" = "csv",
      "dtprime" = "dtprime",
      "r96" = "dtprime",
      "fqd" = "fqd",
      "fqd96" = "fqd",
      "txt" = "fqd",
      "xml" = "xml",
      "rdml" = "rdml",
      "lc96p" = "rdml",
      stop("Unsupported import format: ", format, call. = FALSE)
    )
  }

  if (identical(format, "abi")) {
    return(fromABI())
  }
  if (identical(format, "rotorgene")) {
    return(fromRotorGene())
  }
  if (identical(format, "excel")) {
    return(fromExcel())
  }
  if (identical(format, "csv")) {
    return(fromCSV())
  }
  if (identical(format, "dtprime")) {
    return(fromDTprime())
  }
  if (identical(format, "fqd")) {
    return(fromFQDexport())
  }

  # RDML XML/archive import keeps the richer dedicated parser above.
  rdml_obj <- fromRDML()
  do.call(rdmlType, rdml_obj)
}
