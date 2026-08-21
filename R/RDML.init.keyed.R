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
  if (.is_xml_missing(node) ) return(NA_character_)
  txt <- xml2::xml_text(node)
  if (!testString(txt,
                  min.chars = 1,
                  pattern = "[^[:space:]]",
                  na.ok = TRUE)) return(NA_character_)
  txt
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

#' Read RDML file and return S7 rdmlType object
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
    dateUpadted = NA_character_,
    id = list(),
    experimenter = list(),
    documentation = list(),
    dye = list(),
    sample = list(),
    target = list(),
    thermalCyclingConditions = list(),
    experiment = list()
  )

  as_table_rdml <- function(rdml_obj) {
    rows <- list()
    n <- 0L

    for (exp in rdml_obj$experiment) {
      runs <- prop(exp, "run")
      if (.is_single_na(runs) || !length(runs)) next

      for (run in runs) {
        reacts <- prop(run, "react")
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
        targetId = idType(target_id),
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
        targetId = idType(target_id),
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
    rdml_obj$dateUpadted <- getTextValue(rdml.doc, "/rdml:rdml/rdml:dateUpdated")

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
        id <- genId(node)
        
        # for BioRad empty firs/lastname
        firstName <- getTextValue(node, "rdml:firstName")
        if(is.na(firstName)) {
          firstName <- id@id
        }
        lastName <- getTextValue(node, "rdml:lastName")
        if(is.na(lastName)) {
          lastName <- id@id
        }
        
        experimenterType(
          id = id,
          firstName = firstName,
          lastName = lastName,
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
          cdnaSynthesisMethodType(
            enzyme = getTextValue(cdna_node, "rdml:enzyme"),
            primingMethod = getNumericValue(cdna_node, "rdml:primingMethod"),
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
        patitions = {
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
        backgroundDetermenationMethod = getTextValue(run, "rdml:backgroundDeterminationMethod"),
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
      runs <- prop(exp1, "run")

      if (is.list(runs) && length(runs) > 1L) {
        if (show.progress) cat("\nCombining Bio-Rad runs\n")
        first.run <- runs[[1]]
        first.reacts <- prop(first.run, "react")

        for (run.i in 2:length(runs)) {
          current.run <- runs[[run.i]]
          current.reacts <- prop(current.run, "react")

          for (react in current.reacts) {
            react.id <- .get_id(react)
            existing <- .list_get_by_key(first.reacts, react.id, "id")

            if (is.null(existing)) {
              first.reacts <- .list_set_by_key(first.reacts, react.id, react, "id")
            } else {
              existing_data <- prop(existing, "data")
              new_data <- prop(react, "data")
              if (.is_single_na(existing_data)) existing_data <- list()
              if (.is_single_na(new_data)) new_data <- list()
              prop(existing, "data") <- c(existing_data, new_data)
              first.reacts <- .list_set_by_key(first.reacts, react.id, existing, "id")
            }
          }
        }

        prop(first.run, "react") <- first.reacts
        prop(first.run, "id") <- idType("Combined Run")
        prop(exp1, "run") <- list(first.run)
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

            quantities <- prop(sample_obj, "quantity")
            if (.is_single_na(quantities)) quantities <- list()
            quantities[[length(quantities) + 1L]] <- quantityType(
              targetId = idType(target_id),
              value = unname(dilutions.r[[target_id]][r.id]),
              unit = quantityUnitType("other")
            )
            prop(sample_obj, "quantity") <- quantities
            rdml_obj$sample <- .list_set_by_key(rdml_obj$sample, sample.name, sample_obj, "id")
          }
        }
      }

      # conditions.r used to be written into sample@annotation. The current
      # sampleType in types7.R has no annotation property, so it is deliberately
      # not injected here. Keeping this silent avoids creating invalid S7 data.
      invisible(conditions.r)
    }

    rdml_obj
  }

  if (identical(format, "auto")) {
    ext <- tolower(tools::file_ext(filename))
    format <- switch(
      ext,
      "xml" = "xml",
      "rdml" = "rdml",
      "lc96p" = "rdml",
      if (identical(ext, "")) "rdml" else ext
    )
  }

  if (!format %in% c("rdml", "xml")) {
    stop(
      "Unsupported format for S7 rdml_read(): ", format,
      ". Use an RDML archive (.rdml/.lc96p) or RDML XML file."
    )
  }

  rdml_obj <- fromRDML()
  
  do.call(rdmlType, rdml_obj)
}
