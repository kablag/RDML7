# Native RDML/XML importer --------------------------------------------------

.rdml_import_rdml <- function(
    filename,
    show.progress = TRUE,
    conditions.sep = NULL,
    cluster = NULL,
    format = "rdml") {

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
      # losslessly by the current S7 schema, so leave it out.
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
        sample_obj <- rdml_obj$sample[[i]]
        desc <- S7::prop(sample_obj, "description")

        if (!is.na(desc) && nzchar(desc)) {
          S7::prop(sample_obj, "id") <- idType(desc)
          rdml_obj$sample[[i]] <- sample_obj
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
            S7::prop(target_obj, "type") <-
              targetTypeType(if (isTRUE(is_ref)) "ref" else "toi")
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

  rdml_obj <- fromRDML()
  do.call(rdmlType, rdml_obj)
}
