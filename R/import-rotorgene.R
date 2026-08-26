# fromRotorGene importer -----------------------------------------------------------

.rdmlImportRotorGene <- function(fileName, showProgress = TRUE) {
  fromRotorGene <- function() {
    dat <- xml2::read_xml(fileName)
    rdmlEnv$ns <- xml2::xml_ns(dat)

    sampleNodes <- xml2::xml_find_all(
      dat,
      "/Experiment/Samples/Page/Sample[Name]"
    )

    if (!length(sampleNodes)) {
      stop("No samples found in Rotor-Gene .rex file", call. = FALSE)
    }

    description <- do.call(
      rbind,
      lapply(seq_along(sampleNodes), function(i) {
        el <- sampleNodes[[i]]
        typeRaw <- .getTextValue(el, "Type")
        typeMap <- c("5" = "pos", "3" = "ntc", "1" = "std")
        sampleType <- unname(typeMap[as.character(typeRaw)])
        if (!length(sampleType) || is.na(sampleType)) sampleType <- "unkn"

        quantity <- .rdmlAsNumeric(.getTextValue(el, "GivenConc"))
        if (!length(quantity)) quantity <- NA_real_ else quantity <- quantity[[1L]]

        data.frame(
          fdataName = .getTextValue(el, "ID"),
          expId = "exp1",
          runId = "run1",
          reactId = base::as.numeric(.getTextValue(el, "TubePosition")),
          sample = .getTextValue(el, "Name"),
          sampleType = sampleType,
          quantity = quantity,
          target = NA_character_,
          targetDyeId = NA_character_,
          stringsAsFactors = FALSE
        )
      })
    )

    groups <- xml2::xml_find_all(dat, "/Experiment/Samples/Groups/Group")
    for (i in seq_along(groups)) {
      group <- groups[[i]]
      groupName <- .getTextValue(group, "Name")
      tubeNodes <- xml2::xml_find_all(group, ".//Tube")
      ids <- xml2::xml_text(tubeNodes)
      description$target[description$fdataName %in% ids] <- groupName
    }

    originalTargets <- description$target
    originalTargets[is.na(originalTargets) | !nzchar(originalTargets)] <- "unkn"

    channels <- xml2::xml_find_all(dat, "/Experiment/RawChannels/RawChannel")
    if (!length(channels)) {
      stop("No raw channels found in Rotor-Gene .rex file", call. = FALSE)
    }

    x <- .rdmlNewImport("RotorGene", "1")

    for (i in seq_along(channels)) {
      rawChannel <- channels[[i]]
      dyeId <- .getTextValue(rawChannel, "Name")
      description$targetDyeId <- dyeId
      description$target <- paste(originalTargets, dyeId, sep = "#")

      readings <- .getTextVector(
        rawChannel,
        sprintf("Name[text()='%s']/../Reading", dyeId)
      )
      if (!length(readings)) {
        readings <- xml2::xml_text(xml2::xml_find_all(rawChannel, ".//Reading"))
      }

      ridx <- base::as.integer(description$reactId)
      selected <- readings[ridx]
      curves <- lapply(selected, function(z) {
        if (is.na(z) || !nzchar(z)) return(numeric())
        .rdmlAsNumeric(.splitWs(z))
      })

      lens <- lengths(curves)
      if (!length(lens) || any(lens == 0L)) next
      if (length(unique(lens)) != 1L) {
        stop("Rotor-Gene fluorescence curves have unequal lengths", call. = FALSE)
      }

      mat <- do.call(cbind, curves)
      fdata <- data.frame(cyc = seq_len(nrow(mat)), check.names = FALSE)
      for (j in seq_len(ncol(mat))) {
        fdata[[as.character(description$fdataName[[j]])]] <- mat[, j]
      }

      x <- .rdmlsetFDataImport(x, fdata, description, "adp")
    }

    x
  }

  fromRotorGene()
}
