# fromDTprime importer -----------------------------------------------------------

.rdmlImportDtprime <- function(fileName, showProgress = TRUE) {
  fromDTprime <- function() {
    if (!requireNamespace("data.table", quietly = TRUE)) {
      stop(
        "Package 'data.table' is required for DTprime .r96 import",
        call. = FALSE
      )
    }

    dt96OverloadSignal <- 15000

    # Read as raw bytes first. readLines(..., encoding = "Windows-1251")
    # is not reliable on all Windows/R builds: the returned strings may still
    # contain CP1251 bytes but be treated as UTF-8, causing trimws()/sub()
    # to fail on e.g. "Апрель".
    raw <- readBin(
      fileName,
      what = "raw",
      n = file.info(fileName)$size
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
    iTubes   <- marker("$Information about tubes:$", mode = "prefix")
    iSamples <- marker("$Information about Samples:$", mode = "prefix")
    iMulti   <- marker("$MultiChannel:$", mode = "prefix")
    iDevice  <- marker("^\\$Device", mode = "regex")
    iTests   <- marker("$Information about TESTs:$", mode = "prefix")
    iMut     <- marker("$Parameters MutationMC:$", mode = "prefix")
    iResults <- marker("$Results of optical measurements:$", mode = "prefix")

    requiredMarkers <- c(
      tubes = iTubes,
      multi = iMulti,
      device = iDevice,
      tests = iTests,
      mutation = iMut,
      results = iResults
    )

    if (anyNA(requiredMarkers)) {
      stop(
        "Unsupported or malformed DTprime .r96 file; missing section(s): ",
        paste(names(requiredMarkers)[is.na(requiredMarkers)], collapse = ", "),
        call. = FALSE
      )
    }

    # ---- Tube table ----------------------------------------------------
    # Current .r96 tube rows contain 8 meaningful whitespace-separated
    # fields. Example:
    #   0  2 100 1 120 c0 1 CD53_1
    # They used to become 10 tokens only because the old code replaced tabs
    # and split on a single space, preserving empty tokens.
    tubeEnd <- if (!is.na(iSamples) && iSamples > iTubes) {
      iSamples - 1L
    } else {
      iMulti - 1L
    }

    tubeLines <- lns[(iTubes + 1L):tubeEnd]
    tubeLines <- tubeLines[
      grepl("^\\s*[0-9]+\\s+", tubeLines)
    ]

    tubesInfo <- lapply(tubeLines, .splitWs)
    tubesInfo <- Filter(
      function(z) {
        length(z) >= 8L &&
          grepl("^[0-9]+$", z[[1L]])
      },
      tubesInfo
    )

    if (!length(tubesInfo)) {
      stop("No tube records found in DTprime .r96 file", call. = FALSE)
    }

    # ---- Test/kit table ------------------------------------------------
    kitLines <- lns[(iTests + 1L):(iMut - 1L)]
    kits <- lapply(kitLines, .splitWs)
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

    if (iDevice > iMulti + 1L) {
      concLines <- lns[(iMulti + 1L):(iDevice - 1L)]
      concLines <- concLines[
        nzchar(trimws(concLines)) &
          grepl("^\\s*[0-9]+(?:\\s|$)", concLines)
      ]

      concentrations <- lapply(concLines, .splitWs)
    }

    # ---- Optical measurements -----------------------------------------
    measurementLines <- lns[(iResults + 1L):length(lns)]
    measurementLines <- measurementLines[nzchar(trimws(measurementLines))]

    parts <- lapply(measurementLines, .splitWs)

    if (!length(parts)) {
      stop("DTprime file contains no optical measurements", call. = FALSE)
    }

    partLengths <- vapply(parts, length, integer(1))

    if (length(unique(partLengths)) != 1L) {
      stop(
        "DTprime optical-data rows have inconsistent field counts: ",
        paste(sort(unique(partLengths)), collapse = ", "),
        call. = FALSE
      )
    }

    fraw <- data.table::as.data.table(
      data.table::transpose(parts, fill = NA_character_)
    )

    # 7 metadata fields + 96 tube fields = 103 meaningful fields.
    # Older RDML code expected a 104th '?' column solely because splitting
    # lines ending in whitespace produced a trailing empty string.
    rawNames103 <- c(
      "dye",
      "x1",
      "x2",
      "x3",
      "cycle",
      "exposition",
      "background",
      paste0("tube_", 0:95)
    )

    if (ncol(fraw) == length(rawNames103)) {
      data.table::setnames(fraw, rawNames103)
    } else if (ncol(fraw) == length(rawNames103) + 1L) {
      data.table::setnames(fraw, c(rawNames103, "?"))
    } else {
      stop(
        "Unexpected DTprime optical-data column count: ",
        ncol(fraw),
        " (expected ",
        length(rawNames103),
        " meaningful columns",
        " or ",
        length(rawNames103) + 1L,
        " including a trailing empty field)",
        call. = FALSE
      )
    }

    nRaw <- nrow(fraw)

    # Each PCR cycle has 5 dyes x 2 exposure rows.
    if (nRaw %% 10L != 0L) {
      stop(
        "Unexpected DTprime optical-data row count: ",
        nRaw,
        " (must be divisible by 10: 5 dyes x 2 exposures)",
        call. = FALSE
      )
    }

    nCycles <- nRaw %/% 10L
    fdata <- data.table::data.table(cyc = seq_len(nCycles))

    descrRows <- list()
    nr <- 0L
    dyes <- c("FAM", "HEX", "ROX", "Cy5", "Cy5.5")

    # Helpers for normalized DTprime tables -----------------------------
    kitNameFor <- function(kitId) {
      for (kit in kits) {
        if (length(kit) >= 2L && identical(kit[[1L]], kitId)) {
          return(kit[[2L]])
        }
      }
      "unkn"
    }

    concentrationFor <- function(tubeId) {
      for (conc in concentrations) {
        # Normalized form corresponds to old conc[2] / conc[4].
        # With empty tokens removed these are normally fields 1 / 3.
        if (length(conc) >= 3L && identical(conc[[1L]], tubeId)) {
          return(suppressWarnings(base::as.numeric(conc[[3L]])))
        }
      }
      NA_real_
    }

    for (tube in tubesInfo) {
      # Normalized tube layout:
      # 1 = zero-based tube id
      # 7 = TEST/kit id
      # 8 = sample/tube name
      tubeId   <- tube[[1L]]
      kitId    <- tube[[7L]]
      tubeName <- tube[[8L]]

      if (identical(tubeName, "-")) {
        next
      }

      tubeCol <- paste0("tube_", tubeId)

      if (!(tubeCol %in% names(fraw))) {
        next
      }

      kitName <- kitNameFor(kitId)
      quantity <- concentrationFor(tubeId)

      for (dyeI in seq_along(dyes)) {
        dye <- dyes[[dyeI]]

        firstIdx <- (dyeI - 1L) * 2L + 1L
        secondIdx <- firstIdx + 1L

        if (secondIdx > nRaw) {
          next
        }

        markerValue <- fraw[[tubeCol]][[firstIdx]]

        # DTprime stores "1" for channels/tubes that were not measured.
        if (is.na(markerValue) || identical(markerValue, "1")) {
          next
        }

        idx2000 <- seq(firstIdx, nRaw, by = 10L)
        idx400  <- seq(secondIdx, nRaw, by = 10L)

        if (
          length(idx2000) != nCycles ||
          length(idx400) != nCycles
        ) {
          stop(
            "Inconsistent DTprime exposure series for tube ",
            tubeId,
            ", dye ",
            dye,
            call. = FALSE
          )
        }

        sig2000 <-
          suppressWarnings(base::as.numeric(fraw[[tubeCol]][idx2000])) -
          suppressWarnings(base::as.numeric(fraw$background[idx2000]))

        sig400 <-
          suppressWarnings(base::as.numeric(fraw[[tubeCol]][idx400])) -
          suppressWarnings(base::as.numeric(fraw$background[idx400]))

        if (
          all(is.na(sig2000)) ||
          all(is.na(sig400))
        ) {
          next
        }

        sigcomb <- if (
          !any(sig2000 >= dt96OverloadSignal, na.rm = TRUE)
        ) {
          sig2000
        } else {
          sig400 * 5
        }

        # fdataName is an internal fluorescence-series key and must be
        # unique. Sample names may repeat for technical replicates, therefore
        # include the zero-based DTprime tube id.
        nm2000 <- sprintf("tube_%s_%s_2000", tubeId, dye)
        nm400  <- sprintf("tube_%s_%s_400",  tubeId, dye)
        nmcomb <- sprintf("tube_%s_%s_comb", tubeId, dye)

        fdata[[nm2000]] <- sig2000
        fdata[[nm400]]  <- sig400
        fdata[[nmcomb]] <- sigcomb

        baseRow <- list(
          runId = "run1",
          reactId = base::as.integer(tubeId) + 1L,
          sample = tubeName,
          target = sprintf("%s#%s", kitName, dye),
          targetDyeId = dye,
          sampleType = "unkn",
          quantity = quantity
        )

        nr <- nr + 1L
        descrRows[[nr]] <- data.frame(
          fdataName = nm2000,
          expId = "exp_2000",
          runId = baseRow$runId,
          reactId = baseRow$reactId,
          sample = baseRow$sample,
          target = baseRow$target,
          targetDyeId = baseRow$targetDyeId,
          sampleType = baseRow$sampleType,
          quantity = baseRow$quantity,
          stringsAsFactors = FALSE
        )

        nr <- nr + 1L
        descrRows[[nr]] <- data.frame(
          fdataName = nm400,
          expId = "exp_400",
          runId = baseRow$runId,
          reactId = baseRow$reactId,
          sample = baseRow$sample,
          target = baseRow$target,
          targetDyeId = baseRow$targetDyeId,
          sampleType = baseRow$sampleType,
          quantity = baseRow$quantity,
          stringsAsFactors = FALSE
        )

        nr <- nr + 1L
        descrRows[[nr]] <- data.frame(
          fdataName = nmcomb,
          expId = "combined",
          runId = baseRow$runId,
          reactId = baseRow$reactId,
          sample = baseRow$sample,
          target = baseRow$target,
          targetDyeId = baseRow$targetDyeId,
          sampleType = baseRow$sampleType,
          quantity = baseRow$quantity,
          stringsAsFactors = FALSE
        )
      }
    }

    if (!length(descrRows) || ncol(fdata) < 2L) {
      stop(
        "No usable fluorescence data found in DTprime .r96 file",
        call. = FALSE
      )
    }

    description <- data.table::rbindlist(
      descrRows,
      use.names = TRUE,
      fill = TRUE
    )

    description[, reactId := base::as.integer(reactId)]
    description[, quantity := suppressWarnings(base::as.numeric(quantity))]

    if (showProgress) {
      cat(
        sprintf(
          "\nDTprime: %d cycles, %d fluorescence series, %d active reactions\n",
          nCycles,
          ncol(fdata) - 1L,
          data.table::uniqueN(description$reactId)
        )
      )
    }

    x <- .rdmlNewImport("DTprime", "1")

    .rdmlsetFDataImport(
      x,
      fdata,
      description,
      "adp"
    )
  }

  fromDTprime()
}
