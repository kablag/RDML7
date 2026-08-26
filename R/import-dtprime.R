# fromDTprime importer -----------------------------------------------------------

.rdml_import_dtprime <- function(filename, show.progress = TRUE) {
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

    description[, react.id := base::as.integer(react.id)]
    description[, quantity := suppressWarnings(base::as.numeric(quantity))]

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

    x <- .rdml_new_import("DTprime", "1")

    .rdml_set_fdata_import(
      x,
      fdata,
      description,
      "adp"
    )
  }

  fromDTprime()
}
