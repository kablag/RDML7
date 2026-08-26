#' Set fluorescence data in rdmlType
#'
#' RDML7 is value-based (S7), unlike the original mutable R6 RDML object.
#' Therefore the returned object must be assigned back:
#' `rdml <- SetFData(rdml, fdata, description)`.
#'
#' @param x rdmlType object.
#' @param fdata Matrix/data.frame/data.table. The first column is cyc/tmp;
#'   remaining fluorescence columns are named by description$fdata.name. For
#'   amplification data an auxiliary column named `tmp` is preserved when it is
#'   not itself listed as a fluorescence series.
#' @param description Table produced by AsTable().
#' @param fdata.type "adp" or "mdp".
#' @param conflict How to handle conflicting existing sample/target metadata.
#' @return Modified rdmlType object.
#' @export
#' @include generics.R rdml-utils.R
S7::method(SetFData, rdmlType) <- function(
    x,
    fdata,
    description,
    fdata.type = "adp",
    conflict = c("error", "keep", "replace"),
    ...) {

  checkmate::assertChoice(fdata.type, c("adp", "mdp"))
  conflict <- match.arg(conflict)

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required", call. = FALSE)
  }

  fdata <- data.table::as.data.table(fdata)
  description <- data.table::as.data.table(description)

  if (ncol(fdata) < 2L) {
    stop(
      "fdata must have a coordinate column and at least one fluorescence column",
      call. = FALSE
    )
  }

  required <- c("fdata.name", "exp.id", "run.id", "react.id", "target")
  missing_cols <- setdiff(required, names(description))
  if (length(missing_cols)) {
    stop(
      "description is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  coordinate <- as.numeric(fdata[[1L]])
  described_names <- as.character(description[["fdata.name"]])

  # Optional per-cycle amplification temperature. It is metadata, not a
  # fluorescence series, unless description explicitly names a series "tmp".
  adp_tmp <- NULL
  fdata_names <- names(fdata)[-1L]
  if (
    identical(fdata.type, "adp") &&
    "tmp" %in% fdata_names &&
    !("tmp" %in% described_names)
  ) {
    adp_tmp <- as.numeric(fdata[["tmp"]])
    fdata_names <- setdiff(fdata_names, "tmp")
  }

  value1 <- function(row, name, default = NA) {
    if (!name %in% names(row)) return(default)
    value <- row[[name]]
    if (!length(value)) default else value[[1L]]
  }

  nonempty_string <- function(value) {
    length(value) == 1L && !is.na(value) && nzchar(as.character(value))
  }

  make_pcr_format <- function(exp_id, run_id) {
    idx <- description[["exp.id"]] == exp_id & description[["run.id"]] == run_id
    react_ids <- suppressWarnings(base::as.integer(description[["react.id"]][idx]))

    max_react <- if (length(react_ids) && any(!is.na(react_ids))) {
      max(react_ids, na.rm = TRUE)
    } else {
      96L
    }

    if (max_react > 96L) {
      pcrFormatType(
        rows = 16L,
        columns = 24L,
        rowLabel = labelFormatType("ABC"),
        columnLabel = labelFormatType("123")
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

  handle_conflict <- function(message) {
    if (identical(conflict, "error")) stop(message, call. = FALSE)
    invisible(NULL)
  }

  # Pull top-level collections once. Updating these wrappers in memory avoids
  # rebuilding x for every fluorescence series.
  experiments <- .rdml_prop_keyed(x, "experiment")
  samples <- .rdml_prop_keyed(x, "sample")
  targets <- .rdml_prop_keyed(x, "target")
  dyes <- .rdml_prop_keyed(x, "dye")

  for (fdata_name in fdata_names) {
    idx <- which(described_names == fdata_name)

    if (!length(idx)) {
      warning(
        "No description for fluorescence column: ", fdata_name,
        call. = FALSE
      )
      next
    }
    if (length(idx) > 1L) {
      stop(
        "Multiple description rows for fluorescence column: ", fdata_name,
        call. = FALSE
      )
    }

    row <- description[idx]
    exp_id <- as.character(value1(row, "exp.id"))
    run_id <- as.character(value1(row, "run.id"))
    react_id <- as.character(value1(row, "react.id"))
    target_id <- as.character(value1(row, "target"))
    sample_id <- as.character(value1(row, "sample"))
    sample_type <- as.character(value1(row, "sample.type"))
    dye_id <- as.character(value1(row, "target.dyeId"))

    if (!all(vapply(
      list(exp_id, run_id, react_id, target_id),
      nonempty_string,
      logical(1)
    ))) {
      stop(
        "exp.id, run.id, react.id and target must be non-empty for ",
        fdata_name,
        call. = FALSE
      )
    }

    # experiment ----------------------------------------------------------
    experiment <- experiments[[exp_id]]
    if (is.null(experiment)) {
      experiment <- experimentType(id = idType(exp_id), run = list())
    }

    # run -----------------------------------------------------------------
    runs <- .rdml_prop_keyed(experiment, "run")
    run <- runs[[run_id]]
    if (is.null(run)) {
      run <- runType(
        id = idType(run_id),
        pcrFormat = make_pcr_format(exp_id, run_id),
        react = list()
      )
    }

    # react ---------------------------------------------------------------
    reacts <- .rdml_prop_keyed(run, "react")
    react <- reacts[[react_id]]

    if (is.null(react)) {
      if (!nonempty_string(sample_id)) {
        stop(
          "Cannot create react '", react_id,
          "' without a `sample` value in description",
          call. = FALSE
        )
      }

      react <- reactType(
        id = idType(react_id),
        sample = idReferenceType(sample_id),
        data = list(),
        partitions = list()
      )
    } else if (nonempty_string(sample_id)) {
      old_sample <- .rdml_id_chr(react$sample)
      if (!is.na(old_sample) && !identical(old_sample, sample_id)) {
        handle_conflict(sprintf(
          "React '%s' already references sample '%s', not '%s'",
          react_id, old_sample, sample_id
        ))
        if (identical(conflict, "replace")) {
          S7::prop(react, "sample") <- idReferenceType(sample_id)
        }
      }
    }

    # dataType keyed by targetId -----------------------------------------
    data_list <- .rdml_prop_keyed(react, "data")
    data_obj <- data_list[[target_id]]

    if (is.null(data_obj)) {
      data_obj <- dataType(
        targetId = idReferenceType(target_id),
        cq = NA_real_,
        N0 = NA_real_,
        ampEffMet = NA_character_,
        ampEff = NA_real_,
        ampEffSE = NA_real_,
        corrF = NA_real_,
        corrP = NA_real_,
        meltTemp = NA_real_,
        excl = NA_character_,
        note = NA_character_,
        adp = NA,
        mdp = NA,
        endPt = NA_real_,
        bgFluor = NA_real_,
        bgFluorSlp = NA_real_,
        quantFluor = NA_real_
      )
    }

    if ("cq" %in% names(row)) {
      cq <- value1(row, "cq", NA_real_)
      if (length(cq) == 1L) {
        S7::prop(data_obj, "cq") <- as.numeric(cq)
      }
    }

    fluor <- as.numeric(fdata[[fdata_name]])

    if (identical(fdata.type, "adp")) {
      fpoints <- if (is.null(adp_tmp)) {
        data.table::data.table(cyc = coordinate, fluor = fluor)
      } else {
        data.table::data.table(cyc = coordinate, tmp = adp_tmp, fluor = fluor)
      }
      S7::prop(data_obj, "adp") <- dpAmpCurveType(fpoints = fpoints)
    } else {
      S7::prop(data_obj, "mdp") <- dpMeltingCurveType(
        fpoints = data.table::data.table(tmp = coordinate, fluor = fluor)
      )
    }

    data_list[[target_id]] <- data_obj
    react <- .rdml_set_prop_list(react, "data", data_list)

    reacts[[react_id]] <- react
    run <- .rdml_set_prop_list(run, "react", reacts)

    runs[[run_id]] <- run
    experiment <- .rdml_set_prop_list(experiment, "run", runs)

    experiments[[exp_id]] <- experiment

    # sample --------------------------------------------------------------
    if (nonempty_string(sample_id)) {
      sample_obj <- samples[[sample_id]]
      if (is.null(sample_obj)) {
        sample_obj <- sampleType(
          id = idType(sample_id),
          type = list(),
          quantity = list()
        )
      }

      if (nonempty_string(sample_type)) {
        st <- sampleTargetType(
          targetId = idReferenceType(target_id),
          sampleType = sampleTypeType(sample_type)
        )
        sample_types <- .rdml_prop_keyed(sample_obj, "type")
        sample_types[[target_id]] <- st
        sample_obj <- .rdml_set_prop_list(sample_obj, "type", sample_types)
      }

      if ("quantity" %in% names(row)) {
        quantity <- value1(row, "quantity", NA_real_)
        if (length(quantity) == 1L && !is.na(quantity)) {
          q <- quantityType(
            targetId = idReferenceType(target_id),
            value = as.numeric(quantity),
            unit = quantityUnitType("other")
          )
          quantities <- .rdml_prop_keyed(sample_obj, "quantity")
          quantities[[target_id]] <- q
          sample_obj <- .rdml_set_prop_list(sample_obj, "quantity", quantities)
        }
      }

      samples[[sample_id]] <- sample_obj
    }

    # target/dye ----------------------------------------------------------
    target_obj <- targets[[target_id]]
    if (is.null(target_obj)) {
      if (nonempty_string(dye_id)) {
        target_obj <- targetType(
          id = idType(target_id),
          description = NA_character_,
          documentation = list(),
          xRef = list(),
          type = targetTypeType("toi"),
          amplificationEfficiencyMethod = NA_character_,
          amplificationEfficiency = NA_real_,
          amplificationEfficiencySE = NA_real_,
          meltingTemperature = NA_real_,
          detectionLimit = NA_real_,
          dyeId = idReferenceType(dye_id),
          sequences = NA,
          commercialAssay = NA
        )
        targets[[target_id]] <- target_obj
      } else {
        warning(
          "Target '", target_id,
          "' is absent and cannot be created because target.dyeId is missing",
          call. = FALSE
        )
      }
    } else if (nonempty_string(dye_id)) {
      old_dye <- .rdml_id_chr(target_obj$dyeId)
      if (!is.na(old_dye) && !identical(old_dye, dye_id)) {
        handle_conflict(sprintf(
          "Target '%s' already references dye '%s', not '%s'",
          target_id, old_dye, dye_id
        ))
        if (identical(conflict, "replace")) {
          S7::prop(target_obj, "dyeId") <- idReferenceType(dye_id)
          targets[[target_id]] <- target_obj
        }
      }
    }

    if (nonempty_string(dye_id) && is.null(dyes[[dye_id]])) {
      dyes[[dye_id]] <- dyeType(id = idType(dye_id))
    }
  }

  x <- .rdml_set_prop_list(x, "experiment", experiments)
  x <- .rdml_set_prop_list(x, "sample", samples)
  x <- .rdml_set_prop_list(x, "target", targets)
  x <- .rdml_set_prop_list(x, "dye", dyes)
  x
}
