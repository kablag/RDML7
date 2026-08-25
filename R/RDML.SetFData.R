#' Set fluorescence data in rdmlType
#'
#' rdml7 is value-based (S7), unlike the original mutable R6 RDML object.
#' Therefore the returned object must be assigned back:
#' `rdml <- SetFData(rdml, fdata, description)`.
#'
#' @param x rdmlType object.
#' @param fdata Matrix/data.frame/data.table. First column is cyc/tmp; remaining
#'   columns are fluorescence vectors named by description$fdata.name.
#' @param description Table produced by AsTable().
#' @param fdata.type "adp" or "mdp".
#' @return Modified rdmlType object.
#' @export
#' @include generics.R rdml-utils.R
S7::method(SetFData, rdmlType) <- function(
    x,
    fdata,
    description,
    fdata.type = "adp",
    ...) {
  
  checkmate::assertChoice(fdata.type, c("adp", "mdp"))
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required", call. = FALSE)
  }
  
  fdata <- data.table::as.data.table(fdata)
  description <- data.table::as.data.table(description)
  
  if (ncol(fdata) < 2L) {
    stop("fdata must have a coordinate column and at least one fluorescence column",
         call. = FALSE)
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
  
  coordinate <- fdata[[1L]]
  fdata_names <- names(fdata)[-1L]
  
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
    
    react_ids <- suppressWarnings(
      base::as.integer(description[["react.id"]][idx])
    )
    
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
  
  for (fdata_name in fdata_names) {
    idx <- which(as.character(description[["fdata.name"]]) == fdata_name)
    
    if (!length(idx)) {
      warning("No description for fluorescence column: ", fdata_name,
              call. = FALSE)
      next
    }
    if (length(idx) > 1L) {
      stop("Multiple description rows for fluorescence column: ", fdata_name,
           call. = FALSE)
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
    
    # Top-level experiment -------------------------------------------------
    experiments <- .rdml_prop_keyed(x, "experiment")
    experiment <- experiments[[exp_id]]
    if (is.null(experiment)) {
      experiment <- experimentType(id = idType(exp_id), run = list())
    }
    
    # Run ------------------------------------------------------------------
    runs <- .rdml_prop_keyed(experiment, "run")
    run <- runs[[run_id]]
    if (is.null(run)) {
      run <- runType(
        id = idType(run_id),
        pcrFormat = make_pcr_format(exp_id, run_id),
        react = list()
      )
    }
    
    # React ----------------------------------------------------------------
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
    }
    
    # dataType keyed by targetId ------------------------------------------
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
      S7::prop(data_obj, "adp") <- dpAmpCurveType(
        fpoints = data.table::data.table(
          cyc = as.numeric(coordinate),
          fluor = fluor
        )
      )
    } else {
      S7::prop(data_obj, "mdp") <- dpMeltingCurveType(
        fpoints = data.table::data.table(
          tmp = as.numeric(coordinate),
          fluor = fluor
        )
      )
    }
    
    data_list[[target_id]] <- data_obj
    react <- .rdml_set_prop_list(react, "data", data_list)
    
    reacts[[react_id]] <- react
    run <- .rdml_set_prop_list(run, "react", reacts)
    
    runs[[run_id]] <- run
    experiment <- .rdml_set_prop_list(experiment, "run", runs)
    
    experiments[[exp_id]] <- experiment
    x <- .rdml_set_prop_list(x, "experiment", experiments)
    
    # Ensure referenced sample exists and update target-aware sample type. --
    if (nonempty_string(sample_id)) {
      samples <- .rdml_prop_keyed(x, "sample")
      sample_obj <- samples[[sample_id]]
      if (is.null(sample_obj)) {
        sample_obj <- sampleType(id = idType(sample_id), type = list(), quantity = list())
      }
      
      if (nonempty_string(sample_type)) {
        st <- sampleTargetType(
          targetId = idReferenceType(target_id),
          sampleType = sampleTypeType(sample_type)
        )
        
        sample_types <- .rdml_prop_keyed(sample_obj, "type")
        sample_types[[target_id]] <- st
        sample_obj <- .rdml_set_prop_list(
          sample_obj,
          "type",
          sample_types
        )
      }
      
      # Quantities are also target-aware in the rdml7 schema.
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
          sample_obj <- .rdml_set_prop_list(
            sample_obj,
            "quantity",
            quantities
          )
        }
      }
      
      samples[[sample_id]] <- sample_obj
      x <- .rdml_set_prop_list(x, "sample", samples)
    }
    
    # Ensure target/dye references exist when description provides dye id. --
    targets <- .rdml_prop_keyed(x, "target")
    if (is.null(targets[[target_id]])) {
      if (nonempty_string(dye_id)) {
        targets[[target_id]] <- targetType(
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
        x <- .rdml_set_prop_list(x, "target", targets)
      } else {
        warning(
          "Target '", target_id,
          "' is absent and cannot be created because target.dyeId is missing",
          call. = FALSE
        )
      }
    }
    
    if (nonempty_string(dye_id)) {
      dyes <- .rdml_prop_keyed(x, "dye")
      if (is.null(dyes[[dye_id]])) {
        dyes[[dye_id]] <- dyeType(id = idType(dye_id))
        x <- .rdml_set_prop_list(x, "dye", dyes)
      }
    }
  }
  
  x
}
