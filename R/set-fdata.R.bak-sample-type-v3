#' Add or replace fluorescence data
#'
#' Missing experiments, runs, reactions, samples, targets, and dyes are
#' created as needed. RDML uses value semantics, so assign the returned object.
#'
#' @param x `rdmlType`.
#' @param fdata Matrix/data.frame/data.table. First column is `cyc` or `tmp`;
#' remaining fluorescence columns are matched to `description$fdataName`.
#' @param description CamelCase metadata table. Required: `fdataName`, `expId`,
#' `runId`, `reactId`, and `target`; sample/target metadata are used when new
#' schema objects are created.
#' @param fdataType `"adp"` or `"mdp"`.
#' @param conflict `"error"`, `"keep"`, or `"replace"`.
#' @param ... Reserved for extensions.
#' @return Modified `rdmlType`.
#' @rdname setFData
#' @export
S7::method(setFData, rdmlType) <- function(
    x,
    fdata,
    description,
    fdataType = "adp",
    conflict = c("error", "keep", "replace"),
    ...) {

  checkmate::assertChoice(fdataType, c("adp", "mdp"))
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

  required <- c("fdataName", "expId", "runId", "reactId", "target")
  missingCols <- setdiff(required, names(description))
  if (length(missingCols)) {
    stop(
      "description is missing required columns: ",
      paste(missingCols, collapse = ", "),
      call. = FALSE
    )
  }

  coordinate <- as.numeric(fdata[[1L]])
  describedNames <- as.character(description[["fdataName"]])

  # Optional per-cycle amplification temperature. It is metadata, not a
  # fluorescence series, unless description explicitly names a series "tmp".
  adpTmp <- NULL
  fdataNames <- names(fdata)[-1L]
  if (
    identical(fdataType, "adp") &&
    "tmp" %in% fdataNames &&
    !("tmp" %in% describedNames)
  ) {
    adpTmp <- as.numeric(fdata[["tmp"]])
    fdataNames <- setdiff(fdataNames, "tmp")
  }

  value1 <- function(row, name, default = NA) {
    if (!name %in% names(row)) return(default)
    value <- row[[name]]
    if (!length(value)) default else value[[1L]]
  }

  nonemptyString <- function(value) {
    length(value) == 1L && !is.na(value) && nzchar(as.character(value))
  }

  makePcrFormat <- function(expId, runId) {
    idx <- description[["expId"]] == expId & description[["runId"]] == runId
    reactIds <- suppressWarnings(base::as.integer(description[["reactId"]][idx]))

    maxReact <- if (length(reactIds) && any(!is.na(reactIds))) {
      max(reactIds, na.rm = TRUE)
    } else {
      96L
    }

    if (maxReact > 96L) {
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

  handleConflict <- function(message) {
    if (identical(conflict, "error")) stop(message, call. = FALSE)
    invisible(NULL)
  }

  # Pull top-level collections once. Updating these wrappers in memory avoids
  # rebuilding x for every fluorescence series.
  experiments <- .rdmlPropKeyed(x, "experiment")
  samples <- .rdmlPropKeyed(x, "sample")
  targets <- .rdmlPropKeyed(x, "target")
  dyes <- .rdmlPropKeyed(x, "dye")

  for (fdataName in fdataNames) {
    idx <- which(describedNames == fdataName)

    if (!length(idx)) {
      warning(
        "No description for fluorescence column: ", fdataName,
        call. = FALSE
      )
      next
    }
    if (length(idx) > 1L) {
      stop(
        "Multiple description rows for fluorescence column: ", fdataName,
        call. = FALSE
      )
    }

    row <- description[idx]
    expId <- as.character(value1(row, "expId"))
    runId <- as.character(value1(row, "runId"))
    reactId <- as.character(value1(row, "reactId"))
    targetId <- as.character(value1(row, "target"))
    sampleId <- as.character(value1(row, "sample"))
    sampleType <- as.character(value1(row, "sampleType"))
    dyeId <- as.character(value1(row, "targetDyeId"))
    targetTypeValue <- as.character(value1(row, "targetType"))

    if (!all(vapply(
      list(expId, runId, reactId, targetId),
      nonemptyString,
      logical(1)
    ))) {
      stop(
        "expId, runId, reactId and target must be non-empty for ",
        fdataName,
        call. = FALSE
      )
    }

    # experiment ----------------------------------------------------------
    experiment <- experiments[[expId]]
    if (is.null(experiment)) {
      experiment <- experimentType(id = idType(expId), run = list())
    }

    # run -----------------------------------------------------------------
    runs <- .rdmlPropKeyed(experiment, "run")
    run <- runs[[runId]]
    if (is.null(run)) {
      run <- runType(
        id = idType(runId),
        pcrFormat = makePcrFormat(expId, runId),
        react = list()
      )
    }

    # react ---------------------------------------------------------------
    reacts <- .rdmlPropKeyed(run, "react")
    react <- reacts[[reactId]]

    if (is.null(react)) {
      if (!nonemptyString(sampleId)) {
        stop(
          "Cannot create react '", reactId,
          "' without a `sample` value in description",
          call. = FALSE
        )
      }

      react <- reactType(
        id = idType(reactId),
        sample = idReferenceType(sampleId),
        data = list(),
        partitions = list()
      )
    } else if (nonemptyString(sampleId)) {
      oldSample <- .rdmlIdChr(react$sample)
      if (!is.na(oldSample) && !identical(oldSample, sampleId)) {
        handleConflict(sprintf(
          "React '%s' already references sample '%s', not '%s'",
          reactId, oldSample, sampleId
        ))
        if (identical(conflict, "replace")) {
          S7::prop(react, "sample") <- idReferenceType(sampleId)
        }
      }
    }

    # dataType keyed by targetId -----------------------------------------
    dataList <- .rdmlPropKeyed(react, "data")
    dataObj <- dataList[[targetId]]

    if (is.null(dataObj)) {
      dataObj <- dataType(
        targetId = idReferenceType(targetId),
        cq = NA_real_,
        N0 = NA_real_,
        ampEffMet = NA_character_,
        ampEff = NA_real_,
        ampEffSE = NA_real_,
        corrF = NA_real_,
        corrP = NA_real_,
        meltTemp = NA_real_,
        meltTemps = NA_real_,
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
        S7::prop(dataObj, "cq") <- as.numeric(cq)
      }
    }


    if ("meltTemp" %in% names(row)) {
      meltTemp <- value1(row, "meltTemp", NA_real_)
      if (length(meltTemp) == 1L && !is.na(meltTemp)) {
        S7::prop(dataObj, "meltTemp") <- as.numeric(meltTemp)
      }
    }

    if ("meltTemps" %in% names(row)) {
      meltTemps <- value1(row, "meltTemps", NA_real_)
      meltTemps <- suppressWarnings(as.numeric(meltTemps))
      meltTemps <- meltTemps[!is.na(meltTemps)]

      if (length(meltTemps)) {
        S7::prop(dataObj, "meltTemps") <- meltTemps

        if (is.na(dataObj$meltTemp)) {
          S7::prop(dataObj, "meltTemp") <- meltTemps[[1L]]
        }
      }
    }

    fluor <- as.numeric(fdata[[fdataName]])

    if (identical(fdataType, "adp")) {
      fpoints <- if (is.null(adpTmp)) {
        data.table::data.table(cyc = coordinate, fluor = fluor)
      } else {
        data.table::data.table(cyc = coordinate, tmp = adpTmp, fluor = fluor)
      }
      S7::prop(dataObj, "adp") <- dpAmpCurveType(fpoints = fpoints)
    } else {
      S7::prop(dataObj, "mdp") <- dpMeltingCurveType(
        fpoints = data.table::data.table(tmp = coordinate, fluor = fluor)
      )
    }

    dataList <- .rdmlUpsertListByKey(
      .rdmlPropList(
        react,
        "data"
      ),
      dataObj,
      key = "targetId"
    )
    
    react <- .rdmlSetPropList(
      react,
      "data",
      dataList
    )
    
    
    reactList <- .rdmlUpsertListByKey(
      .rdmlPropList(
        run,
        "react"
      ),
      react,
      key = "id"
    )
    
    run <- .rdmlSetPropList(
      run,
      "react",
      reactList
    )
    
    
    runList <- .rdmlUpsertListByKey(
      .rdmlPropList(
        experiment,
        "run"
      ),
      run,
      key = "id"
    )
    
    experiment <- .rdmlSetPropList(
      experiment,
      "run",
      runList
    )
    
    
    experimentList <- .rdmlUpsertListByKey(
      .rdmlAsList(
        experiments
      ),
      experiment,
      key = "id"
    )
    
    experiments <- rdmlKeyedList(
      experimentList,
      key = "id"
    )

    # sample --------------------------------------------------------------
    if (nonemptyString(sampleId)) {
      sampleObj <- samples[[sampleId]]
      if (is.null(sampleObj)) {
        sampleObj <- sampleType(
          id = idType(sampleId),
          type = list(),
          quantity = list()
        )
      }

      if (nonemptyString(sampleType)) {
        st <- sampleTargetType(
          targetId = idReferenceType(targetId),
          sampleType = sampleTypeType(sampleType)
        )
        sampleTypes <- .rdmlPropKeyed(sampleObj, "type")
        sampleTypes[[targetId]] <- st
        sampleObj <- .rdmlSetPropList(sampleObj, "type", sampleTypes)
      }

      if ("quantity" %in% names(row)) {
        quantity <- value1(row, "quantity", NA_real_)
        if (length(quantity) == 1L && !is.na(quantity)) {
          q <- quantityType(
            targetId = idReferenceType(targetId),
            value = as.numeric(quantity),
            unit = quantityUnitType("other")
          )
          quantities <- .rdmlPropKeyed(sampleObj, "quantity")
          quantities[[targetId]] <- q
          sampleObj <- .rdmlSetPropList(sampleObj, "quantity", quantities)
        }
      }

      samples[[sampleId]] <- sampleObj
    }

    # target/dye ----------------------------------------------------------
    targetObj <- targets[[targetId]]
    if (is.null(targetObj)) {
      if (nonemptyString(dyeId)) {
        targetObj <- targetType(
          id = idType(targetId),
          description = NA_character_,
          documentation = list(),
          xRef = list(),
          type = targetTypeType(
            if (nonemptyString(targetTypeValue)) targetTypeValue else "toi"
          ),
          amplificationEfficiencyMethod = NA_character_,
          amplificationEfficiency = NA_real_,
          amplificationEfficiencySE = NA_real_,
          meltingTemperature = NA_real_,
          detectionLimit = NA_real_,
          dyeId = idReferenceType(dyeId),
          sequences = NA,
          commercialAssay = NA
        )
        targets[[targetId]] <- targetObj
      } else {
        warning(
          "Target '", targetId,
          "' is absent and cannot be created because targetDyeId is missing",
          call. = FALSE
        )
      }
    } else {
      if (nonemptyString(targetTypeValue)) {
        oldTargetType <- .rdmlEnumChr(targetObj$type)
        if (!is.na(oldTargetType) && !identical(oldTargetType, targetTypeValue)) {
          handleConflict(sprintf(
            "Target '%s' already has type '%s', not '%s'",
            targetId, oldTargetType, targetTypeValue
          ))
          if (identical(conflict, "replace")) {
            S7::prop(targetObj, "type") <- targetTypeType(targetTypeValue)
          }
        }
      }

      if (nonemptyString(dyeId)) {
      oldDye <- .rdmlIdChr(targetObj$dyeId)
      if (!is.na(oldDye) && !identical(oldDye, dyeId)) {
        handleConflict(sprintf(
          "Target '%s' already references dye '%s', not '%s'",
          targetId, oldDye, dyeId
        ))
        if (identical(conflict, "replace")) {
          S7::prop(targetObj, "dyeId") <- idReferenceType(dyeId)
        }
      }
      }

      targets[[targetId]] <- targetObj
    }

    if (nonemptyString(dyeId) && is.null(dyes[[dyeId]])) {
      dyes[[dyeId]] <- dyeType(id = idType(dyeId))
    }
  }

  x <- .rdmlSetPropList(x, "experiment", experiments)
  x <- .rdmlSetPropList(x, "sample", samples)
  x <- .rdmlSetPropList(x, "target", targets)
  x <- .rdmlSetPropList(x, "dye", dyes)
  x
}
