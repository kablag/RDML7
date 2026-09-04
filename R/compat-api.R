NULL

# Legacy API compatibility ---------------------------------------------------
#
# This file deliberately preserves the previous public API without deprecation
# warnings. Old entry points keep old argument names and old dotted table
# columns. Canonical entry points use camelCase throughout.

.rdmlLegacyColumnMap <- c(
  "fdata.name" = "fdataName",
  "exp.id" = "expId",
  "run.id" = "runId",
  "react.id" = "reactId",
  "sample.type" = "sampleType",
  "target.dyeId" = "targetDyeId"
)

.rdmlRenameColumns <- function(x, mapping) {
  if (!is.data.frame(x)) {
    return(x)
  }

  out <- data.table::copy(
    data.table::as.data.table(x)
  )

  old <- intersect(
    names(mapping),
    names(out)
  )

  if (length(old)) {
    data.table::setnames(
      out,
      old = old,
      new = unname(mapping[old])
    )
  }

  out
}

.rdmlLegacyToCamelTable <- function(x) {
  .rdmlRenameColumns(
    x,
    .rdmlLegacyColumnMap
  )
}

.rdmlCamelToLegacyTable <- function(x) {
  .rdmlRenameColumns(
    x,
    stats::setNames(
      names(.rdmlLegacyColumnMap),
      unname(.rdmlLegacyColumnMap)
    )
  )
}


# Legacy AsTable ------------------------------------------------------------

.legacyAsTableCore <- function(
    x,
    defaultExpr,
    nameExpr,
    addExpr,
    dotsExpr,
    treatNullAsNa,
    includeHidden) {

  checkmate::assertFlag(treatNullAsNa)
  checkmate::assertFlag(includeHidden)

  dateMade <- x$dateMade
  dateUpdated <- x$dateUpdated
  id <- .rdmlPropList(x, "id")
  experimenter <- .rdmlPropKeyed(x, "experimenter")
  documentation <- .rdmlPropKeyed(x, "documentation")
  dye <- .rdmlPropKeyed(x, "dye")
  sample <- .rdmlPropKeyed(x, "sample")
  target <- .rdmlPropKeyed(x, "target")
  thermalCyclingConditions <- .rdmlPropKeyed(
    x,
    "thermalCyclingConditions"
  )
  experiments <- .rdmlPropList(
    x,
    "experiment"
  )

  rows <- list()
  rowI <- 0L

  for (experiment in experiments) {
    expId <- .rdmlIdChr(experiment$id)

    if (
      !includeHidden &&
      !is.na(expId) &&
      grepl("^\\.", expId)
    ) {
      next
    }

    for (run in .rdmlPropList(experiment, "run")) {
      for (react in .rdmlPropList(run, "react")) {
        for (data in .rdmlPropList(react, "data")) {
          rowI <- rowI + 1L

          fdataName <- eval(
            nameExpr,
            envir = environment()
          )

          defaultValues <- eval(
            defaultExpr,
            envir = environment()
          )

          addValues <- eval(
            addExpr,
            envir = environment()
          )

          dotsValues <- eval(
            dotsExpr,
            envir = environment()
          )

          if (!is.list(defaultValues)) {
            stop(
              "`.default` must evaluate to a list",
              call. = FALSE
            )
          }

          if (!is.list(addValues)) {
            stop(
              "`add.columns` must evaluate to a list",
              call. = FALSE
            )
          }

          result <- c(
            list(
              fdata.name = as.character(fdataName)
            ),
            defaultValues,
            addValues,
            dotsValues
          )

          if (
            is.null(names(result)) ||
            any(names(result) == "")
          ) {
            stop(
              "All AsTable columns must be named",
              call. = FALSE
            )
          }

          for (nm in names(result)) {
            if (is.null(result[[nm]])) {
              if (treatNullAsNa) {
                result[[nm]] <- NA
              }
              next
            }

            if (length(result[[nm]]) != 1L) {
              stop(
                "AsTable column '",
                nm,
                "' returned length ",
                length(result[[nm]]),
                "; every expression must return one value per data element",
                call. = FALSE
              )
            }
          }

          rows[[rowI]] <- result
        }
      }
    }
  }

  if (!length(rows)) {
    return(
      data.table::data.table(
        fdata.name = character()
      )
    )
  }

  out <- data.table::rbindlist(
    rows,
    fill = TRUE,
    use.names = TRUE
  )

  if (anyDuplicated(out$fdata.name)) {
    out[
      ,
      fdata.name := if (.N > 1L) {
        paste(
          fdata.name,
          seq_len(.N),
          sep = "_"
        )
      } else {
        fdata.name
      },
      by = fdata.name
    ]

    warning(
      "fdata.name column has duplicates; sequence numbers were added. ",
      "Consider a different `name.pattern`.",
      call. = FALSE
    )
  }

  data.table::setkey(
    out,
    "fdata.name"
  )

  out
}


#' @rdname legacy-api
#' @export
AsTable <- function(
    x,
    .default = list(
      exp.id = .rdmlIdChr(experiment$id),
      run.id = .rdmlIdChr(run$id),
      react.id = .rdmlIdChr(react$id),
      position = .rdmlReactPosition(
        react,
        run$pcrFormat
      ),
      sample = .rdmlIdChr(react$sample),
      target = .rdmlIdChr(data$targetId),
      target.dyeId = .rdmlTargetDye(
        target,
        .rdmlIdChr(data$targetId)
      ),
      sample.type = .rdmlSampleType(
        sample[[.rdmlIdChr(react$sample)]],
        .rdmlIdChr(data$targetId)
      ),
      adp = .rdmlPresent(data$adp),
      mdp = .rdmlPresent(data$mdp)
    ),
    name.pattern = paste(
      .rdmlIdChr(experiment$id),
      .rdmlIdChr(run$id),
      .rdmlReactPosition(
        react,
        run$pcrFormat
      ),
      .rdmlIdChr(react$sample),
      .rdmlSampleType(
        sample[[.rdmlIdChr(react$sample)]],
        .rdmlIdChr(data$targetId)
      ),
      .rdmlIdChr(data$targetId),
      sep = "_"
    ),
    add.columns = list(),
    treat.null.as.na = FALSE,
    include.hidden = FALSE,
    ...) {

  .legacyAsTableCore(
    x = x,
    defaultExpr = substitute(.default),
    nameExpr = substitute(name.pattern),
    addExpr = substitute(add.columns),
    dotsExpr = substitute(list(...)),
    treatNullAsNa = treat.null.as.na,
    includeHidden = include.hidden
  )
}


# Legacy fluorescence API ---------------------------------------------------

#' @rdname legacy-api
#' @export
GetFData <- function(
    x,
    request,
    dp.type = "adp",
    long.table = FALSE,
    ...) {

  if (S7::S7_inherits(x, dataType)) {
    return(
      getFData(
        x,
        dpType = dp.type,
        ...
      )
    )
  }

  if (!S7::S7_inherits(x, rdmlType)) {
    stop(
      "`x` must be dataType or rdmlType",
      call. = FALSE
    )
  }

  requestCamel <- if (missing(request)) {
    .rdmlLegacyToCamelTable(
      AsTable(x)
    )
  } else {
    .rdmlLegacyToCamelTable(
      request
    )
  }

  out <- getFData(
    x,
    request = requestCamel,
    dpType = dp.type,
    longTable = long.table,
    includeMissing = TRUE,
    ...
  )

  if (isTRUE(long.table)) {
    out <- .rdmlCamelToLegacyTable(out)
  }

  out
}


#' @rdname legacy-api
#' @export
SetFData <- function(
    x,
    fdata,
    description,
    fdata.type = "adp",
    conflict = c(
      "error",
      "keep",
      "replace"
    ),
    ...) {

  setFData(
    x,
    fdata,
    .rdmlLegacyToCamelTable(
      description
    ),
    fdataType = fdata.type,
    conflict = conflict,
    ...
  )
}


# Other legacy operations ---------------------------------------------------

#' @rdname legacy-api
#' @export
AsDendrogram <- function(
    x,
    plot.dendrogram = TRUE,
    ...) {

  asDendrogram(
    x,
    plotDendrogram = plot.dendrogram,
    ...
  )
}

#' @rdname legacy-api
#' @export
MergeRDMLs <- function(
    to.merge,
    data.conflict = c(
      "incoming",
      "base",
      "error"
    )) {

  mergeRDMLs(
    toMerge = to.merge,
    dataConflict = data.conflict
  )
}
