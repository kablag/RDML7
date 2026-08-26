#' Represent fields of rdmlType as a data.table
#'
#' Collections are read through rdmlKeyedList virtual keys; no physical list
#' names are required. Expressions in `default`, `namePattern`,
#' `addColumns`, and `...` are evaluated once per dataType element in the
#' historical asTable evaluation environment.
#'
#' @param x rdmlType object.
#' @param default Named list of expressions evaluated for each dataType object.
#' @param namePattern Expression used to generate fdataName.
#' @param addColumns Named list of extra expressions.
#' @param treatNullAsNa Convert NULL results to NA.
#' @param includeHidden Include experiments whose id starts with `.`.
#' @param ... Additional named expressions.
#' @return data.table.
#' @export
#' @include generics.R rdml-utils.R
S7::method(asTable, rdmlType) <- function(
    x,
    default = list(
      expId = .rdmlIdChr(experiment$id),
      runId = .rdmlIdChr(run$id),
      reactId = .rdmlIdChr(react$id),
      position = .rdmlReactPosition(react, run$pcrFormat),
      sample = .rdmlIdChr(react$sample),
      target = .rdmlIdChr(data$targetId),
      targetDyeId = .rdmlTargetDye(target, .rdmlIdChr(data$targetId)),
      sampleType = .rdmlSampleType(
        sample[[.rdmlIdChr(react$sample)]],
        .rdmlIdChr(data$targetId)
      ),
      adp = .rdmlPresent(data$adp),
      mdp = .rdmlPresent(data$mdp)
    ),
    namePattern = paste(
      .rdmlIdChr(experiment$id),
      .rdmlIdChr(run$id),
      .rdmlReactPosition(react, run$pcrFormat),
      .rdmlIdChr(react$sample),
      .rdmlSampleType(
        sample[[.rdmlIdChr(react$sample)]],
        .rdmlIdChr(data$targetId)
      ),
      .rdmlIdChr(data$targetId),
      sep = "_"
    ),
    addColumns = list(),
    treatNullAsNa = FALSE,
    includeHidden = FALSE,
    ...) {

  checkmate::assertFlag(treatNullAsNa)
  checkmate::assertFlag(includeHidden)

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required", call. = FALSE)
  }

  # Capture the NSE expressions once rather than rebuilding them in every
  # nested-loop iteration.
  nameExpr <- substitute(namePattern)
  defaultExpr <- substitute(default)
  addExpr <- substitute(addColumns)
  dotsExpr <- substitute(list(...))

  # Historical evaluation environment names. User expressions may refer to
  # these objects directly.
  dateMade <- x$dateMade
  dateUpdated <- x$dateUpdated
  id <- .rdmlPropList(x, "id")
  experimenter <- .rdmlPropKeyed(x, "experimenter")
  documentation <- .rdmlPropKeyed(x, "documentation")
  dye <- .rdmlPropKeyed(x, "dye")
  sample <- .rdmlPropKeyed(x, "sample")
  target <- .rdmlPropKeyed(x, "target")
  thermalCyclingConditions <- .rdmlPropKeyed(x, "thermalCyclingConditions")

  experiments <- .rdmlPropList(x, "experiment")
  rows <- list()
  rowI <- 0L

  for (experiment in experiments) {
    expId <- .rdmlIdChr(experiment$id)
    if (!includeHidden && !is.na(expId) && grepl("^\\.", expId)) next

    for (run in .rdmlPropList(experiment, "run")) {
      for (react in .rdmlPropList(run, "react")) {
        for (data in .rdmlPropList(react, "data")) {
          rowI <- rowI + 1L

          fdataName <- eval(nameExpr, envir = environment())
          defaultValues <- eval(defaultExpr, envir = environment())
          addValues <- eval(addExpr, envir = environment())
          dotsValues <- eval(dotsExpr, envir = environment())

          if (!is.list(defaultValues)) {
            stop("`default` must evaluate to a list", call. = FALSE)
          }
          if (!is.list(addValues)) {
            stop("`addColumns` must evaluate to a list", call. = FALSE)
          }

          result <- c(
            list(fdataName = as.character(fdataName)),
            defaultValues,
            addValues,
            dotsValues
          )

          if (is.null(names(result)) || any(names(result) == "")) {
            stop("All asTable columns must be named", call. = FALSE)
          }

          for (nm in names(result)) {
            if (is.null(result[[nm]])) {
              if (treatNullAsNa) result[[nm]] <- NA
              next
            }
            if (length(result[[nm]]) != 1L) {
              stop(
                "asTable column '", nm,
                "' returned length ", length(result[[nm]]),
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
    return(data.table::data.table(fdataName = character()))
  }

  out <- data.table::rbindlist(rows, fill = TRUE, use.names = TRUE)

  if (anyDuplicated(out$fdataName)) {
    # Preserve historical behaviour, but the default name now includes
    # experiment and run ids so normal multi-experiment data are unique.
    out[, fdataName := if (.N > 1L) {
      paste(fdataName, seq_len(.N), sep = "_")
    } else {
      fdataName
    }, by = fdataName]

    warning(
      "fdataName column has duplicates; sequence numbers were added. ",
      "Consider a different `namePattern`.",
      call. = FALSE
    )
  }

  data.table::setkey(out, "fdataName")
  out
}
