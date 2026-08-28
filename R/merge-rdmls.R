#' Merge multiple RDML objects
#'
#' The first object is the metadata base. Experiments, runs, reactions, and
#' target data are merged recursively by schema key.
#'
#' @param toMerge Non-empty list of `rdmlType` objects.
#' @param dataConflict Conflict policy: `"incoming"`, `"base"`, or `"error"`.
#' @return Merged `rdmlType`.
#' @seealso `rdmlRead`, `rdmlWrite`
#' @export
mergeRdmls <- function(
    toMerge,
    dataConflict = c("incoming", "base", "error")) {

  dataConflict <- match.arg(dataConflict)

  if (!is.list(toMerge) || !length(toMerge)) {
    stop("`toMerge` must be a non-empty list of rdmlType objects", call. = FALSE)
  }

  ok <- vapply(
    toMerge,
    function(x) S7::S7_inherits(x, rdmlType),
    logical(1)
  )
  if (!all(ok)) {
    stop("Every element of `toMerge` must be rdmlType", call. = FALSE)
  }

  baseRDML <- toMerge[[1L]]
  if (length(toMerge) == 1L) return(baseRDML)

  mergeRdmlIds <- function(base, incoming) {
    base <- .rdmlAsList(base)
    incoming <- .rdmlAsList(incoming)

    publisherKey <- function(z) {
      publisher <- S7::prop(z, "publisher")
      if (length(publisher) == 1L && !is.na(publisher)) publisher else ""
    }

    existing <- vapply(base, publisherKey, character(1))

    for (obj in incoming) {
      key <- publisherKey(obj)
      if (!(key %in% existing)) {
        base[[length(base) + 1L]] <- obj
        existing <- c(existing, key)
      }
    }

    names(base) <- NULL
    base
  }

  # Top-level metadata intentionally keeps the base definition when the same
  # key exists in both objects. Structural experiment data use deeper rules.
  mergeTopKeyed <- function(baseObj, incomingObj, property) {
    baseList <- .rdmlPropKeyed(baseObj, property)
    incomingList <- .rdmlPropKeyed(incomingObj, property)

    for (key in names(incomingList)) {
      if (is.null(baseList[[key]])) {
        baseList[[key]] <- incomingList[[key]]
      }
    }

    .rdmlSetPropList(baseObj, property, baseList)
  }

  mergeData <- function(baseReact, incomingReact, path) {
    baseData <- .rdmlPropKeyed(baseReact, "data")
    incomingData <- .rdmlPropKeyed(incomingReact, "data")

    for (targetId in names(incomingData)) {
      if (is.null(baseData[[targetId]])) {
        baseData[[targetId]] <- incomingData[[targetId]]
        next
      }

      if (identical(dataConflict, "incoming")) {
        baseData[[targetId]] <- incomingData[[targetId]]
      } else if (identical(dataConflict, "error")) {
        stop(
          "Duplicate data target '", targetId,
          "' at ", path,
          call. = FALSE
        )
      }
    }

    .rdmlSetPropList(baseReact, "data", baseData)
  }

  mergeReact <- function(baseReact, incomingReact, path) {
    baseSample <- .rdmlIdChr(baseReact$sample)
    incomingSample <- .rdmlIdChr(incomingReact$sample)

    if (
      !is.na(baseSample) &&
      !is.na(incomingSample) &&
      !identical(baseSample, incomingSample)
    ) {
      stop(
        "Cannot merge react at ", path,
        ": sample references differ ('", baseSample,
        "' vs '", incomingSample, "')",
        call. = FALSE
      )
    }

    baseReact <- mergeData(baseReact, incomingReact, path)

    # Partitions are not keyed in the current react schema. Preserve base data
    # when present; otherwise take incoming partitions.
    basePartitions <- .rdmlPropList(baseReact, "partitions")
    incomingPartitions <- .rdmlPropList(incomingReact, "partitions")
    if (!length(basePartitions) && length(incomingPartitions)) {
      baseReact <- .rdmlSetPropList(
        baseReact,
        "partitions",
        incomingPartitions
      )
    }

    baseReact
  }

  mergeRun <- function(baseRun, incomingRun, expId, runId) {
    baseReacts <- .rdmlPropKeyed(baseRun, "react")
    incomingReacts <- .rdmlPropKeyed(incomingRun, "react")

    for (reactId in names(incomingReacts)) {
      incomingReact <- incomingReacts[[reactId]]
      baseReact <- baseReacts[[reactId]]

      if (is.null(baseReact)) {
        baseReacts[[reactId]] <- incomingReact
      } else {
        path <- paste0(
          "experiment '", expId,
          "'/run '", runId,
          "'/react '", reactId, "'"
        )
        baseReacts[[reactId]] <- mergeReact(
          baseReact,
          incomingReact,
          path
        )
      }
    }

    .rdmlSetPropList(baseRun, "react", baseReacts)
  }

  mergeExperiment <- function(baseExp, incomingExp, expId) {
    baseRuns <- .rdmlPropKeyed(baseExp, "run")
    incomingRuns <- .rdmlPropKeyed(incomingExp, "run")

    for (runId in names(incomingRuns)) {
      incomingRun <- incomingRuns[[runId]]
      baseRun <- baseRuns[[runId]]

      if (is.null(baseRun)) {
        baseRuns[[runId]] <- incomingRun
      } else {
        baseRuns[[runId]] <- mergeRun(
          baseRun,
          incomingRun,
          expId,
          runId
        )
      }
    }

    .rdmlSetPropList(baseExp, "run", baseRuns)
  }

  for (rdml in toMerge[-1L]) {
    S7::prop(baseRDML, "id") <- mergeRdmlIds(
      S7::prop(baseRDML, "id"),
      S7::prop(rdml, "id")
    )

    for (property in c(
      "experimenter",
      "documentation",
      "dye",
      "sample",
      "target",
      "thermalCyclingConditions"
    )) {
      baseRDML <- mergeTopKeyed(baseRDML, rdml, property)
    }

    baseExperiments <- .rdmlPropKeyed(baseRDML, "experiment")
    incomingExperiments <- .rdmlPropKeyed(rdml, "experiment")

    for (expId in names(incomingExperiments)) {
      incomingExp <- incomingExperiments[[expId]]
      baseExp <- baseExperiments[[expId]]

      if (is.null(baseExp)) {
        baseExperiments[[expId]] <- incomingExp
      } else {
        baseExperiments[[expId]] <- mergeExperiment(
          baseExp,
          incomingExp,
          expId
        )
      }
    }

    baseRDML <- .rdmlSetPropList(
      baseRDML,
      "experiment",
      baseExperiments
    )
  }

  baseRDML
}
