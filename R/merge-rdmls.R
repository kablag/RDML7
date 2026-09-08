#' Merge multiple RDML objects
#'
#' Merge two or more [rdmlType] objects recursively.
#'
#' The **first** RDML object is used as the metadata base. Every subsequent
#' object is merged into the accumulated result from left to right:
#'
#' `mergeRDMLs(rdml1, rdml2, rdml3)` is therefore equivalent to
#' `merge(merge(rdml1, rdml2), rdml3)`.
#'
#' The function accepts either RDML objects as separate arguments:
#'
#' `mergeRDMLs(rdml1, rdml2, rdml3)`
#'
#' or a single list containing RDML objects:
#'
#' `mergeRDMLs(list(rdml1, rdml2, rdml3))`.
#'
#' Top-level keyed metadata (`experimenter`, `documentation`, `dye`, `sample`,
#' `target`, and `thermalCyclingConditions`) keep the definition from the
#' accumulated base object when the same key is present in both objects.
#'
#' `dataConflict` applies to duplicate target data within the same reaction:
#'
#' - `"incoming"`: replace base data with data from the current incoming
#'   object. With multiple objects, later objects therefore have higher
#'   priority.
#' - `"base"`: preserve data already present in the accumulated base object.
#'   With multiple objects, earlier objects therefore have higher priority.
#' - `"error"`: stop at the first duplicate data target.
#'
#' @param ... Two or more `rdmlType` objects, or a single non-empty list of
#'   `rdmlType` objects.
#' @param dataConflict Conflict policy for duplicate target data:
#'   `"incoming"`, `"base"`, or `"error"`.
#'
#' @return Merged `rdmlType`.
#'
#' @examples
#' \dontrun{
#' file1 <- "experiment1.rdml"
#' file2 <- "experiment2.rdml"
#'
#' base <- readRDML(file1)
#' incoming <- readRDML(file2)
#'
#' # Separate arguments
#' merged <- mergeRDMLs(
#'   base,
#'   incoming,
#'   dataConflict = "error"
#' )
#'
#' # Several objects
#' merged <- mergeRDMLs(
#'   rdml1,
#'   rdml2,
#'   rdml3,
#'   dataConflict = "incoming"
#' )
#'
#' # A list is also accepted
#' merged <- mergeRDMLs(
#'   list(rdml1, rdml2, rdml3),
#'   dataConflict = "base"
#' )
#' }
#'
#' @seealso [readRDML()], [writeRDML()]
#' @export
mergeRDMLs <- function(
    ...,
    dataConflict = c(
      "incoming",
      "base",
      "error"
    )) {
  
  dataConflict <- match.arg(
    dataConflict
  )
  
  toMerge <- list(...)
  
  # Support both:
  #   mergeRDMLs(rdml1, rdml2, rdml3)
  # and:
  #   mergeRDMLs(list(rdml1, rdml2, rdml3))
  if (
    length(toMerge) == 1L &&
    is.list(toMerge[[1L]]) &&
    !S7::S7_inherits(
      toMerge[[1L]],
      rdmlType
    )
  ) {
    toMerge <- toMerge[[1L]]
  }
  
  if (!length(toMerge)) {
    stop(
      "`mergeRDMLs()` requires at least one rdmlType object",
      call. = FALSE
    )
  }
  
  ok <- vapply(
    toMerge,
    function(x) {
      S7::S7_inherits(
        x,
        rdmlType
      )
    },
    logical(1)
  )
  
  if (!all(ok)) {
    bad <- which(!ok)
    
    stop(
      "Every object supplied to `mergeRDMLs()` must be rdmlType. ",
      "Invalid element",
      if (length(bad) > 1L) "s" else "",
      ": ",
      paste(
        bad,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  baseRDML <- toMerge[[1L]]
  
  if (length(toMerge) == 1L) {
    return(baseRDML)
  }
  
  mergeRDMLIds <- function(
    base,
    incoming) {
    
    base <- .rdmlAsList(base)
    incoming <- .rdmlAsList(incoming)
    
    publisherKey <- function(z) {
      publisher <- S7::prop(
        z,
        "publisher"
      )
      
      if (
        length(publisher) == 1L &&
        !is.na(publisher)
      ) {
        publisher
      } else {
        ""
      }
    }
    
    existing <- vapply(
      base,
      publisherKey,
      character(1)
    )
    
    for (obj in incoming) {
      key <- publisherKey(obj)
      
      if (!(key %in% existing)) {
        base[[length(base) + 1L]] <- obj
        existing <- c(
          existing,
          key
        )
      }
    }
    
    names(base) <- NULL
    
    base
  }
  
  # Top-level metadata intentionally keeps the base definition when the same
  # key exists in both objects. Structural experiment data use deeper rules.
  mergeTopKeyed <- function(
    baseObj,
    incomingObj,
    property) {
    
    baseList <- .rdmlPropKeyed(
      baseObj,
      property
    )
    incomingList <- .rdmlPropKeyed(
      incomingObj,
      property
    )
    
    for (key in names(incomingList)) {
      if (is.null(baseList[[key]])) {
        baseList[[key]] <- incomingList[[key]]
      }
    }
    
    .rdmlSetPropList(
      baseObj,
      property,
      baseList
    )
  }
  
  mergeData <- function(
    baseReact,
    incomingReact,
    path) {
    
    baseData <- .rdmlPropKeyed(
      baseReact,
      "data"
    )
    incomingData <- .rdmlPropKeyed(
      incomingReact,
      "data"
    )
    
    for (targetId in names(incomingData)) {
      if (is.null(baseData[[targetId]])) {
        baseData[[targetId]] <- incomingData[[targetId]]
        next
      }
      
      if (identical(
        dataConflict,
        "incoming"
      )) {
        baseData[[targetId]] <- incomingData[[targetId]]
      } else if (identical(
        dataConflict,
        "error"
      )) {
        stop(
          "Duplicate data target '",
          targetId,
          "' at ",
          path,
          call. = FALSE
        )
      }
      
      # dataConflict == "base":
      # keep the value already present in baseData.
    }
    
    .rdmlSetPropList(
      baseReact,
      "data",
      baseData
    )
  }
  
  mergeReact <- function(
    baseReact,
    incomingReact,
    path) {
    
    baseSample <- .rdmlIdChr(
      baseReact$sample
    )
    incomingSample <- .rdmlIdChr(
      incomingReact$sample
    )
    
    if (
      !is.na(baseSample) &&
      !is.na(incomingSample) &&
      !identical(
        baseSample,
        incomingSample
      )
    ) {
      stop(
        "Cannot merge react at ",
        path,
        ": sample references differ ('",
        baseSample,
        "' vs '",
        incomingSample,
        "')",
        call. = FALSE
      )
    }
    
    baseReact <- mergeData(
      baseReact,
      incomingReact,
      path
    )
    
    # Partitions are not keyed in the current react schema.
    # Preserve base partitions when present; otherwise use incoming ones.
    basePartitions <- .rdmlPropList(
      baseReact,
      "partitions"
    )
    incomingPartitions <- .rdmlPropList(
      incomingReact,
      "partitions"
    )
    
    if (
      !length(basePartitions) &&
      length(incomingPartitions)
    ) {
      baseReact <- .rdmlSetPropList(
        baseReact,
        "partitions",
        incomingPartitions
      )
    }
    
    baseReact
  }
  
  mergeRun <- function(
    baseRun,
    incomingRun,
    expId,
    runId) {
    
    baseReacts <- .rdmlPropKeyed(
      baseRun,
      "react"
    )
    incomingReacts <- .rdmlPropKeyed(
      incomingRun,
      "react"
    )
    
    for (reactId in names(incomingReacts)) {
      incomingReact <- incomingReacts[[reactId]]
      baseReact <- baseReacts[[reactId]]
      
      if (is.null(baseReact)) {
        baseReacts[[reactId]] <- incomingReact
      } else {
        path <- paste0(
          "experiment '",
          expId,
          "'/run '",
          runId,
          "'/react '",
          reactId,
          "'"
        )
        
        baseReacts[[reactId]] <- mergeReact(
          baseReact,
          incomingReact,
          path
        )
      }
    }
    
    .rdmlSetPropList(
      baseRun,
      "react",
      baseReacts
    )
  }
  
  mergeExperiment <- function(
    baseExp,
    incomingExp,
    expId) {
    
    baseRuns <- .rdmlPropKeyed(
      baseExp,
      "run"
    )
    incomingRuns <- .rdmlPropKeyed(
      incomingExp,
      "run"
    )
    
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
    
    .rdmlSetPropList(
      baseExp,
      "run",
      baseRuns
    )
  }
  
  # Left-to-right merge:
  #
  #   base = rdml1
  #   incoming = rdml2
  #   base = merge(base, incoming)
  #
  #   incoming = rdml3
  #   base = merge(base, incoming)
  #
  # etc.
  for (incomingRDML in toMerge[-1L]) {
    S7::prop(
      baseRDML,
      "id"
    ) <- mergeRDMLIds(
      S7::prop(
        baseRDML,
        "id"
      ),
      S7::prop(
        incomingRDML,
        "id"
      )
    )
    
    for (property in c(
      "experimenter",
      "documentation",
      "dye",
      "sample",
      "target",
      "thermalCyclingConditions"
    )) {
      baseRDML <- mergeTopKeyed(
        baseRDML,
        incomingRDML,
        property
      )
    }
    
    baseExperiments <- .rdmlPropKeyed(
      baseRDML,
      "experiment"
    )
    incomingExperiments <- .rdmlPropKeyed(
      incomingRDML,
      "experiment"
    )
    
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
