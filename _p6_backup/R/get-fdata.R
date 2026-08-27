#' Get fluorescence data from a dataType object
#'
#' @param x dataType object.
#' @param dpType "adp" for amplification or "mdp" for melting data.
#' @return data.table containing the stored curve columns. Amplification data
#'   retain an optional `tmp` column when present in `dpAmpCurveType@fpoints`.
#' @export
#' @include generics.R rdml-utils.R
S7::method(getFData, dataType) <- function(x, dpType = "adp", ...) {
  checkmate::assertChoice(dpType, c("adp", "mdp"))

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required", call. = FALSE)
  }

  curve <- if (identical(dpType, "adp")) x$adp else x$mdp

  if (.rdmlIsMissing(curve)) {
    if (identical(dpType, "adp")) {
      return(data.table::data.table(cyc = numeric(), fluor = numeric()))
    }
    return(data.table::data.table(tmp = numeric(), fluor = numeric()))
  }

  if (identical(dpType, "adp")) {
    if (!S7::S7_inherits(curve, dpAmpCurveType)) {
      stop("dataType@adp must be a dpAmpCurveType", call. = FALSE)
    }

    points <- data.table::copy(
      data.table::as.data.table(curve$fpoints)
    )

    required <- c("cyc", "fluor")
    missingCols <- setdiff(required, names(points))
    if (length(missingCols)) {
      stop(
        "dpAmpCurveType@fpoints is missing: ",
        paste(missingCols, collapse = ", "),
        call. = FALSE
      )
    }

    keep <- intersect(c("cyc", "tmp", "fluor"), names(points))
    return(points[, keep, with = FALSE])
  }

  if (!S7::S7_inherits(curve, dpMeltingCurveType)) {
    stop("dataType@mdp must be a dpMeltingCurveType", call. = FALSE)
  }

  points <- data.table::copy(
    data.table::as.data.table(curve$fpoints)
  )

  required <- c("tmp", "fluor")
  missingCols <- setdiff(required, names(points))
  if (length(missingCols)) {
    stop(
      "dpMeltingCurveType@fpoints is missing: ",
      paste(missingCols, collapse = ", "),
      call. = FALSE
    )
  }

  points[, c("tmp", "fluor"), with = FALSE]
}


#' Get fluorescence data from rdmlType
#'
#' @param x rdmlType object.
#' @param request Output from asTable(x). If omitted, asTable(x) is used.
#' @param dpType "adp" or "mdp".
#' @param longTable If TRUE, return long form joined to request metadata.
#' @param includeMissing If FALSE (default), rows without the selected `dpType`
#'   curve are omitted. If TRUE, keep requested metadata rows even when the
#'   selected curve is absent, with coordinate/fluorescence values as NA.
#' @return data.table.
#' @export
S7::method(getFData, rdmlType) <- function(
    x,
    request,
    dpType = "adp",
    longTable = FALSE,
    includeMissing = FALSE,
    ...) {

  checkmate::assertChoice(dpType, c("adp", "mdp"))
  checkmate::assertFlag(longTable)
  checkmate::assertFlag(includeMissing)

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required", call. = FALSE)
  }

  if (missing(request)) {
    request <- asTable(x)
  } else {
    request <- data.table::copy(
      data.table::as.data.table(request)
    )
  }

  required <- c("fdataName", "expId", "runId", "reactId", "target")
  missingCols <- setdiff(required, names(request))
  if (length(missingCols)) {
    stop(
      "request is missing required columns: ",
      paste(missingCols, collapse = ", "),
      call. = FALSE
    )
  }

  # Canonical API returns only curves that actually exist for the requested
  # data-point type. asTable() provides logical `adp` / `mdp` indicators, so
  # use them early when available. A custom request that omits these columns is
  # still supported; empty curves are removed later by the inner join.
  if (
    !includeMissing &&
    dpType %in% names(request)
  ) {
    keep <- !is.na(request[[dpType]]) &
      as.logical(request[[dpType]])

    request <- request[
      keep
    ]
  }

  if (anyDuplicated(request$fdataName)) {
    warning(
      "fdataName contains duplicates; generating names from ",
      "expId/runId/reactId/target",
      call. = FALSE
    )

    generated <- paste(
      request[["expId"]],
      request[["runId"]],
      request[["reactId"]],
      request[["target"]],
      sep = "_"
    )
    generated <- make.unique(as.character(generated), sep = "_")
    data.table::set(request, j = "fdataName", value = generated)
  }

  longParts <- vector("list", nrow(request))

  for (i in seq_len(nrow(request))) {
    expId <- as.character(request[["expId"]][[i]])
    runId <- as.character(request[["runId"]][[i]])
    reactId <- as.character(request[["reactId"]][[i]])
    targetId <- as.character(request[["target"]][[i]])

    experiment <- x$experiment[[expId]]
    if (is.null(experiment)) stop("Unknown experiment: ", expId, call. = FALSE)

    run <- experiment$run[[runId]]
    if (is.null(run)) stop("Unknown run: ", runId, call. = FALSE)

    react <- run$react[[reactId]]
    if (is.null(react)) stop("Unknown react: ", reactId, call. = FALSE)

    dataObj <- react$data[[targetId]]
    if (is.null(dataObj)) stop("Unknown target data: ", targetId, call. = FALSE)

    points <- getFData(dataObj, dpType = dpType)
    if (nrow(points)) {
      data.table::set(
        points,
        j = "fdataName",
        value = rep(as.character(request[["fdataName"]][[i]]), nrow(points))
      )
    } else {
      points[, fdataName := character()]
    }

    longParts[[i]] <- points
  }

  coord <- if (identical(dpType, "adp")) "cyc" else "tmp"
  outLong <- data.table::rbindlist(longParts, use.names = TRUE, fill = TRUE)

  if (longTable) {
    # Default canonical behaviour is an inner join: only metadata belonging to
    # an existing curve of the selected dpType is returned. `includeMissing`
    # restores the previous left-join behaviour when explicitly requested.
    return(
      merge(
        request,
        outLong,
        by = "fdataName",
        all.x = includeMissing,
        sort = FALSE
      )
    )
  }

  if (!nrow(outLong)) {
    out <- data.table::data.table(numeric())
    data.table::setnames(out, coord)
    return(out)
  }

  out <- data.table::dcast(
    outLong,
    stats::as.formula(sprintf("%s ~ fdataName", coord)),
    value.var = "fluor"
  )

  # If amplification temperatures are present and are unambiguous for each
  # cycle across all selected curves, retain one tmp column in wide output.
  if (identical(dpType, "adp") && "tmp" %in% names(outLong)) {
    tmpRows <- outLong[!is.na(tmp), .(
      nTmp = data.table::uniqueN(tmp),
      tmp = tmp[[1L]]
    ), by = cyc]

    if (nrow(tmpRows) && all(tmpRows$nTmp <= 1L)) {
      tmpRows[, nTmp := NULL]
      out <- merge(out, tmpRows, by = "cyc", all.x = TRUE, sort = FALSE)
      data.table::setcolorder(out, c("cyc", "tmp", setdiff(names(out), c("cyc", "tmp"))))
    }
  }

  desiredCurves <- intersect(as.character(request$fdataName), names(out))
  prefix <- intersect(c(coord, "tmp"), names(out))
  data.table::setcolorder(out, c(prefix, desiredCurves))
  out
}
