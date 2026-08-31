#' Extract fluorescence from one data element
#'
#' @param x `dataType`.
#' @param dpType `"adp"` or `"mdp"`.
#' @param ... Reserved for method compatibility.
#' @return Amplification output contains `cyc`, optional `tmp`, and `fluor`;
#' melting output contains `tmp` and `fluor`. Missing curves return an empty
#' table with the appropriate columns.
#' @rdname getFData
#' @export
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


#' Extract fluorescence from an RDML object
#'
#' @param x `rdmlType`.
#' @param request Table produced by `asTable()`. If omitted, `asTable(x)` is
#' used.
#' @param dpType `"adp"` or `"mdp"`.
#' @param longTable Return long form joined to request metadata instead of
#' wide form.
#' @param includeMissing Keep metadata rows whose selected fluorescence
#'   data type is absent.
#' @param ... Reserved for method compatibility.
#' @return A `data.table`. Long output preserves every requested metadata row;
#' if the selected curve is absent, coordinate/fluorescence values are `NA`.
#' @rdname getFData
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
    # Always keep every requested curve in long output, including curves that
    # have metadata but no stored points.
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
