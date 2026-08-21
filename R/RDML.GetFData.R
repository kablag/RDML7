#' Get fluorescence data from a dataType object
#'
#' @param x dataType object.
#' @param dp.type "adp" for amplification or "mdp" for melting data.
#' @return data.table with cyc/tmp and fluor columns.
#' @export
#' @include functional_wrappers.R RDML.R
S7::method(GetFData, dataType) <- function(x, dp.type = "adp", ...) {
  checkmate::assertChoice(dp.type, c("adp", "mdp"))
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required", call. = FALSE)
  }

  curve <- if (identical(dp.type, "adp")) x$adp else x$mdp
  if (.rdml_is_missing(curve)) {
    coord <- if (identical(dp.type, "adp")) "cyc" else "tmp"
    out <- data.table::data.table(numeric(), numeric())
    data.table::setnames(out, c(coord, "fluor"))
    return(out)
  }

  if (identical(dp.type, "adp")) {
    if (!S7::S7_inherits(curve, dpAmpCurveType)) {
      stop("dataType@adp must be a dpAmpCurveType", call. = FALSE)
    }
    return(data.table::as.data.table(curve$fpoints)[, c("cyc", "fluor"), with = FALSE])
  }

  if (!S7::S7_inherits(curve, dpMeltingCurveType)) {
    stop("dataType@mdp must be a dpMeltingCurveType", call. = FALSE)
  }
  data.table::as.data.table(curve$fpoints)[, c("tmp", "fluor"), with = FALSE]
}

#' Get fluorescence data from rdmlType
#'
#' @param x rdmlType object.
#' @param request Output from AsTable(x). If omitted, AsTable(x) is used.
#' @param dp.type "adp" or "mdp".
#' @param long.table If TRUE, return long form joined to request metadata.
#' @return data.table.
#' @export
S7::method(GetFData, rdmlType) <- function(
    x,
    request,
    dp.type = "adp",
    long.table = FALSE,
    ...) {

  checkmate::assertChoice(dp.type, c("adp", "mdp"))
  checkmate::assertFlag(long.table)
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required", call. = FALSE)
  }

  if (missing(request)) {
    request <- AsTable(x)
  } else {
    request <- data.table::as.data.table(request)
  }

  required <- c("fdata.name", "exp.id", "run.id", "react.id", "target")
  missing_cols <- setdiff(required, names(request))
  if (length(missing_cols)) {
    stop(
      "request is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (anyDuplicated(request$fdata.name)) {
    warning(
      "fdata.name contains duplicates; generating names from exp.id/run.id/react.id/target",
      call. = FALSE
    )
    request[, fdata.name := paste(exp.id, run.id, react.id, target, sep = "_")]
    if (anyDuplicated(request$fdata.name)) {
      request[, fdata.name := make.unique(as.character(fdata.name), sep = "_")]
    }
  }

  long_parts <- vector("list", nrow(request))

  for (i in seq_len(nrow(request))) {
    row <- request[i]
    exp_id <- as.character(row[["exp.id"]])
    run_id <- as.character(row[["run.id"]])
    react_id <- as.character(row[["react.id"]])
    target_id <- as.character(row[["target"]])

    experiment <- x$experiment[[exp_id]]
    if (is.null(experiment)) stop("Unknown experiment: ", exp_id, call. = FALSE)

    run <- experiment$run[[run_id]]
    if (is.null(run)) stop("Unknown run: ", run_id, call. = FALSE)

    react <- run$react[[react_id]]
    if (is.null(react)) stop("Unknown react: ", react_id, call. = FALSE)

    data_obj <- react$data[[target_id]]
    if (is.null(data_obj)) stop("Unknown target data: ", target_id, call. = FALSE)

    points <- GetFData(data_obj, dp.type = dp.type)
    if (!nrow(points)) {
      # Keep an empty component; it will simply not contribute points.
      points[, fdata.name := character()]
    } else {
      points[, fdata.name := as.character(row[["fdata.name"]])]
    }
    long_parts[[i]] <- points
  }

  coord <- if (identical(dp.type, "adp")) "cyc" else "tmp"
  out_long <- data.table::rbindlist(long_parts, use.names = TRUE, fill = TRUE)

  if (long.table) {
    if (!nrow(out_long)) {
      return(merge(request, out_long, by = "fdata.name", all.x = TRUE, sort = FALSE))
    }
    return(merge(request, out_long, by = "fdata.name", sort = FALSE))
  }

  if (!nrow(out_long)) {
    out <- data.table::data.table(numeric())
    data.table::setnames(out, coord)
    return(out)
  }

  out <- data.table::dcast(
    out_long,
    stats::as.formula(sprintf("%s ~ fdata.name", coord)),
    value.var = "fluor"
  )

  desired <- c(coord, intersect(as.character(request$fdata.name), names(out)))
  data.table::setcolorder(out, desired)
  out
}
