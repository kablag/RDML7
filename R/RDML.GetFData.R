#' Get fluorescence data from a dataType object
#'
#' @param x dataType object.
#' @param dp.type "adp" for amplification or "mdp" for melting data.
#' @return data.table containing the stored curve columns. Amplification data
#'   retain an optional `tmp` column when present in `dpAmpCurveType@fpoints`.
#' @export
#' @include generics.R rdml-utils.R
S7::method(GetFData, dataType) <- function(x, dp.type = "adp", ...) {
  checkmate::assertChoice(dp.type, c("adp", "mdp"))

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required", call. = FALSE)
  }

  curve <- if (identical(dp.type, "adp")) x$adp else x$mdp

  if (.rdml_is_missing(curve)) {
    if (identical(dp.type, "adp")) {
      return(data.table::data.table(cyc = numeric(), fluor = numeric()))
    }
    return(data.table::data.table(tmp = numeric(), fluor = numeric()))
  }

  if (identical(dp.type, "adp")) {
    if (!S7::S7_inherits(curve, dpAmpCurveType)) {
      stop("dataType@adp must be a dpAmpCurveType", call. = FALSE)
    }

    points <- data.table::copy(
      data.table::as.data.table(curve$fpoints)
    )

    required <- c("cyc", "fluor")
    missing_cols <- setdiff(required, names(points))
    if (length(missing_cols)) {
      stop(
        "dpAmpCurveType@fpoints is missing: ",
        paste(missing_cols, collapse = ", "),
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
  missing_cols <- setdiff(required, names(points))
  if (length(missing_cols)) {
    stop(
      "dpMeltingCurveType@fpoints is missing: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  points[, c("tmp", "fluor"), with = FALSE]
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
    request <- data.table::copy(
      data.table::as.data.table(request)
    )
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
      "fdata.name contains duplicates; generating names from ",
      "exp.id/run.id/react.id/target",
      call. = FALSE
    )

    generated <- paste(
      request[["exp.id"]],
      request[["run.id"]],
      request[["react.id"]],
      request[["target"]],
      sep = "_"
    )
    generated <- make.unique(as.character(generated), sep = "_")
    data.table::set(request, j = "fdata.name", value = generated)
  }

  long_parts <- vector("list", nrow(request))

  for (i in seq_len(nrow(request))) {
    exp_id <- as.character(request[["exp.id"]][[i]])
    run_id <- as.character(request[["run.id"]][[i]])
    react_id <- as.character(request[["react.id"]][[i]])
    target_id <- as.character(request[["target"]][[i]])

    experiment <- x$experiment[[exp_id]]
    if (is.null(experiment)) stop("Unknown experiment: ", exp_id, call. = FALSE)

    run <- experiment$run[[run_id]]
    if (is.null(run)) stop("Unknown run: ", run_id, call. = FALSE)

    react <- run$react[[react_id]]
    if (is.null(react)) stop("Unknown react: ", react_id, call. = FALSE)

    data_obj <- react$data[[target_id]]
    if (is.null(data_obj)) stop("Unknown target data: ", target_id, call. = FALSE)

    points <- GetFData(data_obj, dp.type = dp.type)
    if (nrow(points)) {
      data.table::set(
        points,
        j = "fdata.name",
        value = rep(as.character(request[["fdata.name"]][[i]]), nrow(points))
      )
    } else {
      points[, fdata.name := character()]
    }

    long_parts[[i]] <- points
  }

  coord <- if (identical(dp.type, "adp")) "cyc" else "tmp"
  out_long <- data.table::rbindlist(long_parts, use.names = TRUE, fill = TRUE)

  if (long.table) {
    # Always keep every requested curve in long output, including curves that
    # have metadata but no stored points.
    return(
      merge(
        request,
        out_long,
        by = "fdata.name",
        all.x = TRUE,
        sort = FALSE
      )
    )
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

  # If amplification temperatures are present and are unambiguous for each
  # cycle across all selected curves, retain one tmp column in wide output.
  if (identical(dp.type, "adp") && "tmp" %in% names(out_long)) {
    tmp_rows <- out_long[!is.na(tmp), .(
      n_tmp = data.table::uniqueN(tmp),
      tmp = tmp[[1L]]
    ), by = cyc]

    if (nrow(tmp_rows) && all(tmp_rows$n_tmp <= 1L)) {
      tmp_rows[, n_tmp := NULL]
      out <- merge(out, tmp_rows, by = "cyc", all.x = TRUE, sort = FALSE)
      data.table::setcolorder(out, c("cyc", "tmp", setdiff(names(out), c("cyc", "tmp"))))
    }
  }

  desired_curves <- intersect(as.character(request$fdata.name), names(out))
  prefix <- intersect(c(coord, "tmp"), names(out))
  data.table::setcolorder(out, c(prefix, desired_curves))
  out
}
