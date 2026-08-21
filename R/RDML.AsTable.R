#' Represent fields of rdmlType as a data.table
#'
#' rdml7 version of the original RDML$AsTable() method. Collections are read
#' through rdmlKeyedList virtual keys; no physical list names are required.
#'
#' @param x rdmlType object.
#' @param .default Named list of expressions evaluated for each dataType object.
#' @param name.pattern Expression used to generate fdata.name.
#' @param add.columns Named list of extra expressions.
#' @param treat.null.as.na Convert NULL results to NA.
#' @param ... Additional named expressions.
#' @return data.table.
#' @export
#' @include functional_wrappers.R RDML.R
S7::method(AsTable, rdmlType) <- function(
    x,
    .default = list(
      exp.id = .rdml_id_chr(experiment$id),
      run.id = .rdml_id_chr(run$id),
      react.id = .rdml_id_chr(react$id),
      position = .rdml_react_position(react, run$pcrFormat),
      sample = .rdml_id_chr(react$sample),
      target = .rdml_id_chr(data$targetId),
      target.dyeId = .rdml_target_dye(target, .rdml_id_chr(data$targetId)),
      sample.type = .rdml_sample_type(
        sample[[.rdml_id_chr(react$sample)]],
        .rdml_id_chr(data$targetId)
      ),
      adp = .rdml_present(data$adp),
      mdp = .rdml_present(data$mdp)
    ),
    name.pattern = paste(
      .rdml_react_position(react, run$pcrFormat),
      .rdml_id_chr(react$sample),
      .rdml_sample_type(
        sample[[.rdml_id_chr(react$sample)]],
        .rdml_id_chr(data$targetId)
      ),
      .rdml_id_chr(data$targetId),
      sep = "_"
    ),
    add.columns = list(),
    treat.null.as.na = FALSE,
    ...) {

  checkmate::assertFlag(treat.null.as.na)
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required", call. = FALSE)
  }

  # Preserve the names used by the historical NSE interface. User expressions
  # in .default/add.columns/... can refer to these objects directly.
  dateMade <- x$dateMade
  dateUpdated <- x$dateUpadted
  id <- .rdml_prop_list(x, "id")
  experimenter <- .rdml_prop_keyed(x, "experimenter")
  documentation <- .rdml_prop_keyed(x, "documentation")
  dye <- .rdml_prop_keyed(x, "dye")
  sample <- .rdml_prop_keyed(x, "sample")
  target <- .rdml_prop_keyed(x, "target")
  thermalCyclingConditions <- .rdml_prop_keyed(x, "thermalCyclingConditions")

  experiments <- .rdml_prop_list(x, "experiment")
  rows <- list()
  row_i <- 0L

  for (experiment in experiments) {
    exp_id <- .rdml_id_chr(experiment$id)
    if (!is.na(exp_id) && grepl("^\\.", exp_id)) next

    for (run in .rdml_prop_list(experiment, "run")) {
      for (react in .rdml_prop_list(run, "react")) {
        for (data in .rdml_prop_list(react, "data")) {
          row_i <- row_i + 1L

          fdata_name <- eval(substitute(name.pattern), envir = environment())
          default_values <- eval(substitute(.default), envir = environment())
          add_values <- eval(substitute(add.columns), envir = environment())
          dots_values <- eval(substitute(list(...)), envir = environment())

          if (!is.list(default_values)) {
            stop("`.default` must evaluate to a list", call. = FALSE)
          }
          if (!is.list(add_values)) {
            stop("`add.columns` must evaluate to a list", call. = FALSE)
          }

          result <- c(
            list(fdata.name = as.character(fdata_name)),
            default_values,
            add_values,
            dots_values
          )

          if (is.null(names(result)) || any(names(result) == "")) {
            stop("All AsTable columns must be named", call. = FALSE)
          }

          for (nm in names(result)) {
            if (is.null(result[[nm]])) {
              if (treat.null.as.na) result[[nm]] <- NA
              next
            }
            if (length(result[[nm]]) != 1L) {
              stop(
                "AsTable column '", nm,
                "' returned length ", length(result[[nm]]),
                "; every expression must return one value per data element",
                call. = FALSE
              )
            }
          }

          rows[[row_i]] <- result
        }
      }
    }
  }

  if (!length(rows)) {
    return(data.table::data.table(fdata.name = character()))
  }

  out <- data.table::rbindlist(rows, fill = TRUE, use.names = TRUE)

  if (anyDuplicated(out$fdata.name)) {
    # Match the old behaviour: add a sequence within each duplicate-name group.
    out[, fdata.name := if (.N > 1L) paste(fdata.name, seq_len(.N), sep = "_") else fdata.name,
        by = fdata.name]
    warning(
      "fdata.name column has duplicates; sequence numbers were added. ",
      "Consider a different `name.pattern`.",
      call. = FALSE
    )
  }

  data.table::setkey(out, "fdata.name")
  out
}
