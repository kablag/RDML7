#' Merge rdmlType objects
#'
#' The first object is the metadata base. Top-level keyed metadata absent from
#' the base are appended from later objects. Experiments, runs, reacts, and
#' react data are merged recursively by their schema keys, so adding a second
#' target to an existing reaction no longer replaces the first target.
#'
#' @param to.merge List of rdmlType objects.
#' @param data.conflict How to resolve the same targetId occurring in the same
#'   experiment/run/react in more than one object: `incoming`, `base`, or
#'   `error`.
#' @return rdmlType.
#' @export
#' @include rdml-utils.R
MergeRDMLs <- function(
    to.merge,
    data.conflict = c("incoming", "base", "error")) {

  data.conflict <- match.arg(data.conflict)

  if (!is.list(to.merge) || !length(to.merge)) {
    stop("`to.merge` must be a non-empty list of rdmlType objects", call. = FALSE)
  }

  ok <- vapply(
    to.merge,
    function(x) S7::S7_inherits(x, rdmlType),
    logical(1)
  )
  if (!all(ok)) {
    stop("Every element of `to.merge` must be rdmlType", call. = FALSE)
  }

  baseRDML <- to.merge[[1L]]
  if (length(to.merge) == 1L) return(baseRDML)

  merge_rdml_ids <- function(base, incoming) {
    base <- .rdml_as_list(base)
    incoming <- .rdml_as_list(incoming)

    publisher_key <- function(z) {
      publisher <- S7::prop(z, "publisher")
      if (length(publisher) == 1L && !is.na(publisher)) publisher else ""
    }

    existing <- vapply(base, publisher_key, character(1))

    for (obj in incoming) {
      key <- publisher_key(obj)
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
  merge_top_keyed <- function(base_obj, incoming_obj, property) {
    base_list <- .rdml_prop_keyed(base_obj, property)
    incoming_list <- .rdml_prop_keyed(incoming_obj, property)

    for (key in names(incoming_list)) {
      if (is.null(base_list[[key]])) {
        base_list[[key]] <- incoming_list[[key]]
      }
    }

    .rdml_set_prop_list(base_obj, property, base_list)
  }

  merge_data <- function(base_react, incoming_react, path) {
    base_data <- .rdml_prop_keyed(base_react, "data")
    incoming_data <- .rdml_prop_keyed(incoming_react, "data")

    for (target_id in names(incoming_data)) {
      if (is.null(base_data[[target_id]])) {
        base_data[[target_id]] <- incoming_data[[target_id]]
        next
      }

      if (identical(data.conflict, "incoming")) {
        base_data[[target_id]] <- incoming_data[[target_id]]
      } else if (identical(data.conflict, "error")) {
        stop(
          "Duplicate data target '", target_id,
          "' at ", path,
          call. = FALSE
        )
      }
    }

    .rdml_set_prop_list(base_react, "data", base_data)
  }

  merge_react <- function(base_react, incoming_react, path) {
    base_sample <- .rdml_id_chr(base_react$sample)
    incoming_sample <- .rdml_id_chr(incoming_react$sample)

    if (
      !is.na(base_sample) &&
      !is.na(incoming_sample) &&
      !identical(base_sample, incoming_sample)
    ) {
      stop(
        "Cannot merge react at ", path,
        ": sample references differ ('", base_sample,
        "' vs '", incoming_sample, "')",
        call. = FALSE
      )
    }

    base_react <- merge_data(base_react, incoming_react, path)

    # Partitions are not keyed in the current react schema. Preserve base data
    # when present; otherwise take incoming partitions.
    base_partitions <- .rdml_prop_list(base_react, "partitions")
    incoming_partitions <- .rdml_prop_list(incoming_react, "partitions")
    if (!length(base_partitions) && length(incoming_partitions)) {
      base_react <- .rdml_set_prop_list(
        base_react,
        "partitions",
        incoming_partitions
      )
    }

    base_react
  }

  merge_run <- function(base_run, incoming_run, exp_id, run_id) {
    base_reacts <- .rdml_prop_keyed(base_run, "react")
    incoming_reacts <- .rdml_prop_keyed(incoming_run, "react")

    for (react_id in names(incoming_reacts)) {
      incoming_react <- incoming_reacts[[react_id]]
      base_react <- base_reacts[[react_id]]

      if (is.null(base_react)) {
        base_reacts[[react_id]] <- incoming_react
      } else {
        path <- paste0(
          "experiment '", exp_id,
          "'/run '", run_id,
          "'/react '", react_id, "'"
        )
        base_reacts[[react_id]] <- merge_react(
          base_react,
          incoming_react,
          path
        )
      }
    }

    .rdml_set_prop_list(base_run, "react", base_reacts)
  }

  merge_experiment <- function(base_exp, incoming_exp, exp_id) {
    base_runs <- .rdml_prop_keyed(base_exp, "run")
    incoming_runs <- .rdml_prop_keyed(incoming_exp, "run")

    for (run_id in names(incoming_runs)) {
      incoming_run <- incoming_runs[[run_id]]
      base_run <- base_runs[[run_id]]

      if (is.null(base_run)) {
        base_runs[[run_id]] <- incoming_run
      } else {
        base_runs[[run_id]] <- merge_run(
          base_run,
          incoming_run,
          exp_id,
          run_id
        )
      }
    }

    .rdml_set_prop_list(base_exp, "run", base_runs)
  }

  for (rdml in to.merge[-1L]) {
    S7::prop(baseRDML, "id") <- merge_rdml_ids(
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
      baseRDML <- merge_top_keyed(baseRDML, rdml, property)
    }

    base_experiments <- .rdml_prop_keyed(baseRDML, "experiment")
    incoming_experiments <- .rdml_prop_keyed(rdml, "experiment")

    for (exp_id in names(incoming_experiments)) {
      incoming_exp <- incoming_experiments[[exp_id]]
      base_exp <- base_experiments[[exp_id]]

      if (is.null(base_exp)) {
        base_experiments[[exp_id]] <- incoming_exp
      } else {
        base_experiments[[exp_id]] <- merge_experiment(
          base_exp,
          incoming_exp,
          exp_id
        )
      }
    }

    baseRDML <- .rdml_set_prop_list(
      baseRDML,
      "experiment",
      base_experiments
    )
  }

  baseRDML
}
