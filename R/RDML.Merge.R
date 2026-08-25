#' Merge rdmlType objects
#'
#' The first object is the base. Duplicate top-level metadata keys are kept from
#' the base object. Experiments/runs with the same id are merged; reacts with the
#' same id are overwritten by later objects, matching the intent of the original
#' R6 implementation.
#'
#' @param to.merge List of rdmlType objects.
#' @return rdmlType.
#' @export
#' @include rdml-utils.R
MergeRDMLs <- function(to.merge) {
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
  
  # S7 objects are ordinary R value objects: no R6 deep clone is needed.
  baseRDML <- to.merge[[1L]]
  if (length(to.merge) == 1L) return(baseRDML)
  
  # Merge unkeyed rdml id list by publisher (base wins duplicates).
  merge_rdml_ids <- function(base, incoming) {
    base <- .rdml_as_list(base)
    incoming <- .rdml_as_list(incoming)
    existing <- vapply(
      base,
      function(z) as.character(z$publisher),
      character(1)
    )
    for (obj in incoming) {
      key <- as.character(obj$publisher)
      if (!key %in% existing) {
        base[[length(base) + 1L]] <- obj
        existing <- c(existing, key)
      }
    }
    names(base) <- NULL
    base
  }
  
  merge_top_keyed <- function(base_obj, incoming_obj, property) {
    base_list <- .rdml_prop_keyed(base_obj, property)
    incoming_list <- .rdml_prop_keyed(incoming_obj, property)
    if (!length(incoming_list)) return(base_obj)
    
    for (key in names(incoming_list)) {
      if (is.null(base_list[[key]])) {
        base_list[[key]] <- incoming_list[[key]]
      }
    }
    S7::prop(base_obj, property) <- S7::S7_data(base_list)
    base_obj
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
        next
      }
      
      base_runs <- .rdml_prop_keyed(base_exp, "run")
      incoming_runs <- .rdml_prop_keyed(incoming_exp, "run")
      
      for (run_id in names(incoming_runs)) {
        incoming_run <- incoming_runs[[run_id]]
        base_run <- base_runs[[run_id]]
        
        if (is.null(base_run)) {
          base_runs[[run_id]] <- incoming_run
          next
        }
        
        base_reacts <- .rdml_prop_keyed(base_run, "react")
        incoming_reacts <- .rdml_prop_keyed(incoming_run, "react")
        
        # Later RDML objects overwrite reacts with the same key.
        for (react_id in names(incoming_reacts)) {
          base_reacts[[react_id]] <- incoming_reacts[[react_id]]
        }
        
        base_run <- .rdml_set_prop_list(
          base_run,
          "react",
          base_reacts
        )
        base_runs[[run_id]] <- base_run
      }
      
      base_exp <- .rdml_set_prop_list(
        base_exp,
        "run",
        base_runs
      )
      base_experiments[[exp_id]] <- base_exp
    }
    
    baseRDML <- .rdml_set_prop_list(
      baseRDML,
      "experiment",
      base_experiments
    )
  }
  
  baseRDML
}
