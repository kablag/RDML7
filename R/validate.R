NULL

# Semantic RDML validation --------------------------------------------------

.rdmlValidationIssue <- function(
    severity,
    path,
    code,
    message) {

  data.table::data.table(
    severity = severity,
    path = path,
    code = code,
    message = message
  )
}


.rdmlValidationEmpty <- function() {
  data.table::data.table(
    severity = character(),
    path = character(),
    code = character(),
    message = character()
  )
}


.rdmlDuplicateKeyIssues <- function(
    values,
    key = "id",
    path,
    label) {

  values <- .rdmlAsList(values)
  if (!length(values)) return(.rdmlValidationEmpty())

  keys <- .getKeys(values, key)
  badMissing <- which(is.na(keys) | !nzchar(keys))
  duplicate <- unique(keys[duplicated(keys) & !is.na(keys)])
  issues <- list()

  if (length(badMissing)) {
    issues[[length(issues) + 1L]] <- .rdmlValidationIssue(
      "error",
      path,
      "missingKey",
      paste0(label, " contains element(s) without a usable ", key)
    )
  }

  if (length(duplicate)) {
    issues[[length(issues) + 1L]] <- .rdmlValidationIssue(
      "error",
      path,
      "duplicateKey",
      paste0(
        label,
        " contains duplicate key(s): ",
        paste(duplicate, collapse = ", ")
      )
    )
  }

  if (!length(issues)) return(.rdmlValidationEmpty())
  data.table::rbindlist(issues)
}


.rdmlReferenceIssue <- function(
    id,
    validIds,
    path,
    code,
    label) {

  if (
    length(id) != 1L ||
    is.na(id) ||
    !nzchar(id)
  ) {
    return(.rdmlValidationIssue(
      "error",
      path,
      code,
      paste0(label, " reference is missing")
    ))
  }

  if (!id %in% validIds) {
    return(.rdmlValidationIssue(
      "error",
      path,
      code,
      paste0(label, " references unknown id '", id, "'")
    ))
  }

  .rdmlValidationEmpty()
}


#' Semantically validate an RDML object
#'
#' Checks duplicate keys, dangling references, and fluorescence-curve
#' consistency in addition to S7 property validation.
#'
#' @param x `rdmlType`.
#' @param level `"structure"`, `"references"`, `"curves"`, or `"full"`.
#' @param action `"return"`, `"warn"`, or `"error"`.
#' @return A `data.table` of class `rdmlValidation` describing issues.
#' @seealso `rdmlIsValid`, `rdmlSummary`
#' @export
validateRDML <- function(
    x,
    level = c("full", "structure", "references", "data"),
    action = c("return", "warn", "error")) {

  level <- match.arg(level)
  action <- match.arg(action)

  if (!S7::S7_inherits(x, rdmlType)) {
    .rdmlAbort(
      code = "invalidObject",
      message = "`x` must be an rdmlType object"
    )
  }

  doStructure <- level %in% c("full", "structure")
  doReferences <- level %in% c("full", "references")
  doData <- level %in% c("full", "data")

  issues <- list()
  add <- function(value) {
    if (nrow(value)) {
      issues[[length(issues) + 1L]] <<- value
    }
  }

  master <- list(
    experimenter = .rdmlPropList(x, "experimenter"),
    documentation = .rdmlPropList(x, "documentation"),
    dye = .rdmlPropList(x, "dye"),
    sample = .rdmlPropList(x, "sample"),
    target = .rdmlPropList(x, "target"),
    thermalCyclingConditions = .rdmlPropList(x, "thermalCyclingConditions"),
    experiment = .rdmlPropList(x, "experiment")
  )

  ids <- lapply(master, .getKeys)

  if (doStructure) {
    for (name in names(master)) {
      add(.rdmlDuplicateKeyIssues(
        master[[name]],
        path = name,
        label = name
      ))
    }
  }

  if (doReferences) {
    for (sampleObj in master$sample) {
      sampleId <- .rdmlIdChr(sampleObj$id)
      samplePath <- paste0("sample.", sampleId)

      for (entry in .rdmlPropList(sampleObj, "type")) {
        add(.rdmlReferenceIssue(
          .rdmlIdChr(entry$targetId),
          ids$target,
          paste0(samplePath, ".type"),
          "unknownTargetReference",
          "Sample type"
        ))
      }

      for (entry in .rdmlPropList(sampleObj, "quantity")) {
        add(.rdmlReferenceIssue(
          .rdmlIdChr(entry$targetId),
          ids$target,
          paste0(samplePath, ".quantity"),
          "unknownTargetReference",
          "Sample quantity"
        ))
      }
    }

    for (targetObj in master$target) {
      targetId <- .rdmlIdChr(targetObj$id)
      dyeId <- .rdmlIdChr(targetObj$dyeId)
      add(.rdmlReferenceIssue(
        dyeId,
        ids$dye,
        paste0("target.", targetId, ".dyeId"),
        "unknownDyeReference",
        "Target dye"
      ))
    }
  }

  for (experiment in master$experiment) {
    expId <- .rdmlIdChr(experiment$id)
    runs <- .rdmlPropList(experiment, "run")

    if (doStructure) {
      add(.rdmlDuplicateKeyIssues(
        runs,
        path = paste0("experiment.", expId, ".run"),
        label = "run"
      ))
    }

    for (run in runs) {
      runId <- .rdmlIdChr(run$id)
      runPath <- paste0("experiment.", expId, ".run.", runId)

      if (doReferences) {
        for (ref in .rdmlPropList(run, "documentation")) {
          add(.rdmlReferenceIssue(
            .rdmlIdChr(ref),
            ids$documentation,
            paste0(runPath, ".documentation"),
            "unknownDocumentationReference",
            "Run documentation"
          ))
        }

        for (ref in .rdmlPropList(run, "experimenter")) {
          add(.rdmlReferenceIssue(
            .rdmlIdChr(ref),
            ids$experimenter,
            paste0(runPath, ".experimenter"),
            "unknownExperimenterReference",
            "Run experimenter"
          ))
        }

        if (.rdmlPresent(run$thermalCyclingConditions)) {
          add(.rdmlReferenceIssue(
            .rdmlIdChr(run$thermalCyclingConditions),
            ids$thermalCyclingConditions,
            paste0(runPath, ".thermalCyclingConditions"),
            "unknownThermalCyclingReference",
            "Run thermalCyclingConditions"
          ))
        }
      }

      reacts <- .rdmlPropList(run, "react")

      if (doStructure) {
        add(.rdmlDuplicateKeyIssues(
          reacts,
          path = paste0(runPath, ".react"),
          label = "react"
        ))
      }

      for (react in reacts) {
        reactId <- .rdmlIdChr(react$id)
        reactPath <- paste0(runPath, ".react.", reactId)

        if (doReferences) {
          add(.rdmlReferenceIssue(
            .rdmlIdChr(react$sample),
            ids$sample,
            paste0(reactPath, ".sample"),
            "unknownSampleReference",
            "Reaction sample"
          ))
        }

        dataList <- .rdmlPropList(react, "data")

        if (doStructure) {
          add(.rdmlDuplicateKeyIssues(
            dataList,
            key = "targetId",
            path = paste0(reactPath, ".data"),
            label = "data"
          ))
        }

        for (dataObj in dataList) {
          targetId <- .rdmlIdChr(dataObj$targetId)
          dataPath <- paste0(reactPath, ".data.", targetId)

          if (doReferences) {
            add(.rdmlReferenceIssue(
              targetId,
              ids$target,
              paste0(dataPath, ".targetId"),
              "unknownTargetReference",
              "Data target"
            ))
          }

          if (doData) {
            if (.rdmlPresent(dataObj$meltTemps)) {
              multi <- as.numeric(dataObj$meltTemps)

              if (
                .rdmlPresent(dataObj$meltTemp) &&
                length(multi) &&
                !isTRUE(all.equal(
                  as.numeric(dataObj$meltTemp),
                  multi[[1L]]
                ))
              ) {
                add(.rdmlValidationIssue(
                  "warning",
                  dataPath,
                  "meltTempMismatch",
                  "meltTemp differs from the first value in meltTemps"
                ))
              }
            }

            if (.rdmlPresent(dataObj$adp)) {
              points <- dataObj$adp$fpoints
              missingColumns <- setdiff(c("cyc", "fluor"), names(points))
              if (length(missingColumns)) {
                add(.rdmlValidationIssue(
                  "error",
                  paste0(dataPath, ".adp"),
                  "invalidAmplificationCurve",
                  paste0("Missing column(s): ", paste(missingColumns, collapse = ", "))
                ))
              } else if (anyDuplicated(points$cyc)) {
                add(.rdmlValidationIssue(
                  "warning",
                  paste0(dataPath, ".adp"),
                  "duplicateCycle",
                  "Amplification curve contains duplicate cycle coordinates"
                ))
              }
            }

            if (.rdmlPresent(dataObj$mdp)) {
              points <- dataObj$mdp$fpoints
              missingColumns <- setdiff(c("tmp", "fluor"), names(points))
              if (length(missingColumns)) {
                add(.rdmlValidationIssue(
                  "error",
                  paste0(dataPath, ".mdp"),
                  "invalidMeltingCurve",
                  paste0("Missing column(s): ", paste(missingColumns, collapse = ", "))
                ))
              } else if (anyDuplicated(points$tmp)) {
                add(.rdmlValidationIssue(
                  "warning",
                  paste0(dataPath, ".mdp"),
                  "duplicateTemperature",
                  "Melting curve contains duplicate temperature coordinates"
                ))
              }
            }
          }
        }
      }
    }
  }

  out <- if (length(issues)) {
    data.table::rbindlist(issues, use.names = TRUE, fill = TRUE)
  } else {
    .rdmlValidationEmpty()
  }

  class(out) <- c("rdmlValidation", class(out))

  errorCount <- sum(out$severity == "error")
  warningCount <- sum(out$severity == "warning")

  if (identical(action, "warn") && nrow(out)) {
    .rdmlWarn(
      code = "validation",
      message = sprintf(
        "RDML validation: %d error(s), %d warning(s)",
        errorCount,
        warningCount
      ),
      issues = out
    )
  }

  if (identical(action, "error") && errorCount > 0L) {
    .rdmlAbort(
      code = "validation",
      message = sprintf(
        "RDML validation failed with %d error(s)",
        errorCount
      ),
      issues = out
    )
  }

  out
}


#' Test whether an RDML object has semantic errors
#'
#' @param x `rdmlType`.
#' @param level Validation level passed to `validateRDML()`.
#' @return `TRUE` if no validation issue has severity `"error"`.
#' @seealso `validateRDML`
#' @export
rdmlIsValid <- function(x, level = "full") {
  result <- validateRDML(
    x,
    level = level,
    action = "return"
  )

  !any(result$severity == "error")
}


#' @export
print.rdmlValidation <- function(x, ...) {
  errors <- sum(x$severity == "error")
  warnings <- sum(x$severity == "warning")

  cat(sprintf(
    "<rdmlValidation> %d error(s), %d warning(s)\n",
    errors,
    warnings
  ))

  if (nrow(x)) {
    print(
      as.data.frame(x),
      row.names = FALSE
    )
  }

  invisible(x)
}
