# as-table.R ---------------------------------------------------------------
# Public asTable() API:
# - namePattern supports documented {field} templates.
# - columns selects built-in output columns.
# - named expressions in ... add custom columns.
# - legacy default/addColumns remain supported for compatibility.

.rdmlAsTableDefaultColumns <- c(
  "expId",
  "runId",
  "reactId",
  "position",
  "sample",
  "target",
  "targetDyeId",
  "sampleType",
  "adp",
  "mdp"
)

.rdmlAsTableBuiltins <- function(
    experiment,
    run,
    react,
    data,
    samples,
    targets) {
  
  expId <- .rdmlIdChr(
    S7::prop(experiment, "id")
  )
  runId <- .rdmlIdChr(
    S7::prop(run, "id")
  )
  reactId <- .rdmlIdChr(
    S7::prop(react, "id")
  )
  sampleId <- .rdmlIdChr(
    S7::prop(react, "sample")
  )
  targetId <- .rdmlIdChr(
    S7::prop(data, "targetId")
  )
  
  pcrFormat <- S7::prop(
    run,
    "pcrFormat"
  )
  
  sampleObject <- if (
    is.na(sampleId) ||
    !nzchar(sampleId)
  ) {
    NULL
  } else {
    samples[[sampleId]]
  }
  
  list(
    expId = expId,
    runId = runId,
    reactId = reactId,
    position = .rdmlReactPosition(
      react,
      pcrFormat
    ),
    sample = sampleId,
    target = targetId,
    targetDyeId = .rdmlTargetDye(
      targets,
      targetId
    ),
    sampleType = .rdmlSampleType(
      sampleObject,
      targetId
    ),
    adp = .rdmlPresent(
      S7::prop(data, "adp")
    ),
    mdp = .rdmlPresent(
      S7::prop(data, "mdp")
    )
  )
}


.rdmlAsTableRenderPattern <- function(
    template,
    values) {
  
  if (
    !is.character(template) ||
    length(template) != 1L ||
    is.na(template)
  ) {
    stop(
      "`namePattern` must evaluate to one non-NA character string",
      call. = FALSE
    )
  }
  
  matches <- gregexpr(
    "\\{[A-Za-z][A-Za-z0-9_.]*\\}",
    template,
    perl = TRUE
  )
  
  tokens <- regmatches(
    template,
    matches
  )[[1L]]
  
  if (!length(tokens)) {
    return(template)
  }
  
  fields <- substring(
    tokens,
    2L,
    nchar(tokens) - 1L
  )
  
  unknown <- setdiff(
    unique(fields),
    names(values)
  )
  
  if (length(unknown)) {
    stop(
      "Unknown field",
      if (length(unknown) > 1L) "s" else "",
      " in `namePattern`: ",
      paste(
        sprintf("{%s}", unknown),
        collapse = ", "
      ),
      ". Available fields: ",
      paste(
        sprintf("{%s}", names(values)),
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  replacements <- vapply(
    fields,
    function(field) {
      value <- values[[field]]
      
      if (is.null(value)) {
        return("NA")
      }
      
      if (length(value) != 1L) {
        stop(
          "`",
          field,
          "` must have length 1 when used in `namePattern`",
          call. = FALSE
        )
      }
      
      if (
        length(value) == 1L &&
        is.atomic(value) &&
        is.na(value)
      ) {
        return("NA")
      }
      
      as.character(value)
    },
    character(1)
  )
  
  regmatches(
    template,
    matches
  ) <- list(replacements)
  
  template
}


.rdmlAsTableCheckList <- function(
    x,
    arg) {
  
  if (!is.list(x)) {
    stop(
      "`",
      arg,
      "` must evaluate to a list",
      call. = FALSE
    )
  }
  
  if (
    length(x) &&
    (
      is.null(names(x)) ||
      anyNA(names(x)) ||
      any(names(x) == "")
    )
  ) {
    stop(
      "All columns in `",
      arg,
      "` must be named",
      call. = FALSE
    )
  }
  
  x
}


.rdmlAsTableCheckValues <- function(
    x,
    treatNullAsNa) {
  
  if (
    is.null(names(x)) ||
    anyNA(names(x)) ||
    any(names(x) == "")
  ) {
    stop(
      "All asTable columns must be named",
      call. = FALSE
    )
  }
  
  for (nm in names(x)) {
    value <- x[[nm]]
    
    if (is.null(value)) {
      if (treatNullAsNa) {
        x[[nm]] <- NA
      }
      next
    }
    
    if (length(value) != 1L) {
      stop(
        "asTable column '",
        nm,
        "' returned length ",
        length(value),
        "; every expression must return one value per data element",
        call. = FALSE
      )
    }
  }
  
  x
}


.rdmlAsTableLegacyDollarRoot <- function(expr) {
  while (
    is.call(expr) &&
    identical(
      expr[[1L]],
      as.name("$")
    )
  ) {
    expr <- expr[[2L]]
  }
  
  if (is.symbol(expr)) {
    return(
      as.character(expr)
    )
  }
  
  NA_character_
}


.rdmlAsTableRewriteLegacyDollar <- function(expr) {
  if (!is.call(expr)) {
    return(expr)
  }
  
  if (
    identical(
      expr[[1L]],
      as.name("$")
    ) &&
    .rdmlAsTableLegacyDollarRoot(expr) %in%
    c(
      "experiment",
      "run",
      "react",
      "data"
    )
  ) {
    objectExpr <- .rdmlAsTableRewriteLegacyDollar(
      expr[[2L]]
    )
    property <- as.character(
      expr[[3L]]
    )
    
    return(
      substitute(
        S7::prop(
          OBJECT,
          PROPERTY
        ),
        list(
          OBJECT = objectExpr,
          PROPERTY = property
        )
      )
    )
  }
  
  as.call(
    lapply(
      as.list(expr),
      .rdmlAsTableRewriteLegacyDollar
    )
  )
}


.rdmlAsTableEvalCompat <- function(
    expr,
    publicValues,
    legacyValues,
    callerEnv) {
  
  publicError <- NULL
  
  out <- tryCatch(
    eval(
      expr,
      envir = publicValues,
      enclos = callerEnv
    ),
    error = function(e) {
      publicError <<- e
      NULL
    }
  )
  
  if (is.null(publicError)) {
    return(out)
  }
  
  legacyError <- NULL
  
  legacyExpr <- .rdmlAsTableRewriteLegacyDollar(
    expr
  )
  
  out <- tryCatch(
    eval(
      legacyExpr,
      envir = legacyValues,
      enclos = callerEnv
    ),
    error = function(e) {
      legacyError <<- e
      NULL
    }
  )
  
  if (is.null(legacyError)) {
    return(out)
  }
  
  stop(
    conditionMessage(publicError),
    call. = FALSE
  )
}


#' Build an RDML metadata table
#'
#' Each `dataType` becomes one row.
#'
#' The recommended API uses `columns` to select built-in columns and named
#' expressions in `...` to add custom columns. `namePattern` may be a string
#' template such as
#' `"{expId}_{position}_{sample}_{sampleType}_{target}"`.
#'
#' Two levels of values are available when creating table columns.
#'
#' The first level consists of ready-to-use scalar fields:
#' `expId`, `runId`, `reactId`, `position`, `sample`, `target`,
#' `targetDyeId`, `sampleType`, `adp`, and `mdp`.
#'
#' These fields may be used directly in expressions supplied through `...`
#' and in `namePattern` templates.
#'
#' The second level exposes the current RDML objects for advanced custom
#' extraction through `...`:
#' `experiment`, `run`, `react`, `data`, `samples`, `targets`,
#' `dateMade`, `dateUpdated`, `id`, `experimenter`, `documentation`,
#' `dye`, and `thermalCyclingConditions`.
#'
#' These objects are intentionally not available as `{field}` placeholders
#' in `namePattern`, because they are structured RDML/S7 objects rather than
#' scalar values. Values extracted from them in a named `...` expression are
#' available to `namePattern` under the custom column name.
#'
#' For example:
#'
#' `asTable(x, cq = S7::prop(data, "cq"),
#'   namePattern = "{expId}_{position}_{sample}_{cq}_{target}")`
#'
#' `default` and `addColumns` are retained for compatibility with earlier
#' versions. New code should use `columns` and `...`.
#'
#' @param x `rdmlType`.
#' @param default Deprecated compatibility argument. A named list of
#'   expressions defining the base output columns. When supplied, `columns`
#'   must not be used.
#' @param namePattern Character template or expression producing one
#'   `fdataName` value per `dataType`.
#'
#'   In a character template, fields are inserted using `{field}` syntax.
#'   Available built-in template fields are `expId`, `runId`, `reactId`,
#'   `position`, `sample`, `target`, `targetDyeId`, `sampleType`, `adp`,
#'   and `mdp`. Named custom columns supplied through `...` are also
#'   available as template fields.
#'
#'   Example:
#'   `"{expId}_{runId}_{position}_{sample}_{sampleType}_{target}"`.
#' @param addColumns Deprecated compatibility argument. Named list of
#'   additional expressions. New code should pass named expressions in `...`.
#' @param treatNullAsNa Convert `NULL` expression results to `NA`.
#' @param includeHidden Include experiments whose id starts with `.`.
#' @param columns Character vector selecting built-in output columns.
#'   `NULL` uses all built-in columns. Available built-in columns are
#'   `expId`, `runId`, `reactId`, `position`, `sample`, `target`,
#'   `targetDyeId`, `sampleType`, `adp`, and `mdp`.
#'   Custom columns supplied in `...` are appended automatically.
#' @param ... Named expressions defining custom columns. Expressions are
#'   evaluated once per `dataType`.
#'
#'   Ready-to-use scalar fields available directly in these expressions are
#'   `expId`, `runId`, `reactId`, `position`, `sample`, `target`,
#'   `targetDyeId`, `sampleType`, `adp`, and `mdp`.
#'
#'   For advanced extraction, the current RDML objects are also available:
#'   `experiment`, `run`, `react`, `data`, `samples`, `targets`,
#'   `dateMade`, `dateUpdated`, `id`, `experimenter`, `documentation`,
#'   `dye`, and `thermalCyclingConditions`.
#'
#'   Examples:
#'   `short = paste(sample, target, sep = "_")`
#'   or `cq = data$cq`.
#' @return Keyed `data.table`, one row per `dataType`.
#' @rdname asTable
#' @export
S7::method(asTable, rdmlType) <- function(
    x,
    default = NULL,
    namePattern = "{position}_{sample}_{sampleType}_{target}",
    addColumns = NULL,
    treatNullAsNa = FALSE,
    includeHidden = FALSE,
    columns = NULL,
    ...) {
  
  checkmate::assertFlag(treatNullAsNa)
  checkmate::assertFlag(includeHidden)
  
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop(
      "Package 'data.table' is required",
      call. = FALSE
    )
  }
  
  defaultMissing <- missing(default)
  addColumnsMissing <- missing(addColumns)
  columnsMissing <- missing(columns) || is.null(columns)
  
  if (
    !defaultMissing &&
    !columnsMissing
  ) {
    stop(
      "Use either `columns` or legacy `default`, not both",
      call. = FALSE
    )
  }
  
  if (!columnsMissing) {
    checkmate::assertCharacter(
      columns,
      any.missing = FALSE,
      min.len = 0L,
      unique = TRUE
    )
    
    unknownColumns <- setdiff(
      columns,
      .rdmlAsTableDefaultColumns
    )
    
    if (length(unknownColumns)) {
      stop(
        "Unknown built-in column",
        if (length(unknownColumns) > 1L) "s" else "",
        ": ",
        paste(
          unknownColumns,
          collapse = ", "
        ),
        ". Available built-in columns: ",
        paste(
          .rdmlAsTableDefaultColumns,
          collapse = ", "
        ),
        call. = FALSE
      )
    }
  }
  
  callerEnv <- parent.frame()
  
  nameExpr <- substitute(namePattern)
  defaultExpr <- substitute(default)
  addExpr <- substitute(addColumns)
  dotsExpr <- substitute(list(...))
  
  dateMade <- S7::prop(
    x,
    "dateMade"
  )
  dateUpdated <- S7::prop(
    x,
    "dateUpdated"
  )
  id <- .rdmlPropList(
    x,
    "id"
  )
  experimenter <- .rdmlPropKeyed(
    x,
    "experimenter"
  )
  documentation <- .rdmlPropKeyed(
    x,
    "documentation"
  )
  dye <- .rdmlPropKeyed(
    x,
    "dye"
  )
  samples <- .rdmlPropKeyed(
    x,
    "sample"
  )
  targets <- .rdmlPropKeyed(
    x,
    "target"
  )
  thermalCyclingConditions <- .rdmlPropKeyed(
    x,
    "thermalCyclingConditions"
  )
  experiments <- .rdmlPropList(
    x,
    "experiment"
  )
  
  rows <- list()
  rowI <- 0L
  
  for (experiment in experiments) {
    expId <- .rdmlIdChr(
      S7::prop(
        experiment,
        "id"
      )
    )
    
    if (
      !includeHidden &&
      !is.na(expId) &&
      grepl(
        "^\\.",
        expId
      )
    ) {
      next
    }
    
    for (run in .rdmlPropList(
      experiment,
      "run"
    )) {
      for (react in .rdmlPropList(
        run,
        "react"
      )) {
        for (data in .rdmlPropList(
          react,
          "data"
        )) {
          rowI <- rowI + 1L
          
          builtinValues <- .rdmlAsTableBuiltins(
            experiment = experiment,
            run = run,
            react = react,
            data = data,
            samples = samples,
            targets = targets
          )
          
          # Public evaluation context has two documented levels:
          # 1) scalar built-in fields in `builtinValues`;
          # 2) structured RDML objects below, intended for advanced
          #    extraction in named expressions supplied through `...`.
          #
          # Only scalar built-ins and named custom columns are exposed to
          # character `{field}` placeholders in `namePattern`.
          publicValues <- c(
            builtinValues,
            list(
              experiment = experiment,
              run = run,
              react = react,
              data = data,
              samples = samples,
              targets = targets,
              dateMade = dateMade,
              dateUpdated = dateUpdated,
              id = id,
              experimenter = experimenter,
              documentation = documentation,
              dye = dye,
              thermalCyclingConditions = thermalCyclingConditions
            )
          )
          
          # Compatibility context for historical expressions that used `$`
          # on the current experiment/run/react/data objects.
          legacyValues <- list(
            dateMade = dateMade,
            dateUpdated = dateUpdated,
            id = id,
            experimenter = experimenter,
            documentation = documentation,
            dye = dye,
            sample = samples,
            target = targets,
            thermalCyclingConditions = thermalCyclingConditions,
            experiment = experiment,
            run = run,
            react = react,
            data = data,
            expId = builtinValues$expId,
            .rdmlIdChr = .rdmlIdChr,
            .rdmlReactPosition = .rdmlReactPosition,
            .rdmlSampleType = .rdmlSampleType,
            .rdmlTargetDye = .rdmlTargetDye,
            .rdmlPresent = .rdmlPresent
          )
          
          dotsValues <- eval(
            dotsExpr,
            envir = publicValues,
            enclos = callerEnv
          )
          
          dotsValues <- .rdmlAsTableCheckList(
            dotsValues,
            "..."
          )
          
          duplicateCustom <- intersect(
            names(dotsValues),
            names(builtinValues)
          )
          
          if (length(duplicateCustom)) {
            stop(
              "Custom column",
              if (length(duplicateCustom) > 1L) "s" else "",
              " in `...` conflict with built-in field",
              if (length(duplicateCustom) > 1L) "s" else "",
              ": ",
              paste(
                duplicateCustom,
                collapse = ", "
              ),
              call. = FALSE
            )
          }
          
          # Character namePattern templates intentionally see only
          # scalar built-ins plus scalar custom columns. Structured RDML
          # objects from `publicValues` are not template fields.
          nameValues <- c(
            builtinValues,
            dotsValues
          )
          
          rawName <- .rdmlAsTableEvalCompat(
            nameExpr,
            publicValues = c(
              nameValues,
              list(
                experiment = experiment,
                run = run,
                react = react,
                data = data,
                samples = samples,
                targets = targets
              )
            ),
            legacyValues = legacyValues,
            callerEnv = callerEnv
          )
          
          fdataName <- .rdmlAsTableRenderPattern(
            rawName,
            nameValues
          )
          
          if (!defaultMissing) {
            defaultValues <- .rdmlAsTableEvalCompat(
              defaultExpr,
              publicValues = publicValues,
              legacyValues = legacyValues,
              callerEnv = callerEnv
            )
            
            defaultValues <- .rdmlAsTableCheckList(
              defaultValues,
              "default"
            )
          } else {
            selectedColumns <- if (columnsMissing) {
              .rdmlAsTableDefaultColumns
            } else {
              columns
            }
            
            defaultValues <- builtinValues[
              selectedColumns
            ]
          }
          
          if (!addColumnsMissing) {
            addValues <- .rdmlAsTableEvalCompat(
              addExpr,
              publicValues = publicValues,
              legacyValues = legacyValues,
              callerEnv = callerEnv
            )
            
            addValues <- .rdmlAsTableCheckList(
              addValues,
              "addColumns"
            )
          } else {
            addValues <- list()
          }
          
          duplicatedOutput <- intersect(
            names(defaultValues),
            c(
              names(addValues),
              names(dotsValues)
            )
          )
          
          duplicatedExtras <- intersect(
            names(addValues),
            names(dotsValues)
          )
          
          duplicatedOutput <- unique(
            c(
              duplicatedOutput,
              duplicatedExtras
            )
          )
          
          if (length(duplicatedOutput)) {
            stop(
              "Duplicate output column",
              if (length(duplicatedOutput) > 1L) "s" else "",
              ": ",
              paste(
                duplicatedOutput,
                collapse = ", "
              ),
              call. = FALSE
            )
          }
          
          result <- c(
            list(
              fdataName = as.character(
                fdataName
              )
            ),
            defaultValues,
            addValues,
            dotsValues
          )
          
          result <- .rdmlAsTableCheckValues(
            result,
            treatNullAsNa = treatNullAsNa
          )
          
          rows[[rowI]] <- result
        }
      }
    }
  }
  
  if (!length(rows)) {
    return(
      data.table::data.table(
        fdataName = character()
      )
    )
  }
  
  out <- data.table::rbindlist(
    rows,
    fill = TRUE,
    use.names = TRUE
  )
  
  if (anyDuplicated(out$fdataName)) {
    out[
      ,
      fdataName := if (.N > 1L) {
        paste(
          fdataName,
          seq_len(.N),
          sep = "_"
        )
      } else {
        fdataName
      },
      by = fdataName
    ]
    
    warning(
      "fdataName column has duplicates; sequence numbers were added. ",
      "Consider a different `namePattern`.",
      call. = FALSE
    )
  }
  
  data.table::setkey(
    out,
    "fdataName"
  )
  
  return(out[])
}
