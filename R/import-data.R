#' @include properties.R conditions.R set-fdata.R rdml-utils.R
NULL

# Canonical importer intermediate representation ----------------------------

classImportFdataType <- S7::new_property(
  S7::class_character,
  validator = function(value) {
    if (
      length(value) != 1L ||
      is.na(value) ||
      !value %in% c("adp", "mdp")
    ) {
      return("must be one of: adp, mdp")
    }
    NULL
  }
)


#' One fluorescence series produced by an importer
#'
#' @param fdataType `"adp"` or `"mdp"`.
#' @param fdata Wide fluorescence table accepted by `setFData()`.
#' @param description Camel-case description table accepted by `setFData()`.
#' @export
rdmlImportSeries <- S7::new_class(
  "rdmlImportSeries",
  properties = list(
    fdataType = classImportFdataType,
    fdata = classDataTable,
    description = classDataTable
  ),
  validator = function(self) {
    if (ncol(self@fdata) < 2L) {
      return("@fdata must contain a coordinate column and fluorescence data")
    }

    required <- c(
      "fdataName",
      "expId",
      "runId",
      "reactId",
      "target"
    )

    missingColumns <- setdiff(
      required,
      names(self@description)
    )

    if (length(missingColumns)) {
      return(paste0(
        "@description is missing: ",
        paste(missingColumns, collapse = ", ")
      ))
    }

    NULL
  }
)


# Public convenience accessors for importer intermediate objects.
#
# These classes are intentionally not rdmlBaseType objects, but plugin authors
# should still be able to inspect and modify them with the same `$` syntax used
# elsewhere in RDML7.
S7::method(names, rdmlImportSeries) <- function(x) {
  S7::prop_names(x)
}

S7::method(`$`, rdmlImportSeries) <- function(x, name) {
  S7::prop(x, name)
}

S7::method(`$<-`, rdmlImportSeries) <- function(x, name, value) {
  S7::prop(x, name) <- value
  x
}


.testImportSeriesList <- S7::new_property(
  S7::class_list,
  validator = function(value) {
    if (!length(value)) {
      return("must contain at least one rdmlImportSeries")
    }

    ok <- vapply(
      value,
      function(x) S7::S7_inherits(x, rdmlImportSeries),
      logical(1)
    )

    if (!all(ok)) {
      return("must be a list of rdmlImportSeries objects")
    }

    NULL
  }
)


#' Parsed importer data before construction of rdmlType
#'
#' Vendor readers are encouraged to return this object. `rdmlRead()` then
#' builds the RDML tree centrally, which keeps import modules independent from
#' the internal experiment/run/react/data hierarchy.
#'
#' @param series List of `rdmlImportSeries` objects.
#' @param publisher Optional source/device publisher.
#' @param serialNumber Source/device serial identifier.
#' @param version RDML version used for the constructed object.
#' @param format Source format identifier.
#' @param preserveReactIds If TRUE, generated pcrFormat definitions are removed
#'   so supplied reaction/well identifiers remain literal (useful for RDES and
#'   rotor instruments).
#' @param metadata Additional importer metadata retained during construction.
#' @param losses List of `rdmlLossRecord()` objects.
#' @export
rdmlImportData <- S7::new_class(
  "rdmlImportData",
  properties = list(
    series = .testImportSeriesList,
    publisher = classCharacterNaNonemptySingle,
    serialNumber = classCharacterNonemptySingle,
    version = classCharacterNonemptySingle,
    format = classCharacterNaNonemptySingle,
    preserveReactIds = classFlag,
    metadata = S7::new_property(S7::class_list, default = list()),
    losses = S7::new_property(S7::class_list, default = list())
  ),
  constructor = function(
      series,
      publisher = NA_character_,
      serialNumber = "1",
      version = "1.2",
      format = NA_character_,
      preserveReactIds = FALSE,
      metadata = list(),
      losses = list()) {

    S7::new_object(
      S7::S7_object(),
      series = series,
      publisher = publisher,
      serialNumber = serialNumber,
      version = version,
      format = format,
      preserveReactIds = preserveReactIds,
      metadata = metadata,
      losses = losses
    )
  }
)


S7::method(names, rdmlImportData) <- function(x) {
  S7::prop_names(x)
}

S7::method(`$`, rdmlImportData) <- function(x, name) {
  S7::prop(x, name)
}

S7::method(`$<-`, rdmlImportData) <- function(x, name, value) {
  S7::prop(x, name) <- value
  x
}


.rdmlImportTouchedRuns <- function(importData) {
  rows <- data.table::rbindlist(
    lapply(
      S7::prop(importData, "series"),
      function(series) {
        description <- data.table::as.data.table(S7::prop(series, "description"))
        description[, .(expId, runId)]
      }
    ),
    use.names = TRUE,
    fill = TRUE
  )

  unique(rows)
}


.rdmlPreserveReactIds <- function(x, importData) {
  pairs <- .rdmlImportTouchedRuns(importData)
  experiments <- .rdmlPropKeyed(x, "experiment")

  for (i in seq_len(nrow(pairs))) {
    expId <- as.character(pairs$expId[[i]])
    runId <- as.character(pairs$runId[[i]])

    experiment <- experiments[[expId]]
    if (is.null(experiment)) next

    runs <- .rdmlPropKeyed(experiment, "run")
    run <- runs[[runId]]
    if (is.null(run)) next

    S7::prop(run, "pcrFormat") <- NA
    runs[[runId]] <- run
    experiment <- .rdmlSetPropList(experiment, "run", runs)
    experiments[[expId]] <- experiment
  }

  .rdmlSetPropList(x, "experiment", experiments)
}


#' Build rdmlType from canonical importer data
#'
#' @param importData `rdmlImportData` object.
#' @param loss How to handle declared lossy conversions: `"warn"`, `"error"`,
#'   or `"allow"`.
#' @param ... Additional arguments forwarded to `setFData()`.
#' @return `rdmlType`.
#' @export
rdmlBuildImport <- function(
    importData,
    loss = c("warn", "error", "allow"),
    ...) {

  if (!S7::S7_inherits(importData, rdmlImportData)) {
    .rdmlAbort(
      code = "invalidImportData",
      message = "`importData` must be an rdmlImportData object"
    )
  }

  loss <- match.arg(loss)
  .rdmlSignalLosses(S7::prop(importData, "losses"), loss = loss)

  publisher <- if (.rdmlIsMissing(S7::prop(importData, "publisher"))) {
    NULL
  } else {
    S7::prop(importData, "publisher")
  }

  x <- .rdmlNewImport(
    publisher = publisher,
    serialNumber = S7::prop(importData, "serialNumber")
  )

  S7::prop(x, "version") <- S7::prop(importData, "version")

  for (series in S7::prop(importData, "series")) {
    x <- setFData(
      x,
      S7::prop(series, "fdata"),
      S7::prop(series, "description"),
      fdataType = S7::prop(series, "fdataType"),
      ...
    )
  }

  if (isTRUE(S7::prop(importData, "preserveReactIds"))) {
    x <- .rdmlPreserveReactIds(x, importData)
  }

  x
}
