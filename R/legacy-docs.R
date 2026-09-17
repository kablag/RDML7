#' Legacy API compatibility wrappers
#'
#' These functions preserve public names, dotted argument names, and (where
#' applicable) dotted output-column names used by previous RDML releases. New
#' code should use the canonical camelCase API.
#'
#' \tabular{ll}{
#' Legacy \tab Canonical \cr
#' `AsTable()` \tab `asTable()` \cr
#' `GetFData()` \tab `getFData()` \cr
#' `SetFData()` \tab `setFData()` \cr
#' `AsDendrogram()` \tab `asDendrogram()` \cr
#' `asXML()` \tab `asXML()` \cr
#' `MergeRDMLs()` \tab `mergeRDMLs()` \cr
#' }
#'
#' Legacy `AsTable()` returns names such as `fdata.name`, `exp.id`, `run.id`,
#' `react.id`, `sample.type`, and `target.dyeId`; `asTable()` uses
#' `fdataName`, `expId`, `runId`, `reactId`, `sampleType`, and `targetDyeId`.
#'
#' Earlier verb-prefixed aliases are also retained:
#'
#' \tabular{ll}{
#' Alias \tab Canonical \cr
#' `rdmlRead()` \tab `readRDML()` \cr
#' `rdmlWrite()` \tab `writeRDML()` \cr
#' `rdmlRegisterFormat()` \tab `registerRDMLFormat()` \cr
#' `rdmlUnregisterFormat()` \tab `unregisterRDMLFormat()` \cr
#' `rdmlDetectFormat()` \tab `detectRDMLFormat()` \cr
#' `rdmlFormats()` \tab `listRDMLFormats()` \cr
#' `rdmlLoadModule()` \tab `loadRDMLModule()` \cr
#' `rdmlBuildImport()` \tab `buildRDMLImport()` \cr
#' `rdmlFromFData()` \tab `buildRDMLFromFData()` \cr
#' `rdmlValidate()` \tab `validateRDML()` \cr
#' `rdmlIsValid()` \tab `isValidRDML()` \cr
#' `rdmlEdit()` \tab `editRDML()` \cr
#' `mergeRdmls()` \tab `mergeRDMLs()` \cr
#' `asXml()` \tab `asXML()` \cr
#' `rdmlLossRecord()` \tab `newRDMLLossRecord()` \cr
#' }
#'
#' @param x An [rdmlType] object, or a [dataType] for `GetFData()`.
#' @param .default Named list of expressions defining the base `AsTable()`
#'   output columns. See [asTable()] for values available during evaluation.
#' @param name.pattern Expression defining one legacy `fdata.name` per target
#'   result.
#' @param add.columns Named list of additional `AsTable()` expressions.
#' @param treat.null.as.na Logical. Convert `NULL` expression results to `NA`.
#' @param include.hidden Logical. Include experiment identifiers beginning
#'   with `.`.
#' @param request Legacy metadata table describing curves to extract. Required
#'   columns are `fdata.name`, `exp.id`, `run.id`, `react.id`, and `target`.
#' @param dp.type Curve type: `"adp"` or `"mdp"`.
#' @param long.table Logical. Return long rather than wide fluorescence data.
#' @param fdata Wide fluorescence matrix or table; the first column is `cyc`
#'   for `"adp"` or `tmp` for `"mdp"`.
#' @param description Legacy metadata table describing `fdata` columns.
#' @param fdata.type Curve type: `"adp"` or `"mdp"`.
#' @param conflict Metadata conflict policy: `"error"`, `"keep"`, or
#'   `"replace"`.
#' @param plot.dendrogram Logical. Plot the generated dendrogram.
#' @param to.merge Non-empty list of [rdmlType] objects.
#' @param data.conflict Duplicate target-data policy: `"incoming"`, `"base"`,
#'   or `"error"`.
#' @param ... Additional arguments passed unchanged to the canonical API.
#'
#' @return `AsTable()` and `GetFData()` return [data.table::data.table]
#'   objects with legacy dotted column names where applicable. `SetFData()`
#'   and `MergeRDMLs()` return [rdmlType] objects. `AsDendrogram()` returns a
#'   `dendrogram`. Alias functions return exactly what their canonical
#'   counterparts return.
#'
#' @examples
#' amplificationFile <- system.file(
#'   "extdata", "lc96_bACTXY.rdml", package = "RDML7"
#' )
#' meltingFile <- system.file(
#'   "extdata", "BioRad_qPCR_melt.rdml", package = "RDML7"
#' )
#' amplification <- rdmlRead(amplificationFile, showProgress = FALSE)
#' melting <- rdmlRead(meltingFile, showProgress = FALSE)
#'
#' legacyMeta <- AsTable(amplification)
#' legacyCurves <- GetFData(
#'   amplification,
#'   request = legacyMeta[seq_len(min(2L, nrow(legacyMeta))), ],
#'   dp.type = "adp"
#' )
#' copied <- SetFData(
#'   rdmlType(),
#'   legacyCurves,
#'   legacyMeta[seq_len(min(2L, nrow(legacyMeta))), ],
#'   fdata.type = "adp"
#' )
#' tree <- AsDendrogram(melting, plot.dendrogram = FALSE)
#' merged <- MergeRDMLs(list(amplification, melting), data.conflict = "base")
#'
#' @name legacy-api
#' @seealso [asTable()], [getFData()], [setFData()], [asDendrogram()],
#'   [asXML()], [mergeRDMLs()], `RDML7-package`
NULL
