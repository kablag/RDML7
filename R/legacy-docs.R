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
#' @param x RDML object or data element passed to the canonical API.
#' @param .default Named list of expressions defining legacy output columns.
#' @param name.pattern Expression defining legacy fluorescence-data names.
#' @param add.columns Named list of additional column expressions.
#' @param treat.null.as.na Convert `NULL` expression results to `NA`.
#' @param include.hidden Include experiments whose id starts with `.`.
#' @param request Metadata table describing curves to extract.
#' @param dp.type Fluorescence type: `"adp"` or `"mdp"`.
#' @param long.table Return long rather than wide fluorescence data.
#' @param fdata Fluorescence matrix or table.
#' @param description Metadata table describing `fdata` columns.
#' @param fdata.type Fluorescence type: `"adp"` or `"mdp"`.
#' @param conflict Conflict policy passed to `setFData()`.
#' @param plot.dendrogram Plot the generated dendrogram.
#' @param to.merge List of RDML objects to merge.
#' @param data.conflict Policy for conflicting data during a merge.
#' @param ... Additional arguments passed to the canonical API.
#'
#' @name legacy-api
#' @seealso `RDML7-package`
NULL
