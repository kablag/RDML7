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
#' `MergeRDMLs()` \tab `mergeRdmls()` \cr
#' }
#'
#' Legacy `AsTable()` returns names such as `fdata.name`, `exp.id`, `run.id`,
#' `react.id`, `sample.type`, and `target.dyeId`; `asTable()` uses
#' `fdataName`, `expId`, `runId`, `reactId`, `sampleType`, and `targetDyeId`.
#'
#' @name legacy-api
#' @aliases AsTable GetFData SetFData AsDendrogram asXML MergeRDMLs 
#' @seealso `RDML-package`
NULL
