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
#' `AsXML()` \tab `asXml()` \cr
#' `MergeRDMLs()` \tab `mergeRdmls()` \cr
#' `rdml_read()` \tab `rdmlRead()` \cr
#' `rdml_write()` \tab `rdmlWrite()` \cr
#' `rdml_formats()` \tab `rdmlFormats()` \cr
#' `rdml_register_format()` \tab `rdmlRegisterFormat()` \cr
#' `rdml_unregister_format()` \tab `rdmlUnregisterFormat()` \cr
#' `rdml_detect_format()` \tab `rdmlDetectFormat()` \cr
#' `rdml_load_module()` \tab `rdmlLoadModule()` \cr
#' `rdml_from_fdata()` \tab `rdmlFromFData()` \cr
#' }
#'
#' Legacy `AsTable()` returns names such as `fdata.name`, `exp.id`, `run.id`,
#' `react.id`, `sample.type`, and `target.dyeId`; `asTable()` uses
#' `fdataName`, `expId`, `runId`, `reactId`, `sampleType`, and `targetDyeId`.
#'
#' @name legacy-api
#' @aliases AsTable GetFData SetFData AsDendrogram AsXML MergeRDMLs 
#' @aliases rdml_read rdml_write rdml_formats rdml_register_format 
#' @aliases rdml_unregister_format rdml_detect_format rdml_load_module 
#' @aliases rdml_from_fdata
#' @seealso `RDML-package`
NULL
