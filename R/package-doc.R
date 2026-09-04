#' RDML: qPCR data import, conversion, validation, and export
#'
#' `RDML` provides S7 classes and a functional API for Real-time PCR Data
#' Markup Language (RDML) data and several instrument/vendor formats. The
#' canonical API uses lower camel case and value semantics: functions that
#' modify an object return a new `rdmlType` that should be assigned back.
#'
#' @section Main workflow:
#' A typical workflow is:
#'
#' 1. Read a file with `readRDML()`.
#' 2. Inspect it with `rdmlSummary()` and `validateRDML()`.
#' 3. Build a metadata table with `asTable()`.
#' 4. Extract curves with `getFData()`.
#' 5. Modify or add curves with `setFData()`.
#' 6. Export with `writeRDML()`.
#'
#' Use `rdmlFormats()` to list registered file formats and their capabilities.
#'
#' @section Value semantics:
#' RDML objects are S7 value objects, not mutable R6 environments. Therefore:
#'
#' ```
#' x <- setFData(x, fdata, description)
#' ```
#'
#' rather than relying on in-place reference mutation.
#'
#' @section File-format registry:
#' Built-in readers cover RDML/XML and several vendor/table formats. The
#' registry is extensible through `rdmlRegisterFormat()` and
#' `rdmlLoadModule()`. Third-party importers may return either an `rdmlType`
#' directly or an `rdmlImportData` intermediate representation.
#'
#' @section RDES:
#' RDES (Real-time PCR Data Essential Spreadsheet Format) is supported for
#' import and export. It is a UTF-8, tab-separated format for one run, with
#' amplification and melting data stored in separate files. See
#' `vignette("RDES", package = "RDML")`.
#'
#' @section Legacy API:
#' Historical names such as `AsTable()`, `GetFData()`, `SetFData()` and
#' `MergeRDMLs()` remain available as compatibility
#' wrappers. New code should use the camelCase API.
#'
#' @seealso
#' `readRDML()`, `writeRDML()`, `asTable()`, `getFData()`, `setFData()`,
#' `validateRDML()`, `rdmlSummary()`, `rdmlFormats()`
#'
#' @name RDML-package
#' @aliases RDML RDML-package
#' @keywords internal
"_PACKAGE"
