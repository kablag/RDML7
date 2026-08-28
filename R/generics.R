# Public S7 generics --------------------------------------------------------

#' Build a metadata table from an RDML object
#'
#' @param x Object to convert.
#' @param ... Method-specific arguments.
#' @return A `data.table` for `rdmlType`.
#' @seealso `getFData()`, `setFData()`
#' @export
asTable <- S7::new_generic("asTable", "x")

#' Extract fluorescence data
#'
#' @param x `dataType` or `rdmlType`.
#' @param ... Method-specific arguments.
#' @return A `data.table`.
#' @seealso `asTable()`, `setFData()`
#' @export
getFData <- S7::new_generic("getFData", "x")

#' Add or replace fluorescence data
#'
#' @param x `rdmlType`.
#' @param ... Method-specific arguments.
#' @return Modified `rdmlType`; assign it back to keep changes.
#' @seealso `rdmlFromFData()`, `asTable()`, `getFData()`
#' @export
setFData <- S7::new_generic("setFData", "x")

#' Represent RDML structure as a dendrogram
#'
#' @param x `rdmlType`.
#' @param ... Method-specific arguments.
#' @return Dendrogram representation invisibly.
#' @seealso `rdmlSummary()`
#' @export
asDendrogram <- S7::new_generic("asDendrogram", "x")

#' Serialize an RDML object to XML
#'
#' @param x `rdmlType`.
#' @param ... Method-specific arguments.
#' @return XML text or output path invisibly.
#' @seealso `rdmlWrite()`
#' @export
asXml <- S7::new_generic("asXml", "x")
