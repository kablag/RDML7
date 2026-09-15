# Public S7 generics --------------------------------------------------------

#' Build a metadata table from an RDML object
#'
#' @param x Object to convert.
#' @param ... Method-specific arguments.
#' @return A tabular representation of `x`.
#' @seealso `getFData()`, `setFData()`
#' @export
asTable <- S7::new_generic("asTable", "x")

#' Extract fluorescence data
#'
#' @param x Object from which to extract fluorescence data.
#' @param ... Method-specific arguments.
#' @return A tabular representation of the fluorescence data.
#' @seealso `asTable()`, `setFData()`
#' @export
getFData <- S7::new_generic("getFData", "x")

#' Add or replace fluorescence data
#'
#' @param x Object to modify.
#' @param ... Method-specific arguments.
#' @return The modified object; assign it back to keep changes.
#' @seealso `buildRDMLFromFData()`, `asTable()`, `getFData()`
#' @export
setFData <- S7::new_generic("setFData", "x")

#' Represent RDML structure as a dendrogram
#'
#' @param x Object to represent as a dendrogram.
#' @param ... Method-specific arguments.
#' @return Dendrogram representation invisibly.
#' @seealso `summary()`
#' @export
asDendrogram <- S7::new_generic("asDendrogram", "x")

#' Serialize an RDML object to XML
#'
#' @param x Object to serialize.
#' @param ... Method-specific arguments.
#' @return XML text or output path invisibly.
#' @seealso `writeRDML()`
#' @export
asXML <- S7::new_generic("asXML", "x")
