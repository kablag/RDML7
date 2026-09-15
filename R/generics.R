# Public S7 generics --------------------------------------------------------

#' Build a metadata table from an RDML object
#'
#' @param x Object to convert.
#' @param ... Method-specific arguments.
#' @details The `rdmlType` method supports selecting built-in columns with
#'   `columns`, adding named expressions through `...`, controlling hidden
#'   experiments with `includeHidden`, and compatibility arguments `default`,
#'   `namePattern`, `addColumns`, and `treatNullAsNa`.
#' @return A tabular representation of `x`.
#' @seealso `getFData()`, `setFData()`
#' @export
asTable <- S7::new_generic("asTable", "x")

#' Extract fluorescence data
#'
#' @param x Object from which to extract fluorescence data.
#' @param ... Method-specific arguments.
#' @details The `dataType` and `rdmlType` methods accept `dpType` to select
#'   amplification or melting data. The `rdmlType` method additionally accepts
#'   `request`, `longTable`, and `includeMissing`.
#' @return A tabular representation of the fluorescence data.
#' @seealso `asTable()`, `setFData()`
#' @export
getFData <- S7::new_generic("getFData", "x")

#' Add or replace fluorescence data
#'
#' @param x Object to modify.
#' @param ... Method-specific arguments.
#' @details The `rdmlType` method accepts `fdata`, `description`, `fdataType`,
#'   and `conflict` to supply fluorescence and metadata tables, select the curve
#'   type, and resolve existing data.
#' @return The modified object; assign it back to keep changes.
#' @seealso `buildRDMLFromFData()`, `asTable()`, `getFData()`
#' @export
setFData <- S7::new_generic("setFData", "x")

#' Represent RDML structure as a dendrogram
#'
#' @param x Object to represent as a dendrogram.
#' @param ... Method-specific arguments.
#' @details The `rdmlType` method accepts `plotDendrogram` to control plotting.
#' @return Dendrogram representation invisibly.
#' @seealso `summary()`
#' @export
asDendrogram <- S7::new_generic("asDendrogram", "x")

#' Serialize an RDML object to XML
#'
#' @param x Object to serialize.
#' @param ... Method-specific arguments.
#' @details The `rdmlType` method accepts an optional `fileName` output path and
#'   a `loss` policy (`"warn"`, `"error"`, or `"allow"`) for package data that
#'   standard RDML XML cannot represent.
#' @return XML text or output path invisibly.
#' @seealso `writeRDML()`
#' @export
asXML <- S7::new_generic("asXML", "x")
