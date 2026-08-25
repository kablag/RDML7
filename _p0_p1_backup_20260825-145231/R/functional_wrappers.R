# Functional S7 API --------------------------------------------------------
#
# The original package exposed these operations as mutable R6 methods
# (obj$AsTable(), obj$GetFData(), ...).  rdml7 uses S7 value objects, so the
# public API is expressed as S7 generics.

#' Convert an RDML object to a description table
#' @param x An RDML object.
#' @param ... Method-specific arguments.
#' @export
AsTable <- S7::new_generic("AsTable", "x")

#' Get fluorescence data
#' @param x An RDML or dataType object.
#' @param ... Method-specific arguments.
#' @export
GetFData <- S7::new_generic("GetFData", "x")

#' Set fluorescence data
#' @param x An RDML object.
#' @param ... Method-specific arguments.
#' @return A modified RDML object. Assign the result back to keep changes.
#' @export
SetFData <- S7::new_generic("SetFData", "x")

#' Represent RDML structure as a dendrogram
#' @param x An RDML object.
#' @param ... Method-specific arguments.
#' @export
AsDendrogram <- S7::new_generic("AsDendrogram", "x")

#' Serialize RDML to XML / RDML archive
#' @param x An RDML object.
#' @param ... Method-specific arguments.
#' @export
AsXML <- S7::new_generic("AsXML", "x")
