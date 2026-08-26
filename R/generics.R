# Public S7 generics --------------------------------------------------------
#
# The original package exposed these operations as mutable R6 methods
# (obj$asTable(), obj$getFData(), ...).  rdml7 uses S7 value objects, so the
# public API is expressed as S7 generics.

#' Convert an RDML object to a description table
#' @param x An RDML object.
#' @param ... Method-specific arguments.
#' @export
asTable <- S7::new_generic("asTable", "x")

#' Get fluorescence data
#' @param x An RDML or dataType object.
#' @param ... Method-specific arguments.
#' @export
getFData <- S7::new_generic("getFData", "x")

#' Set fluorescence data
#' @param x An RDML object.
#' @param ... Method-specific arguments.
#' @return A modified RDML object. Assign the result back to keep changes.
#' @export
setFData <- S7::new_generic("setFData", "x")

#' Represent RDML structure as a dendrogram
#' @param x An RDML object.
#' @param ... Method-specific arguments.
#' @export
asDendrogram <- S7::new_generic("asDendrogram", "x")

#' Serialize RDML to XML / RDML archive
#' @param x An RDML object.
#' @param ... Method-specific arguments.
#' @export
asXml <- S7::new_generic("asXml", "x")
