# Import helpers -------------------------------------------------------------

# XML namespace scratch environment retained for compatibility with the
# upstream parser. It is used only during synchronous import calls.
rdmlEnv <- new.env(parent = emptyenv())

.isXmlMissing <- function(x) inherits(x, "xml_missing")

.xmlNodesApply <- function(nodes, FUN) {
  if (length(nodes) == 0L) return(list())
  lapply(seq_along(nodes), function(i) FUN(nodes[[i]]))
}

.compact <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

.listGetByKey <- function(x, key, keyProperty = "id") {
  if (!is.list(x) || length(x) == 0L) return(NULL)
  pos <- match(key, .getKeys(x, keyProperty))
  if (is.na(pos)) return(NULL)
  x[[pos]]
}

.listSetByKey <- function(x, key, value, keyProperty = "id") {
  if (!is.list(x)) stop("`x` must be a list", call. = FALSE)

  keys <- if (length(x)) .getKeys(x, keyProperty) else character()
  pos <- match(key, keys)

  if (is.null(value)) {
    if (!is.na(pos)) x[[pos]] <- NULL
    return(x)
  }

  valueKey <- .getKey(value, keyProperty)
  if (is.na(valueKey) || !identical(valueKey, key)) {
    stop(
      "replacement key ('", key, "') does not match object ",
      keyProperty, " ('", valueKey, "')",
      call. = FALSE
    )
  }

  if (is.na(pos)) x[[length(x) + 1L]] <- value else x[[pos]] <- value
  x
}

.getTextValue <- function(tree, path, ns = rdmlEnv$ns) {
  node <- xml2::xml_find_first(tree, path, ns)
  if (.isXmlMissing(node)) return(NA_character_)
  txt <- xml2::xml_text(node)
  if (!length(txt) || !nzchar(trimws(txt))) return(NA_character_)
  txt
}

.getTextVector <- function(tree, path, ns = rdmlEnv$ns) {
  xml2::xml_text(xml2::xml_find_all(tree, path, ns))
}

.getLogicalValue <- function(tree, path, ns = rdmlEnv$ns) {
  node <- xml2::xml_find_first(tree, path, ns)
  if (.isXmlMissing(node)) return(NA)
  switch(
    tolower(xml2::xml_text(node)),
    "true" = TRUE,
    "false" = FALSE,
    as.logical(xml2::xml_text(node))
  )
}

.getNumericValue <- function(tree, path, ns = rdmlEnv$ns) {
  node <- xml2::xml_find_first(tree, path, ns)
  if (.isXmlMissing(node)) return(NA_real_)
  out <- .rdmlAsNumeric(xml2::xml_text(node))
  if (!length(out)) NA_real_ else out[[1L]]
}

.getNumericVector <- function(tree, path, ns = rdmlEnv$ns) {
  .rdmlAsNumeric(xml2::xml_text(xml2::xml_find_all(tree, path, ns)))
}

.getIntegerValue <- function(tree, path, ns = rdmlEnv$ns) {
  node <- xml2::xml_find_first(tree, path, ns)
  if (.isXmlMissing(node)) return(NA_integer_)
  xml2::xml_integer(node)
}

.getIntegerVector <- function(tree, path, ns = rdmlEnv$ns) {
  xml2::xml_integer(xml2::xml_find_all(tree, path, ns))
}

.genId <- function(node) {
  id <- xml2::xml_attr(node, "id")
  if (length(id) != 1L || is.na(id)) stop("XML node has no id attribute")
  idType(id = id)
}

.genIdRef <- function(node) {
  id <- xml2::xml_attr(node, "id")
  if (length(id) != 1L || is.na(id)) stop("XML node has no id attribute")
  idReferenceType(id = id)
}

.rdmlAsNumeric <- function(val) {
  if (!length(val)) return(NULL)
  out <- suppressWarnings(base::as.numeric(val))
  bad <- is.na(out) & !is.na(val)
  if (any(bad)) {
    out[bad] <- suppressWarnings(base::as.numeric(gsub(",", ".", val[bad])))
  }
  out
}

.fromPositionToId <- function(
    reactId,
    pcrFormat = pcrFormatType(
      rows = 8L,
      columns = 12L,
      rowLabel = labelFormatType("ABC"),
      columnLabel = labelFormatType("123")
    )) {
  row <- which(LETTERS == gsub("([A-Z])[0-9]+", "\\1", reactId))
  col <- base::as.integer(gsub("[A-Z]([0-9]+)", "\\1", reactId))
  (row - 1L) * pcrFormat$columns + col
}

.getIds <- function(l) {
  unname(vapply(l, .getId, character(1)))
}

.rdmlNewImport <- function(publisher = NULL, serialNumber = "1") {
  ids <- if (is.null(publisher)) {
    list()
  } else {
    list(rdmlIdType(
      publisher = publisher,
      serialNumber = serialNumber,
      MD5Hash = NA_character_
    ))
  }

  rdmlType(
    dateMade = NA_character_,
    dateUpdated = NA_character_,
    id = ids,
    experimenter = list(),
    documentation = list(),
    dye = list(),
    sample = list(),
    target = list(),
    thermalCyclingConditions = list(),
    experiment = list()
  )
}

.rdmlsetFDataImport <- function(x, fdata, description, fdataType = "adp") {
  setFData(x, fdata, description, fdataType = fdataType)
}

.splitWs <- function(x) {
  strsplit(trimws(x), "\\s+", perl = TRUE)[[1L]]
}

.firstMatchField <- function(
    x, field, value, returnField, default = NA_character_) {
  for (el in x) {
    if (length(el) >= max(field, returnField) && identical(el[[field]], value)) {
      return(el[[returnField]])
    }
  }
  default
}


