# Import helpers -------------------------------------------------------------

# XML namespace scratch environment retained for compatibility with the
# upstream parser. It is used only during synchronous import calls.
rdml.env <- new.env(parent = emptyenv())

.is_xml_missing <- function(x) inherits(x, "xml_missing")

.xml_nodes_apply <- function(nodes, FUN) {
  if (length(nodes) == 0L) return(list())
  lapply(seq_along(nodes), function(i) FUN(nodes[[i]]))
}

.compact <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

.list_get_by_key <- function(x, key, key_property = "id") {
  if (!is.list(x) || length(x) == 0L) return(NULL)
  pos <- match(key, .get_keys(x, key_property))
  if (is.na(pos)) return(NULL)
  x[[pos]]
}

.list_set_by_key <- function(x, key, value, key_property = "id") {
  if (!is.list(x)) stop("`x` must be a list", call. = FALSE)

  keys <- if (length(x)) .get_keys(x, key_property) else character()
  pos <- match(key, keys)

  if (is.null(value)) {
    if (!is.na(pos)) x[[pos]] <- NULL
    return(x)
  }

  value_key <- .get_key(value, key_property)
  if (is.na(value_key) || !identical(value_key, key)) {
    stop(
      "replacement key ('", key, "') does not match object ",
      key_property, " ('", value_key, "')",
      call. = FALSE
    )
  }

  if (is.na(pos)) x[[length(x) + 1L]] <- value else x[[pos]] <- value
  x
}

getTextValue <- function(tree, path, ns = rdml.env$ns) {
  node <- xml2::xml_find_first(tree, path, ns)
  if (.is_xml_missing(node)) return(NA_character_)
  txt <- xml2::xml_text(node)
  if (!length(txt) || !nzchar(trimws(txt))) return(NA_character_)
  txt
}

getTextVector <- function(tree, path, ns = rdml.env$ns) {
  xml2::xml_text(xml2::xml_find_all(tree, path, ns))
}

getLogicalValue <- function(tree, path, ns = rdml.env$ns) {
  node <- xml2::xml_find_first(tree, path, ns)
  if (.is_xml_missing(node)) return(NA)
  switch(
    tolower(xml2::xml_text(node)),
    "true" = TRUE,
    "false" = FALSE,
    as.logical(xml2::xml_text(node))
  )
}

getNumericValue <- function(tree, path, ns = rdml.env$ns) {
  node <- xml2::xml_find_first(tree, path, ns)
  if (.is_xml_missing(node)) return(NA_real_)
  out <- .rdml_as_numeric(xml2::xml_text(node))
  if (!length(out)) NA_real_ else out[[1L]]
}

getNumericVector <- function(tree, path, ns = rdml.env$ns) {
  .rdml_as_numeric(xml2::xml_text(xml2::xml_find_all(tree, path, ns)))
}

getIntegerValue <- function(tree, path, ns = rdml.env$ns) {
  node <- xml2::xml_find_first(tree, path, ns)
  if (.is_xml_missing(node)) return(NA_integer_)
  xml2::xml_integer(node)
}

getIntegerVector <- function(tree, path, ns = rdml.env$ns) {
  xml2::xml_integer(xml2::xml_find_all(tree, path, ns))
}

genId <- function(node) {
  id <- xml2::xml_attr(node, "id")
  if (length(id) != 1L || is.na(id)) stop("XML node has no id attribute")
  idType(id = id)
}

genIdRef <- function(node) {
  id <- xml2::xml_attr(node, "id")
  if (length(id) != 1L || is.na(id)) stop("XML node has no id attribute")
  idReferenceType(id = id)
}

.rdml_as_numeric <- function(val) {
  if (!length(val)) return(NULL)
  out <- suppressWarnings(base::as.numeric(val))
  bad <- is.na(out) & !is.na(val)
  if (any(bad)) {
    out[bad] <- suppressWarnings(base::as.numeric(gsub(",", ".", val[bad])))
  }
  out
}

FromPositionToId <- function(
    react.id,
    pcrFormat = pcrFormatType(
      rows = 8L,
      columns = 12L,
      rowLabel = labelFormatType("ABC"),
      columnLabel = labelFormatType("123")
    )) {
  row <- which(LETTERS == gsub("([A-Z])[0-9]+", "\\1", react.id))
  col <- base::as.integer(gsub("[A-Z]([0-9]+)", "\\1", react.id))
  (row - 1L) * pcrFormat$columns + col
}

GetIds <- function(l) {
  unname(vapply(l, .get_id, character(1)))
}

.rdml_new_import <- function(publisher = NULL, serial_number = "1") {
  ids <- if (is.null(publisher)) {
    list()
  } else {
    list(rdmlIdType(
      publisher = publisher,
      serialNumber = serial_number,
      MD5Hash = NA_character_
    ))
  }

  rdmlType(
    version = "1.2",
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

.rdml_set_fdata_import <- function(x, fdata, description, fdata.type = "adp") {
  SetFData(x, fdata, description, fdata.type = fdata.type)
}

.split_ws <- function(x) {
  strsplit(trimws(x), "\\s+", perl = TRUE)[[1L]]
}

.first_match_field <- function(
    x, field, value, return_field, default = NA_character_) {
  for (el in x) {
    if (length(el) >= max(field, return_field) && identical(el[[field]], value)) {
      return(el[[return_field]])
    }
  }
  default
}
