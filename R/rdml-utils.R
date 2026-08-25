# RDML object utilities ----------------------------------------------------

.rdml_is_missing <- function(x) {
  if (is.null(x)) {
    return(TRUE)
  }

  # S7 objects (including idType, dataType, rdmlKeyedList, etc.)
  # are values, never scalar NA sentinels.
  if (S7::S7_inherits(x)) {
    return(FALSE)
  }

  length(x) == 1L &&
    is.atomic(x) &&
    isTRUE(is.na(x))
}

.rdml_present <- function(x) {
  !.rdml_is_missing(x)
}

.rdml_as_list <- function(x) {
  if (.rdml_is_missing(x)) {
    return(list())
  }
  if (S7::S7_inherits(x, rdmlKeyedList)) {
    return(S7::S7_data(x))
  }
  if (is.list(x)) {
    return(x)
  }
  list(x)
}

# Read a list-valued S7 property without relying on physical names().
.rdml_prop_list <- function(x, name) {
  .rdml_as_list(S7::prop(x, name))
}

# Read a property as rdmlKeyedList when its S7_property contains rdml_key.
.rdml_prop_keyed <- function(x, name) {
  value <- .rdml_prop_list(x, name)
  key <- .property_key(x, name)
  if (is.null(key)) {
    return(value)
  }
  rdmlKeyedList(value, key = key)
}

# Set a list-valued property, stripping the transient rdmlKeyedList wrapper.
.rdml_set_prop_list <- function(x, name, value) {
  if (S7::S7_inherits(value, rdmlKeyedList)) {
    value <- S7::S7_data(value)
  }
  names(value) <- NULL
  S7::prop(x, name) <- value
  x
}

.rdml_id_chr <- function(x) {
  
  if (.rdml_is_missing(x)) {
    return(NA_character_)
  }
  
  if (S7::S7_inherits(x, idType)) {
    id <- S7::prop(x, "id")
    
    if (
      is.character(id) &&
      length(id) == 1L
    ) {
      return(id)
    }
    
    return(NA_character_)
  }
  
  if (
    is.character(x) &&
    length(x) == 1L
  ) {
    return(x)
  }
  
  NA_character_
}

.rdml_enum_chr <- function(x) {
  
  if (.rdml_is_missing(x)) {
    return(NA_character_)
  }
  
  if (S7::S7_inherits(x, rdmlEnum)) {
    value <- S7::prop(x, "value")
    
    if (
      is.character(value) &&
      length(value) == 1L
    ) {
      return(value)
    }
    
    return(NA_character_)
  }
  
  if (
    is.character(x) &&
    length(x) == 1L
  ) {
    return(x)
  }
  
  NA_character_
}

# Human-readable well position. reactType in rdml7 intentionally stores only
# the RDML reaction id; position is derived from run$pcrFormat when needed.
.rdml_react_position <- function(react, pcrFormat) {
  id <- .rdml_id_chr(react$id)
  if (is.na(id)) {
    return(NA_character_)
  }

  n <- suppressWarnings(base::as.integer(id))
  if (is.na(n) || .rdml_is_missing(pcrFormat)) {
    # Imported hand-labelled ids are already human-readable.
    return(id)
  }

  rows <- pcrFormat$rows
  columns <- pcrFormat$columns
  row_label <- .rdml_enum_chr(pcrFormat$rowLabel)
  col_label <- .rdml_enum_chr(pcrFormat$columnLabel)

  if (row_label == "ABC" && col_label == "123") {
    if (rows > length(LETTERS)) {
      stop("Too many rows for 'ABC' PCR format", call. = FALSE)
    }
    return(sprintf(
      "%s%02i",
      LETTERS[(n - 1L) %/% columns + 1L],
      (n - 1L) %% columns + 1L
    ))
  }

  if (row_label == "123" && col_label == "ABC") {
    if (columns > length(LETTERS)) {
      stop("Too many columns for 'ABC' PCR format", call. = FALSE)
    }
    return(sprintf(
      "%02i%s",
      (n - 1L) %/% columns + 1L,
      LETTERS[(n - 1L) %% columns + 1L]
    ))
  }

  if (row_label == "123" && col_label == "123") {
    return(sprintf(
      "r%02ic%02i",
      (n - 1L) %/% columns + 1L,
      (n - 1L) %% columns + 1L
    ))
  }

  # A1a1 and other formats are not reconstructed yet; keep the RDML id.
  id
}

# sampleType$type is target-aware in rdml7. Return the type corresponding to
# the current target, rather than assuming one type per sample.
.rdml_sample_type <- function(sample, target_id) {
  if (is.null(sample) || .rdml_is_missing(sample)) {
    return(NA_character_)
  }

  types <- .rdml_prop_list(sample, "type")
  if (!length(types)) {
    return(NA_character_)
  }

  type_keys <- .get_keys(types, "targetId")
  pos <- match(target_id, type_keys)
  if (is.na(pos)) {
    return(NA_character_)
  }

  .rdml_enum_chr(types[[pos]]$sampleType)
}

.rdml_target_dye <- function(targets, target_id) {
  target <- targets[[target_id]]
  if (is.null(target) || .rdml_is_missing(target$dyeId)) {
    return(NA_character_)
  }
  .rdml_id_chr(target$dyeId)
}

# Upsert an object in an ordinary list by an S7 key property. This is useful
# for RDML lists that are not declared as rdmlKeyedList (e.g. quantity).
.rdml_upsert_list_by_key <- function(x, value, key = "id") {
  x <- .rdml_as_list(x)
  value_key <- .get_key(value, key)
  current <- .get_keys(x, key)
  pos <- match(value_key, current)
  if (is.na(pos)) {
    x[[length(x) + 1L]] <- value
  } else {
    x[[pos]] <- value
  }
  names(x) <- NULL
  x
}
