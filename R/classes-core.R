# Core S7 types ------------------------------------------------------------

.rdmlSchemaVersion <- "1.3"

# Abstract enum base.
rdmlEnum <- S7::new_class(
  "rdmlEnum",
  properties = list(
    value = S7::class_character,
    variants = S7::class_character
  ),
  validator = function(self) {
    if (length(self@value) != 1L) {
      "enum values must have length 1"
    } else if (!(self@value %in% self@variants)) {
      sprintf(
        "enum value must be one of: %s",
        paste(self@variants, collapse = ", ")
      )
    }
  },
  abstract = TRUE
)

S7::method(print, rdmlEnum) <- function(x, ...) {
  cat(class(x)[1L], "::", x@value, sep = "")
  invisible(x)
}

S7::method(as.character, rdmlEnum) <- function(x, ...) {
  x@value
}

.newEnumClass <- function(enumClass, variants) {
  S7::new_class(
    enumClass,
    parent = rdmlEnum,
    properties = list(
      value = S7::class_character,
      variants = S7::new_property(
        S7::class_character,
        default = variants
      )
    ),
    constructor = function(value = "") {
      S7::new_object(
        S7::S7_object(),
        value = value,
        variants = variants
      )
    }
  )
}

# idType ------------------------------------------------------------------

#' RDML identifier
#'
#' Stores one character identifier used as a schema key. `as.character()`
#' extracts the identifier and `print()` displays it directly.
#'
#' @seealso `idReferenceType`, `rdmlKeyedList`, `rdmlType`
#' @export
idType <- S7::new_class(
  "idType",
  properties = list(
    id = S7::class_character
  ),
  validator = function(self) {
    if (length(self@id) != 1L) {
      "id value must have length 1"
    }
  }
)

S7::method(print, idType) <- function(x, ...) {
  cat(x@id)
  invisible(x)
}

S7::method(as.character, idType) <- function(x, ...) {
  as.character(x@id)
}

classId <- S7::new_property(
  S7::class_any,
  validator = function(value) {
    if (!S7::S7_inherits(value, idType)) {
      "must be an idType"
    }
  }
)

#' RDML identifier reference
#'
#' Extends `idType` for references between RDML elements, for example sample,
#' target, dye, documentation, and thermal-program references.
#'
#' @seealso `idType`, `validateRDML`
#' @export
idReferenceType <- S7::new_class(
  "idReferenceType",
  parent = idType
)

# Base type ---------------------------------------------------------------

rdmlBaseType <- S7::new_class(
  "rdmlBaseType",
  abstract = TRUE
)

S7::method(names, rdmlBaseType) <- function(x) {
  S7::prop_names(x)
}

# Public convenience accessor ------------------------------------------------
#
# RDML7 intentionally supports nested `$` navigation for S7 schema objects.
# Keyed list properties are exposed as transient rdmlKeyedList wrappers.
S7::method(`$`, rdmlBaseType) <- function(x, name) {
  value <- S7::prop(x, name)
  key <- .propertyKey(x, name)

  if (!is.null(key) && is.list(value)) {
    return(rdmlKeyedList(value, key = key))
  }

  value
}

S7::method(`$<-`, rdmlBaseType) <- function(x, name, value) {
  key <- .propertyKey(x, name)

  if (!is.null(key) && S7::S7_inherits(value, rdmlKeyedList)) {
    value <- S7::S7_data(value)
  }

  S7::prop(x, name) <- value
  x
}
