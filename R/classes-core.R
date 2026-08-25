# Core S7 types ------------------------------------------------------------

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

#' @export
print.rdmlEnum <- function(x, ...) {
  cat(class(x)[1L], "::", x@value, sep = "")
  invisible(x)
}

#' @export
as.character.rdmlEnum <- function(x, ...) {
  x@value
}

new_enum_class <- function(enum_class, variants) {
  S7::new_class(
    enum_class,
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

#' @export
print.idType <- function(x, ...) {
  cat(x@id)
  invisible(x)
}

#' @export
as.character.idType <- function(x, ...) {
  as.character(x@id)
}

class_id <- S7::new_property(
  S7::class_any,
  validator = function(value) {
    if (!S7::S7_inherits(value, idType)) {
      "must be an idType"
    }
  }
)

#' RDML identifier reference
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

# The default S7 `$` accessor is extended so list properties declared with
# rdml_key metadata are exposed as transient rdmlKeyedList wrappers.
#' @export
`$.rdmlBaseType` <- function(x, name) {
  if (typeof(x) %in% c("list", "environment")) {
    return(NextMethod())
  }

  value <- S7::prop(x, name)
  key <- .property_key(x, name)

  if (
    !is.null(key) &&
    is.list(value)
  ) {
    return(
      rdmlKeyedList(
        value,
        key = key
      )
    )
  }

  value
}

#' @export
`$<-.rdmlBaseType` <- function(x, name, value) {
  key <- .property_key(x, name)

  if (
    !is.null(key) &&
    S7::S7_inherits(value, rdmlKeyedList)
  ) {
    value <- S7::S7_data(value)
  }

  S7::prop(x, name) <- value
  x
}
