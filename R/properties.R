# Property validators and property metadata ---------------------------------
# Internal helpers used by the S7 RDML schema.

.is_single_na <- function(x) {
  length(x) == 1L &&
    is.atomic(x) &&
    isTRUE(is.na(x))
}

class_datetime_na <- S7::new_property(
  S7::class_any,
  validator = function(value) {
    if (.is_single_na(value)) {
      return(NULL)
    }

    if (length(value) != 1L) {
      return("must be NA or a single date/datetime value")
    }

    parsed_datetime <- suppressWarnings(
      lubridate::ymd_hms(value, quiet = TRUE)
    )
    parsed_date <- suppressWarnings(
      lubridate::ymd(value, quiet = TRUE)
    )

    if (is.na(parsed_datetime) && is.na(parsed_date)) {
      "must be NA or pass lubridate::ymd_hms()/ymd() conversion"
    }
  },
  default = NA_character_
)

class_character_na_nonempty_single <- S7::new_property(
  S7::class_any,
  validator = function(value) {
    if (!checkmate::testString(
      value,
      min.chars = 1,
      pattern = "[^[:space:]]",
      na.ok = TRUE
    )) {
      "must be NA or a single non-empty string"
    }
  },
  default = NA_character_
)

class_character_nonempty_single <- S7::new_property(
  S7::class_character,
  validator = function(value) {
    if (
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(trimws(value))
    ) {
      "must be a single non-empty string"
    }
  }
)

class_flag_na <- S7::new_property(
  S7::class_any,
  validator = function(value) {
    if (!checkmate::testFlag(value, na.ok = TRUE)) {
      "must be NA or a flag"
    }
  },
  default = NA
)

class_flag <- S7::new_property(
  S7::class_logical,
  validator = function(value) {
    if (length(value) != 1L || is.na(value)) {
      "must be a flag"
    }
  }
)

# data.table inherits data.frame; keeping the property typed as data.frame
# accepts both without a setter that is coupled to a particular property name.
class_datatable <- S7::new_property(
  S7::class_data.frame
)

class_number_na_single <- S7::new_property(
  S7::class_any,
  validator = function(value) {
    if (!checkmate::testNumber(value, na.ok = TRUE)) {
      "must be NA or a number"
    }
  },
  default = NA_real_
)

class_number_single <- S7::new_property(
  S7::class_numeric,
  validator = function(value) {
    if (
      length(value) != 1L ||
      is.na(value)
    ) {
      "must be a number"
    }
  }
)

class_positive_integer_single <- S7::new_property(
  S7::class_integer,
  validator = function(value) {
    if (!checkmate::testInt(value, lower = 0)) {
      "must be a positive integer"
    }
  }
)

class_integer_na_single <- S7::new_property(
  S7::class_any,
  validator = function(value) {
    if (!checkmate::testInt(value, na.ok = TRUE)) {
      "must be NA or an integer"
    }
  },
  default = NA_integer_
)

class_positive_integer_na_single <- S7::new_property(
  S7::class_any,
  validator = function(value) {
    if (!checkmate::testInt(value, lower = 0, na.ok = TRUE)) {
      "must be NA or a positive integer"
    }
  },
  default = NA_integer_
)

test_class <- function(className) {
  S7::new_property(
    S7::class_any,
    validator = function(value) {
      if (
        checkmate::testClass(value, className) &&
        length(value) == 1L
      ) {
        return(NULL)
      }

      paste("must be a", className)
    },
    default = NA
  )
}

test_class_na <- function(className) {
  S7::new_property(
    S7::class_any,
    validator = function(value) {
      if (.is_single_na(value)) {
        return(NULL)
      }

      if (
        checkmate::testClass(value, className) &&
        length(value) == 1L
      ) {
        return(NULL)
      }

      paste("must be NA or", className)
    },
    default = NA
  )
}

test_class_list <- function(className) {
  S7::new_property(
    S7::class_any,
    validator = function(value) {
      if (
        is.list(value) &&
        all(vapply(
          value,
          function(x) checkmate::testClass(x, className),
          logical(1)
        ))
      ) {
        return(NULL)
      }

      paste("must be a list of", className)
    },
    default = NA
  )
}

test_class_na_list <- function(className) {
  S7::new_property(
    S7::class_any,
    validator = function(value) {
      if (.is_single_na(value)) {
        return(NULL)
      }

      if (
        is.list(value) &&
        all(vapply(
          value,
          function(x) checkmate::testClass(x, className),
          logical(1)
        ))
      ) {
        return(NULL)
      }

      paste("must be NA or a list of", className)
    },
    default = NA
  )
}

test_class_na_keyed_list <- function(className, key = "id") {
  property <- S7::new_property(
    S7::class_any,
    validator = function(value) {
      if (.is_single_na(value)) {
        return(NULL)
      }

      if (!is.list(value)) {
        return(paste("must be NA or a list of", className))
      }

      correct_class <- vapply(
        value,
        function(x) checkmate::testClass(x, className),
        logical(1)
      )

      if (!all(correct_class)) {
        return(paste("must be NA or a list of", className))
      }

      keys <- .get_keys(value, key)

      if (anyNA(keys) || any(keys == "")) {
        return(
          paste0(
            className,
            " objects must have non-empty ",
            key
          )
        )
      }

      if (anyDuplicated(keys)) {
        duplicated_keys <- unique(keys[duplicated(keys)])
        return(
          paste0(
            "duplicated ",
            key,
            ": ",
            paste(duplicated_keys, collapse = ", ")
          )
        )
      }

      NULL
    },
    default = NA
  )

  attr(property, "rdml_key") <- key
  attr(property, "rdml_element_class") <- className

  property
}

test_class_na_id_list <- function(className) {
  test_class_na_keyed_list(
    className,
    key = "id"
  )
}

.get_property_definition <- function(x, name) {
  cls <- S7::S7_class(x)

  repeat {
    properties <- cls@properties

    if (name %in% names(properties)) {
      return(properties[[name]])
    }

    parent <- cls@parent

    if (is.null(parent)) {
      return(NULL)
    }

    cls <- parent
  }
}

.property_key <- function(x, name) {
  property <- .get_property_definition(x, name)

  if (is.null(property)) {
    return(NULL)
  }

  attr(property, "rdml_key")
}

.property_element_class <- function(x, name) {
  property <- .get_property_definition(x, name)

  if (is.null(property)) {
    return(NULL)
  }

  attr(property, "rdml_element_class")
}
