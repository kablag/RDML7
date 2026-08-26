# Property validators and property metadata ---------------------------------
# Internal helpers used by the S7 RDML schema.

.isSingleNa <- function(x) {
  length(x) == 1L &&
    is.atomic(x) &&
    isTRUE(is.na(x))
}

classDateTimeNa <- S7::new_property(
  S7::class_any,
  validator = function(value) {
    if (.isSingleNa(value)) {
      return(NULL)
    }

    if (length(value) != 1L) {
      return("must be NA or a single date/datetime value")
    }

    parsedDatetime <- suppressWarnings(
      lubridate::ymd_hms(value, quiet = TRUE)
    )
    parsedDate <- suppressWarnings(
      lubridate::ymd(value, quiet = TRUE)
    )

    if (is.na(parsedDatetime) && is.na(parsedDate)) {
      "must be NA or pass lubridate::ymd_hms()/ymd() conversion"
    }
  },
  default = NA_character_
)

classCharacterNaNonemptySingle <- S7::new_property(
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

classCharacterNonemptySingle <- S7::new_property(
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

classFlagNa <- S7::new_property(
  S7::class_any,
  validator = function(value) {
    if (!checkmate::testFlag(value, na.ok = TRUE)) {
      "must be NA or a flag"
    }
  },
  default = NA
)

classFlag <- S7::new_property(
  S7::class_logical,
  validator = function(value) {
    if (length(value) != 1L || is.na(value)) {
      "must be a flag"
    }
  }
)

# data.table inherits data.frame; keeping the property typed as data.frame
# accepts both without a setter that is coupled to a particular property name.
classDataTable <- S7::new_property(
  S7::class_data.frame
)

classNumberNaSingle <- S7::new_property(
  S7::class_any,
  validator = function(value) {
    if (!checkmate::testNumber(value, na.ok = TRUE)) {
      "must be NA or a number"
    }
  },
  default = NA_real_
)

classNumberSingle <- S7::new_property(
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

classPositiveIntegerSingle <- S7::new_property(
  S7::class_integer,
  validator = function(value) {
    if (!checkmate::testInt(value, lower = 0)) {
      "must be a positive integer"
    }
  }
)

classIntegerNaSingle <- S7::new_property(
  S7::class_any,
  validator = function(value) {
    if (!checkmate::testInt(value, na.ok = TRUE)) {
      "must be NA or an integer"
    }
  },
  default = NA_integer_
)

classPositiveIntegerNaSingle <- S7::new_property(
  S7::class_any,
  validator = function(value) {
    if (!checkmate::testInt(value, lower = 0, na.ok = TRUE)) {
      "must be NA or a positive integer"
    }
  },
  default = NA_integer_
)

.resolveS7Class <- function(className, envir = parent.frame()) {
  cls <- get0(
    className,
    envir = envir,
    inherits = TRUE
  )

  if (is.null(cls)) {
    stop(
      "Unknown S7 class in property definition: ",
      className,
      call. = FALSE
    )
  }

  cls
}


# The class object is resolved when the property is created. This is
# important for package code: an S7 instance may have a package-qualified
# class name such as "RDML7::rdmlIdType", therefore checkmate::testClass(x,
# "rdmlIdType") is not a reliable S7 class test.
.testClass <- function(className) {
  cls <- .resolveS7Class(
    className,
    envir = parent.frame()
  )

  S7::new_property(
    S7::class_any,
    validator = function(value) {
      if (S7::S7_inherits(value, cls)) {
        return(NULL)
      }

      paste("must be a", className)
    },
    default = NA
  )
}


.testClassNa <- function(className) {
  cls <- .resolveS7Class(
    className,
    envir = parent.frame()
  )

  S7::new_property(
    S7::class_any,
    validator = function(value) {
      if (.isSingleNa(value)) {
        return(NULL)
      }

      if (S7::S7_inherits(value, cls)) {
        return(NULL)
      }

      paste("must be NA or", className)
    },
    default = NA
  )
}


.testClassList <- function(className) {
  cls <- .resolveS7Class(
    className,
    envir = parent.frame()
  )

  S7::new_property(
    S7::class_any,
    validator = function(value) {
      if (
        is.list(value) &&
        all(vapply(
          value,
          function(x) S7::S7_inherits(x, cls),
          logical(1)
        ))
      ) {
        return(NULL)
      }

      paste("must be a list of", className)
    },
    default = list()
  )
}


.testClassNaList <- function(className) {
  cls <- .resolveS7Class(
    className,
    envir = parent.frame()
  )

  S7::new_property(
    S7::class_any,
    validator = function(value) {
      if (.isSingleNa(value)) {
        return(NULL)
      }

      if (
        is.list(value) &&
        all(vapply(
          value,
          function(x) S7::S7_inherits(x, cls),
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


.testClassNaKeyedList <- function(className, key = "id") {
  cls <- .resolveS7Class(
    className,
    envir = parent.frame()
  )

  property <- S7::new_property(
    S7::class_any,
    validator = function(value) {
      if (.isSingleNa(value)) {
        return(NULL)
      }

      if (!is.list(value)) {
        return(paste("must be NA or a list of", className))
      }

      correctClass <- vapply(
        value,
        function(x) S7::S7_inherits(x, cls),
        logical(1)
      )

      if (!all(correctClass)) {
        return(paste("must be NA or a list of", className))
      }

      keys <- .getKeys(value, key)

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
        duplicatedKeys <- unique(
          keys[duplicated(keys)]
        )

        return(
          paste0(
            "duplicated ",
            key,
            ": ",
            paste(
              duplicatedKeys,
              collapse = ", "
            )
          )
        )
      }

      NULL
    },
    default = NA
  )

  attr(property, "rdmlKey") <- key
  attr(property, "rdmlElementClass") <- className

  property
}


.testClassNaIdList <- function(className) {
  .testClassNaKeyedList(
    className,
    key = "id"
  )
}

.getPropertyDefinition <- function(x, name) {
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

.propertyKey <- function(x, name) {
  property <- .getPropertyDefinition(x, name)

  if (is.null(property)) {
    return(NULL)
  }

  attr(property, "rdmlKey")
}

.propertyElementClass <- function(x, name) {
  property <- .getPropertyDefinition(x, name)

  if (is.null(property)) {
    return(NULL)
  }

  attr(property, "rdmlElementClass")
}
