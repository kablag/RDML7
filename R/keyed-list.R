# Keyed list abstraction ----------------------------------------------------
# RDML collections are physically unnamed lists. Their virtual names are
# derived from an object property such as `id` or `targetId`.

.getKey <- function(x, key = "id") {
  value <- tryCatch(
    S7::prop(x, key),
    error = function(e) NULL
  )

  if (is.null(value)) {
    return(NA_character_)
  }

  if (
    is.character(value) &&
    length(value) == 1L
  ) {
    return(value)
  }

  id <- tryCatch(
    S7::prop(value, "id"),
    error = function(e) NA_character_
  )

  if (
    is.character(id) &&
    length(id) == 1L
  ) {
    return(id)
  }

  NA_character_
}

.getKeys <- function(x, key = "id") {
  vapply(
    x,
    .getKey,
    character(1),
    key = key
  )
}

.getId <- function(x) {
  .getKey(x, "id")
}

.getIds <- function(x) {
  .getKeys(x, "id")
}

.setKeyedListData <- function(x, value) {
  names(value) <- NULL
  S7::S7_data(x) <- value
  x
}

.asReplacementList <- function(value) {
  if (S7::S7_inherits(value, rdmlKeyedList)) {
    return(S7::S7_data(value))
  }

  if (S7::S7_inherits(value)) {
    return(list(value))
  }

  if (is.list(value)) {
    return(value)
  }

  list(value)
}

#' Keyed list view used by RDML S7 objects
#'
#' Derives virtual element names from a property such as `id` or `targetId`
#' and enables nested `$` / `[[` access without storing physical list names.
#'
#' @param data List of schema objects.
#' @param key Property used as the virtual key.
#' @return An `rdmlKeyedList` S7 object.
#' @seealso `rdmlType`, `idType`
#' @export
rdmlKeyedList <- S7::new_class(
  "rdmlKeyedList",
  parent = S7::class_list,
  properties = list(
    key = S7::class_character
  ),
  constructor = function(x = list(), key = "id") {
    if (!is.list(x)) {
      stop("`x` must be a list", call. = FALSE)
    }

    if (length(key) != 1L || is.na(key) || !nzchar(key)) {
      stop("`key` must be one non-empty string", call. = FALSE)
    }

    names(x) <- NULL

    S7::new_object(x, key = key)
  },
  validator = function(self) {
    x <- S7::S7_data(self)

    if (!length(x)) {
      return(NULL)
    }

    keys <- .getKeys(x, self@key)

    errors <- character()

    if (anyNA(keys) || any(keys == "")) {
      errors <- c(
        errors,
        paste0(
          "all objects must have a non-empty ",
          self@key
        )
      )
    }

    duplicates <- unique(
      keys[ duplicated(keys) & !is.na(keys)]
    )

    if (length(duplicates)) {
      errors <- c(
        errors,
        paste0("duplicated ", self@key, ": ",
          paste(duplicates, collapse = ", ")
        )
      )
    }

    if (length(errors)) {
      errors
    }
  }
)

S7::method(names, rdmlKeyedList) <- function(x) {
  .getKeys(S7::S7_data(x), x@key)
}

S7::method(`names<-`, rdmlKeyedList) <- function(x, value) {
  stop(
    "`names` of rdmlKeyedList are derived from object keys and cannot be changed",
    call. = FALSE
  )
}

S7::method(`[[`, rdmlKeyedList) <- function(x, i, ...) {
  data <- S7::S7_data(x)

  if (is.character(i)) {
    if (length(i) != 1L) {
      stop("character index must have length 1", call. = FALSE)
    }

    pos <- match(i, names(x))

    if (is.na(pos)) {
      return(NULL)
    }

    return(data[[pos]])
  }

  data[[i, ...]]
}

S7::method(`[[<-`, rdmlKeyedList) <- function(x, i, ..., value) {
  dots <- list(...)

  if (length(dots)) {
    stop(
      "recursive indexing is not supported for rdmlKeyedList",
      call. = FALSE
    )
  }

  data <- S7::S7_data(x)

  if (is.character(i)) {
    if (
      length(i) != 1L ||
      is.na(i) ||
      !nzchar(i)
    ) {
      stop(
        "character index must be one non-empty key",
        call. = FALSE
      )
    }

    keys <- .getKeys(data, x@key)

    pos <- match(i, keys)

    # Removal -----------------------------------------------------------
    if (is.null(value)) {
      if (is.na(pos)) {
        stop(
          "Unknown ", x@key, ": '", i, "'",
          call. = FALSE
        )
      }

      data[[pos]] <- NULL

      return(.setKeyedListData(x, data))
    }

    valueKey <- .getKey(value, x@key)

    if (length(valueKey) != 1L || is.na(valueKey) || !nzchar(valueKey)) {
      stop(
        "replacement object must have a non-empty ",
        x@key,
        call. = FALSE
      )
    }

    # Existing element: a changed key is a rename ----------------------
    if (!is.na(pos)) {
      if (!identical(valueKey, i)) {
        newPos <- match(valueKey, keys)

        if (!is.na(newPos) && newPos != pos) {
          stop(
            "Cannot rename '", i,
            "' to '", valueKey,
            "': this ", x@key,
            " already exists",
            call. = FALSE
          )
        }
      }

      data[[pos]] <- value

      return(
        .setKeyedListData(x, data)
      )
    }

    # New element: index and object key must agree ---------------------
    if (!identical(valueKey, i)) {
      stop(
        "Cannot add object under key '", i,
        "': object ", x@key,
        " is '", valueKey, "'",
        call. = FALSE
      )
    }

    data[[length(data) + 1L]] <- value

    return(
      .setKeyedListData(x, data)
    )
  }

  data[[i]] <- value
  .setKeyedListData(x, data)
}

S7::method(`$`, rdmlKeyedList) <- function(x, name) {
  x[[name]]
}

S7::method(`$<-`, rdmlKeyedList) <- function(x, name, value) {
  x[[name]] <- value
  x
}

S7::method(`[`, rdmlKeyedList) <- function(x, i, ...) {
  data <- S7::S7_data(x)

  if (missing(i)) {
    return(x)
  }

  if (is.character(i)) {
    pos <- match(i, names(x))
    unknown <- is.na(pos)

    if (any(unknown)) {
      stop(
        "Unknown ",
        x@key,
        ": ",
        paste(i[unknown], collapse = ", "),
        call. = FALSE
      )
    }

    return(
      rdmlKeyedList(data[pos], key = x@key)
    )
  }

  rdmlKeyedList(data[i, ...], key = x@key)
}

S7::method(`[<-`, rdmlKeyedList) <- function(x, i, ..., value) {
  dots <- list(...)

  if (length(dots)) {
    stop(
      "multidimensional indexing is not supported for rdmlKeyedList",
      call. = FALSE
    )
  }

  data <- S7::S7_data(x)

  # x[] <- value -------------------------------------------------------
  if (missing(i)) {
    if (is.null(value)) {
      return(
        .setKeyedListData(x, list())
      )
    }

    value <- .asReplacementList(value)

    return(.setKeyedListData(x, value))
  }

  # Numeric / logical indexing ----------------------------------------
  if (!is.character(i)) {
    if (is.null(value)) {
      data[i] <- NULL
    } else {
      value <- .asReplacementList(value)
      data[i] <- value
    }

    return(.setKeyedListData(x, data))
  }

  # Character keys ----------------------------------------------------
  if (anyNA(i) || any(i == "")) {
    stop(
      "keys must be non-empty strings",
      call. = FALSE
    )
  }

  if (anyDuplicated(i)) {
    stop(
      "duplicate keys in replacement index are not allowed",
      call. = FALSE
    )
  }

  currentKeys <- .getKeys(
    data,
    x@key
  )

  # Removal ------------------------------------------------------------
  if (is.null(value)) {
    pos <- match(i, currentKeys)

    if (anyNA(pos)) {
      stop(
        "Unknown ", x@key, ": ",
        paste(
          sprintf("'%s'", i[is.na(pos)]),
          collapse = ", "
        ),
        call. = FALSE
      )
    }

    data <- data[-pos]

    return(.setKeyedListData(x, data))
  }

  # Replacement / addition --------------------------------------------
  value <- .asReplacementList(value)

  if (length(value) != length(i)) {
    stop(
      "replacement length (",
      length(value),
      ") must equal index length (",
      length(i),
      ")",
      call. = FALSE
    )
  }

  valueKeys <- .getKeys(
    value,
    x@key
  )

  badKey <- is.na(valueKeys) | valueKeys == ""

  if (any(badKey)) {
    stop(
      "all replacement objects must have a non-empty ",
      x@key,
      call. = FALSE
    )
  }

  for (k in seq_along(i)) {
    # Recompute because an earlier replacement may have renamed a key.
    currentKeys <- .getKeys(data, x@key)

    oldKey <- i[[k]]
    newKey <- valueKeys[[k]]
    pos <- match(
      oldKey,
      currentKeys
    )

    if (!is.na(pos)) {
      if (!identical(oldKey, newKey)) {
        conflictPos <- match(
          newKey,
          currentKeys
        )

        if (
          !is.na(conflictPos) &&
          conflictPos != pos
        ) {
          stop(
            "Cannot rename '",
            oldKey,
            "' to '",
            newKey,
            "': this ",
            x@key,
            " already exists",
            call. = FALSE
          )
        }
      }

      data[[pos]] <- value[[k]]
      next
    }

    if (!identical(oldKey, newKey)) {
      stop(
        "Cannot add object under key '",
        oldKey,
        "': object ",
        x@key,
        " is '",
        newKey,
        "'",
        call. = FALSE
      )
    }

    data[[length(data) + 1L]] <- value[[k]]
  }

  .setKeyedListData(x, data)
}
