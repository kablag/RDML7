.get_key <- function(x, key = "id") {
  
  value <- tryCatch(
    prop(x, key),
    error = function(e) NULL
  )
  
  if (is.null(value)) {
    return(NA_character_)
  }
  
  # Если ключ уже character
  if (
    is.character(value) &&
    length(value) == 1L
  ) {
    return(value)
  }
  
  # idType / idReferenceType -> @id
  id <- tryCatch(
    prop(value, "id"),
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


.get_keys <- function(x, key = "id") {
  vapply(
    x,
    .get_key,
    character(1),
    key = key
  )
}

.get_id <- function(x) {
  .get_key(x, "id")
}

.get_ids <- function(x) {
  .get_keys(x, "id")
}

.set_id_list_data <- function(x, value) {
  # Физических names быть не должно
  names(value) <- NULL
  
  S7_data(x) <- value
  
  x
}


.as_replacement_list <- function(value) {
  
  if (S7_inherits(value, rdmlKeyedList)) {
    return(S7_data(value))
  }
  
  # Один S7-объект = один элемент
  if (S7_inherits(value)) {
    return(list(value))
  }
  
  if (is.list(value)) {
    return(value)
  }
  
  list(value)
}


rdmlKeyedList <- new_class(
  "rdmlKeyedList",
  
  parent = class_list,
  
  properties = list(
    key = class_character
  ),
  
  constructor = function(x = list(), key = "id") {
    
    if (!is.list(x)) {
      stop("`x` must be a list", call. = FALSE)
    }
    
    names(x) <- NULL
    
    new_object(
      x,
      key = key
    )
  },
  
  validator = function(self) {
    
    x <- S7_data(self)
    
    if (!length(x)) {
      return(NULL)
    }
    
    keys <- .get_keys(
      x,
      self@key
    )
    
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
      keys[
        duplicated(keys) &
          !is.na(keys)
      ]
    )
    
    if (length(duplicates)) {
      errors <- c(
        errors,
        paste0(
          "duplicated ",
          self@key,
          ": ",
          paste(
            duplicates,
            collapse = ", "
          )
        )
      )
    }
    
    if (length(errors)) {
      errors
    }
  }
)

method(names, rdmlKeyedList) <- function(x) {
  .get_keys(
    S7_data(x),
    x@key
  )
}

method(`names<-`, rdmlKeyedList) <- function(x, value) {
  stop(
    "`names` of rdmlKeyedList are derived from object keys and cannot be changed",
    call. = FALSE
  )
}

method(`[[`, rdmlKeyedList) <- function(x, i, ...) {
  
  data <- S7_data(x)
  
  if (is.character(i)) {
    
    if (length(i) != 1L) {
      stop(
        "character index must have length 1",
        call. = FALSE
      )
    }
    
    pos <- match(i, names(x))
    
    # Поведение как у обычного list[["unknown"]]
    if (is.na(pos)) {
      return(NULL)
    }
    
    return(data[[pos]])
  }
  
  data[[i, ...]]
}

method(`[[<-`, rdmlKeyedList) <- function(x, i, ..., value) {
  
  dots <- list(...)
  
  if (length(dots)) {
    stop(
      "recursive indexing is not supported for rdmlKeyedList",
      call. = FALSE
    )
  }
  
  data <- S7_data(x)
  
  # -------------------------
  # Индекс по id
  # -------------------------
  
  if (is.character(i)) {
    
    if (
      length(i) != 1L ||
      is.na(i) ||
      !nzchar(i)
    ) {
      stop(
        "character index must be one non-empty id",
        call. = FALSE
      )
    }
    
    keys <- .get_keys(
      data,
      x@key
    )
    pos <- match(i, keys)
    
    # Удаление
    if (is.null(value)) {
      
      if (is.na(pos)) {
        stop(
          "Unknown id: '", i, "'",
          call. = FALSE
        )
      }
      
      data[[pos]] <- NULL
      
      return(
        .set_id_list_data(x, data)
      )
    }
    
    # Проверяем id нового объекта
    value_key <- .get_key(
      value,
      x@key
    )
    
    if (
      length(value_key) != 1L ||
      is.na(value_key) ||
      !nzchar(value_key)
    ) {
      stop(
        "replacement object must have a non-empty id",
        call. = FALSE
      )
    }
    
    if (!identical(value_key, i)) {
      stop(
        "Index key ('", i,
        "') does not match object ",
        x@key,
        " ('", value_key, "')",
        call. = FALSE
      )
    }
    
    # Замена либо добавление
    if (is.na(pos)) {
      data[[length(data) + 1L]] <- value
    } else {
      data[[pos]] <- value
    }
    
    return(
      .set_id_list_data(x, data)
    )
  }
  
  # -------------------------
  # Обычный числовой индекс
  # -------------------------
  
  data[[i]] <- value
  
  .set_id_list_data(x, data)
}

method(`$`, rdmlKeyedList) <- function(x, name) {
  x[[name]]
}

method(`$<-`, rdmlKeyedList) <- function(x, name, value) {
  x[[name]] <- value
  x
}

method(`[`, rdmlKeyedList) <- function(x, i, ...) {
  
  data <- S7_data(x)
  
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
      rdmlKeyedList(
        data[pos],
        key = x@key
      )
    )
  }
  
  rdmlKeyedList(
    data[i, ...],
    key = x@key
  )
}

method(`[<-`, rdmlKeyedList) <- function(x, i, ..., value) {
  
  dots <- list(...)
  
  if (length(dots)) {
    stop(
      "multidimensional indexing is not supported for rdmlKeyedList",
      call. = FALSE
    )
  }
  
  data <- S7_data(x)
  
  # x[] <- ...
  if (missing(i)) {
    
    if (is.null(value)) {
      return(
        .set_id_list_data(x, list())
      )
    }
    
    value <- .as_replacement_list(value)
    
    return(
      .set_id_list_data(x, value)
    )
  }
  
  # -------------------------
  # Обычные числовые индексы
  # -------------------------
  
  if (!is.character(i)) {
    
    if (is.null(value)) {
      data[i] <- NULL
    } else {
      value <- .as_replacement_list(value)
      data[i] <- value
    }
    
    return(
      .set_id_list_data(x, data)
    )
  }
  
  # -------------------------
  # Индексы по id
  # -------------------------
  
  if (
    anyNA(i) ||
    any(i == "")
  ) {
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
  
  current_keys <- .get_keys(data)
  
  # Удаляем несколько объектов
  if (is.null(value)) {
    
    pos <- match(i, current_keys)
    
    if (anyNA(pos)) {
      stop(
        "Unknown id: ",
        paste(
          sprintf("'%s'", i[is.na(pos)]),
          collapse = ", "
        ),
        call. = FALSE
      )
    }
    
    data <- data[-pos]
    
    return(
      .set_id_list_data(x, data)
    )
  }
  
  value <- .as_replacement_list(value)
  
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
  
  value_ids <- .get_keys(
    value,
    x@key
  )
  
  bad_id <- is.na(value_keys) | value_keys == ""
  
  if (any(bad_id)) {
    stop(
      "all replacement objects must have a non-empty id",
      call. = FALSE
    )
  }
  
  mismatch <- value_keys != i
  
  if (any(mismatch)) {
    k <- which(mismatch)[1L]
    
    stop(
      "Index id ('", i[[k]],
      "') does not match object id ('",
      value_keys[[k]], "')",
      call. = FALSE
    )
  }
  
  # Последовательно заменяем существующие
  # или добавляем новые
  for (k in seq_along(i)) {
    
    current_keys <- .get_keys(data)
    
    pos <- match(i[[k]], current_keys)
    
    if (is.na(pos)) {
      data[[length(data) + 1L]] <- value[[k]]
    } else {
      data[[pos]] <- value[[k]]
    }
  }
  
  .set_id_list_data(x, data)
}


# .has_id <- function(x) {
#   id <- .get_id(x)
#   
#   length(id) == 1L &&
#     !is.na(id) &&
#     nzchar(id)
# }
# 
# 
# .as_id_list <- function(x) {
#   
#   if (!is.list(x) || !length(x)) {
#     return(x)
#   }
#   
#   has_id <- vapply(
#     x,
#     .has_id,
#     logical(1)
#   )
#   
#   # Превращаем в rdmlKeyedList только если
#   # ВСЕ элементы имеют id
#   if (all(has_id)) {
#     rdmlKeyedList(x)
#   } else {
#     x
#   }
# }


has_id <- function(x, id) {
  id %in% names(x)
}

keys <- function(x) {
  UseMethod("keys")
}

keys.rdmlKeyedList <- function(x) {
  names(x)
}

.is_single_na <- function(x) {
  length(x) == 1L &&
    is.atomic(x) &&
    is.na(x)
}

.get_property_definition <- function(x, name) {
  cls <- S7_class(x)
  
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