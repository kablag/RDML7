.get_id <- function(x) {
  
  id <- tryCatch(
    prop(x, "id"),
    error = function(e) NULL
  )
  
  if (is.null(id)) {
    return(NA_character_)
  }
  
  # На случай если id уже character
  if (
    is.character(id) &&
    length(id) == 1L
  ) {
    return(id)
  }
  
  # idType -> @id
  value <- tryCatch(
    prop(id, "id"),
    error = function(e) NA_character_
  )
  
  if (
    is.character(value) &&
    length(value) == 1L
  ) {
    return(value)
  }
  
  NA_character_
}

.get_ids <- function(x) {
  vapply(
    x,
    .get_id,
    character(1)
  )
}

.set_id_list_data <- function(x, value) {
  # Физических names быть не должно
  names(value) <- NULL
  
  S7_data(x) <- value
  
  x
}


.as_replacement_list <- function(value) {
  
  if (S7_inherits(value, rdmlIdList)) {
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

.rdml_id_list_properties <- c(
  "experiment"
)

rdmlIdList <- new_class(
  "rdmlIdList",
  
  parent = class_list,
  
  constructor = function(x = list()) {
    
    if (!is.list(x)) {
      stop("`x` must be a list", call. = FALSE)
    }
    
    # ВАЖНО:
    # никаких физических names() не храним
    names(x) <- NULL
    
    new_object(x)
  },
  
  validator = function(self) {
    
    x <- S7_data(self)
    
    if (!length(x)) {
      return(NULL)
    }
    
    ids <- vapply(
      x,
      .get_id,
      character(1)
    )
    
    errors <- character()
    
    if (anyNA(ids) || any(ids == "")) {
      errors <- c(
        errors,
        "all objects must have a non-empty id"
      )
    }
    
    duplicates <- unique(
      ids[duplicated(ids) & !is.na(ids)]
    )
    
    if (length(duplicates)) {
      errors <- c(
        errors,
        paste0(
          "duplicated id: ",
          paste(duplicates, collapse = ", ")
        )
      )
    }
    
    if (length(errors)) {
      errors
    }
  }
)


method(names, rdmlIdList) <- function(x) {
  
  vapply(
    S7_data(x),
    .get_id,
    character(1)
  )
}

method(`names<-`, rdmlIdList) <- function(x, value) {
  stop(
    "`names` of rdmlIdList are derived from object ids and cannot be changed",
    call. = FALSE
  )
}

method(`[[`, rdmlIdList) <- function(x, i, ...) {
  
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

method(`[[<-`, rdmlIdList) <- function(x, i, ..., value) {
  
  dots <- list(...)
  
  if (length(dots)) {
    stop(
      "recursive indexing is not supported for rdmlIdList",
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
    
    ids <- .get_ids(data)
    pos <- match(i, ids)
    
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
    value_id <- .get_id(value)
    
    if (
      length(value_id) != 1L ||
      is.na(value_id) ||
      !nzchar(value_id)
    ) {
      stop(
        "replacement object must have a non-empty id",
        call. = FALSE
      )
    }
    
    if (!identical(value_id, i)) {
      stop(
        "Index id ('", i,
        "') does not match object id ('",
        value_id, "')",
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

method(`$`, rdmlIdList) <- function(x, name) {
  x[[name]]
}

method(`$<-`, rdmlIdList) <- function(x, name, value) {
  x[[name]] <- value
  x
}

method(`[`, rdmlIdList) <- function(x, i, ...) {
  
  data <- S7_data(x)
  
  if (missing(i)) {
    return(x)
  }
  
  if (is.character(i)) {
    
    pos <- match(i, names(x))
    
    unknown <- is.na(pos)
    
    if (any(unknown)) {
      stop(
        "Unknown id: ",
        paste(i[unknown], collapse = ", "),
        call. = FALSE
      )
    }
    
    return(
      rdmlIdList(data[pos])
    )
  }
  
  rdmlIdList(
    data[i, ...]
  )
}

method(`[<-`, rdmlIdList) <- function(x, i, ..., value) {
  
  dots <- list(...)
  
  if (length(dots)) {
    stop(
      "multidimensional indexing is not supported for rdmlIdList",
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
      "ids must be non-empty strings",
      call. = FALSE
    )
  }
  
  if (anyDuplicated(i)) {
    stop(
      "duplicate ids in replacement index are not allowed",
      call. = FALSE
    )
  }
  
  current_ids <- .get_ids(data)
  
  # Удаляем несколько объектов
  if (is.null(value)) {
    
    pos <- match(i, current_ids)
    
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
  
  value_ids <- .get_ids(value)
  
  bad_id <- is.na(value_ids) | value_ids == ""
  
  if (any(bad_id)) {
    stop(
      "all replacement objects must have a non-empty id",
      call. = FALSE
    )
  }
  
  mismatch <- value_ids != i
  
  if (any(mismatch)) {
    k <- which(mismatch)[1L]
    
    stop(
      "Index id ('", i[[k]],
      "') does not match object id ('",
      value_ids[[k]], "')",
      call. = FALSE
    )
  }
  
  # Последовательно заменяем существующие
  # или добавляем новые
  for (k in seq_along(i)) {
    
    current_ids <- .get_ids(data)
    
    pos <- match(i[[k]], current_ids)
    
    if (is.na(pos)) {
      data[[length(data) + 1L]] <- value[[k]]
    } else {
      data[[pos]] <- value[[k]]
    }
  }
  
  .set_id_list_data(x, data)
}


.has_id <- function(x) {
  id <- .get_id(x)
  
  length(id) == 1L &&
    !is.na(id) &&
    nzchar(id)
}


.as_id_list <- function(x) {
  
  if (!is.list(x) || !length(x)) {
    return(x)
  }
  
  has_id <- vapply(
    x,
    .has_id,
    logical(1)
  )
  
  # Превращаем в rdmlIdList только если
  # ВСЕ элементы имеют id
  if (all(has_id)) {
    rdmlIdList(x)
  } else {
    x
  }
}


has_id <- function(x, id) {
  id %in% names(x)
}

ids <- function(x) {
  UseMethod("ids")
}

ids.rdmlIdList <- function(x) {
  names(x)
}

.is_single_na <- function(x) {
  length(x) == 1L &&
    is.atomic(x) &&
    is.na(x)
}
