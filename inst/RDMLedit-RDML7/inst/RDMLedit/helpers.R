# RDMLedit helpers for RDML7
#
# This file deliberately does not modify S7 classes at run time.
# All editor-specific behaviour is implemented as ordinary functions.

editor_s7_props <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  
  tryCatch(
    S7::props(x),
    error = function(e) NULL
  )
}

editor_is_s7 <- function(x) {
  !is.null(
    editor_s7_props(x)
  )
}

editor_has_prop <- function(x, name) {
  properties <- editor_s7_props(x)
  
  !is.null(properties) &&
    name %in% names(properties)
}

editor_empty_to_null <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NULL)
  }
  
  if (length(x) == 1L && (is.na(x) || identical(x, ""))) {
    return(NULL)
  }
  
  x
}

editor_text_value <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }
  
  x <- as.character(x[[1L]])
  
  if (!nzchar(x)) {
    return(NA_character_)
  }
  
  x
}

editor_display <- function(x, default = "") {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) {
    return(default)
  }
  
  paste(as.character(x), collapse = ";")
}

editor_clone <- function(x) {
  unserialize(serialize(x, NULL))
}

editor_id_chr <- function(x) {
  if (is.null(x)) {
    return("")
  }
  
  if (editor_has_prop(x, "id")) {
    return(
      editor_id_chr(
        editor_get_prop(
          x,
          "id"
        )
      )
    )
  }
  
  if (length(x) == 0L || all(is.na(x))) {
    return("")
  }
  
  as.character(x[[1L]])
}

# Read an S7 property through RDML7's public `$` accessor.
#
# This is important for keyed schema collections: RDML7 stores the underlying
# property as an unnamed list, while `$` exposes it as an rdmlKeyedList whose
# names are computed from the element key (`id`, `targetId`, `publisher`, ...).
editor_get_prop <- function(x, property, default = NULL) {
  if (
    is.null(x) ||
    !editor_has_prop(x, property)
  ) {
    return(default)
  }
  
  value <- tryCatch(
    do.call(
      "$",
      list(
        x,
        property
      )
    ),
    error = function(e) {
      # Fallback for an S7 object that does not implement RDML7's `$`.
      S7::prop(
        x,
        property
      )
    }
  )
  
  if (is.null(value)) {
    default
  } else {
    value
  }
}


# Write through RDML7's public `$<-` accessor where possible.
#
# For keyed collections this lets RDML7 unwrap/validate rdmlKeyedList objects
# instead of storing editor-specific names in the underlying schema list.
editor_set_prop <- function(x, property, value) {
  tryCatch(
    do.call(
      "$<-",
      list(
        x,
        property,
        value
      )
    ),
    error = function(e) {
      S7::prop(
        x,
        property
      ) <- value
      
      x
    }
  )
}


editor_collection <- function(x, property) {
  value <- editor_get_prop(
    x,
    property,
    NULL
  )
  
  if (
    is.null(value) ||
    !length(value)
  ) {
    return(value)
  }
  
  value
}


editor_collection_names <- function(x, property) {
  value <- editor_collection(
    x,
    property
  )
  
  if (
    is.null(value) ||
    !length(value)
  ) {
    return(character())
  }
  
  nms <- names(value)
  
  if (is.null(nms)) {
    character()
  } else {
    nms
  }
}


editor_get_collection_item <- function(
    x,
    property,
    key) {
  
  if (
    is.null(key) ||
    length(key) != 1L ||
    is.na(key) ||
    !nzchar(key)
  ) {
    return(NULL)
  }
  
  collection <- editor_collection(
    x,
    property
  )
  
  if (
    is.null(collection) ||
    !length(collection)
  ) {
    return(NULL)
  }
  
  keys <- names(collection)
  
  if (
    is.null(keys) ||
    !(key %in% keys)
  ) {
    return(NULL)
  }
  
  collection[[key]]
}


editor_set_collection_item <- function(
    x,
    property,
    old_key = NULL,
    new_key,
    object) {
  
  stopifnot(
    is.character(property),
    length(property) == 1L,
    !is.na(property),
    nzchar(property),
    is.character(new_key),
    length(new_key) == 1L,
    !is.na(new_key),
    nzchar(new_key)
  )
  
  collection <- editor_collection(
    x,
    property
  )
  
  # RDML7's `$` accessor normally returns an empty rdmlKeyedList for keyed
  # properties. Keep a defensive fallback for a genuinely NULL property.
  if (is.null(collection)) {
    collection <- list()
  }
  
  keys <- names(collection)
  
  if (
    !is.null(old_key) &&
    length(old_key) == 1L &&
    !is.na(old_key) &&
    nzchar(old_key) &&
    !is.null(keys) &&
    old_key %in% keys &&
    old_key != new_key
  ) {
    collection[[old_key]] <- NULL
  }
  
  # For rdmlKeyedList this uses its `[[<-` method. The list's visible key is
  # derived from the object itself; we never assign physical names().
  collection[[new_key]] <- object
  
  editor_set_prop(
    x,
    property,
    collection
  )
}


editor_remove_collection_item <- function(
    x,
    property,
    key) {
  
  if (
    is.null(key) ||
    length(key) != 1L ||
    is.na(key) ||
    !nzchar(key)
  ) {
    return(x)
  }
  
  collection <- editor_collection(
    x,
    property
  )
  
  if (
    is.null(collection) ||
    !length(collection)
  ) {
    return(x)
  }
  
  keys <- names(collection)
  
  if (
    is.null(keys) ||
    !(key %in% keys)
  ) {
    return(x)
  }
  
  collection[[key]] <- NULL
  
  editor_set_prop(
    x,
    property,
    collection
  )
}


editor_construct_or_update <- function(
    existing,
    constructor,
    values) {
  
  values <- values[!vapply(values, is.null, logical(1))]
  
  if (is.null(existing)) {
    return(do.call(constructor, values))
  }
  
  do.call(
    S7::set_props,
    c(
      list(existing),
      values
    )
  )
}

editor_parse_numeric <- function(x) {
  x <- editor_empty_to_null(x)
  
  if (is.null(x)) {
    return(NULL)
  }
  
  out <- suppressWarnings(as.numeric(x))
  
  if (anyNA(out)) {
    stop(
      sprintf(
        "Expected numeric value, got: %s",
        paste(x, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  out
}

editor_parse_logical <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NULL)
  }
  
  as.logical(x)
}

editor_path_text <- function(x) {
  paste(
    capture.output(
      str(
        S7::props(x),
        max.level = 4L,
        give.attr = FALSE
      )
    ),
    collapse = "\n"
  )
}

editor_summary_text <- function(x) {
  if (is.null(x)) {
    return("")
  }
  
  paste(
    capture.output(summary(x)),
    collapse = "\n"
  )
}

editor_curve_long <- function(x, dp_type = c("adp", "mdp")) {
  dp_type <- match.arg(dp_type)
  
  out <- tryCatch(
    RDML7::getFData(
      x,
      longTable = TRUE,
      dpType = dp_type
    ),
    error = function(e) e
  )
  
  if (inherits(out, "error")) {
    stop(
      sprintf(
        "getFData(dpType = %s) failed: %s",
        dp_type,
        conditionMessage(out)
      ),
      call. = FALSE
    )
  }
  
  out <- as.data.frame(out)
  
  # Canonical long-table form.
  coordinate_name <- if (dp_type == "adp") "cyc" else "tmp"
  
  if (
    coordinate_name %in% names(out) &&
    "fluor" %in% names(out)
  ) {
    out$.coordinate <- as.numeric(out[[coordinate_name]])
    out$.fluor <- as.numeric(out$fluor)
    return(out)
  }
  
  # Alternative representation: a list-column named adp/mdp.
  if (dp_type %in% names(out)) {
    rows <- vector("list", nrow(out))
    
    for (i in seq_len(nrow(out))) {
      curve <- out[[dp_type]][[i]]
      
      if (
        is.null(curve) ||
        !editor_is_s7(curve) ||
        !editor_has_prop(curve, "fpoints")
      ) {
        next
      }
      
      fpoints <- S7::prop(curve, "fpoints")
      if (is.null(fpoints)) {
        next
      }
      
      fpoints <- as.data.frame(fpoints)
      if (
        !(coordinate_name %in% names(fpoints)) ||
        !("fluor" %in% names(fpoints))
      ) {
        next
      }
      
      meta <- out[
        rep(i, nrow(fpoints)),
        setdiff(names(out), dp_type),
        drop = FALSE
      ]
      
      meta$.coordinate <- as.numeric(fpoints[[coordinate_name]])
      meta$.fluor <- as.numeric(fpoints$fluor)
      rows[[i]] <- meta
    }
    
    rows <- Filter(Negate(is.null), rows)
    
    if (!length(rows)) {
      return(data.frame())
    }
    
    return(do.call(rbind, rows))
  }
  
  stop(
    sprintf(
      "Could not identify %s curve columns returned by getFData(). Columns: %s",
      dp_type,
      paste(names(out), collapse = ", ")
    ),
    call. = FALSE
  )
}

editor_curve_label <- function(data) {
  candidates <- c(
    "position",
    "reactId",
    "react.id",
    "sample",
    "target"
  )
  
  cols <- intersect(candidates, names(data))
  
  if (!length(cols)) {
    return(rep("curve", nrow(data)))
  }
  
  apply(
    data[, cols, drop = FALSE],
    1L,
    function(row) {
      paste(
        row[nzchar(as.character(row))],
        collapse = " / "
      )
    }
  )
}

editor_save_rdml <- function(x, file) {
  RDML7::writeRDML(
    x,
    file
  )
  
  invisible(file)
}



