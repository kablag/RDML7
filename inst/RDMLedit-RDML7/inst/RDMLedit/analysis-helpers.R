# Full qPCR / melting support for RDML7 Editor
#
# All preprocessing state is kept by the Shiny session. RDML7 S7 classes are
# never modified at run time.

editor_prop <- function(x, name, default = NULL) {
  editor_get_prop(
    x,
    name,
    default
  )
}


editor_list_prop <- function(x, name) {
  value <- editor_get_prop(
    x,
    name,
    list()
  )
  
  if (
    is.null(value) ||
    !length(value)
  ) {
    return(list())
  }
  
  value
}


# Keep the names used by server-full.R, but route all operations through the
# single keyed-list-aware implementation in helpers.R.
editor_collection_get <- function(x, property, key) {
  editor_get_collection_item(
    x,
    property,
    key
  )
}


editor_collection_put <- function(
    x,
    property,
    old_key,
    new_key,
    object) {
  
  editor_set_collection_item(
    x,
    property,
    old_key,
    new_key,
    object
  )
}


editor_collection_drop <- function(x, property, key) {
  editor_remove_collection_item(
    x,
    property,
    key
  )
}


editor_get_run <- function(x, exp_id, run_id) {
  experiment <- editor_collection_get(
    x,
    "experiment",
    exp_id
  )
  
  editor_collection_get(
    experiment,
    "run",
    run_id
  )
}

editor_get_react <- function(x, exp_id, run_id, react_id) {
  run <- editor_get_run(
    x,
    exp_id,
    run_id
  )
  
  editor_collection_get(
    run,
    "react",
    react_id
  )
}

editor_get_data <- function(
    x,
    exp_id,
    run_id,
    react_id,
    target_id) {
  
  react <- editor_get_react(
    x,
    exp_id,
    run_id,
    react_id
  )
  
  editor_collection_get(
    react,
    "data",
    target_id
  )
}

editor_set_experiment <- function(
    x,
    old_exp_id,
    new_exp_id,
    experiment) {
  
  editor_collection_put(
    x,
    "experiment",
    old_exp_id,
    new_exp_id,
    experiment
  )
}

editor_set_run <- function(
    x,
    exp_id,
    old_run_id,
    new_run_id,
    run) {
  
  experiment <- editor_collection_get(
    x,
    "experiment",
    exp_id
  )
  
  experiment <- editor_collection_put(
    experiment,
    "run",
    old_run_id,
    new_run_id,
    run
  )
  
  editor_set_experiment(
    x,
    exp_id,
    exp_id,
    experiment
  )
}

editor_set_react <- function(
    x,
    exp_id,
    run_id,
    old_react_id,
    new_react_id,
    react) {
  
  run <- editor_get_run(
    x,
    exp_id,
    run_id
  )
  
  run <- editor_collection_put(
    run,
    "react",
    old_react_id,
    new_react_id,
    react
  )
  
  editor_set_run(
    x,
    exp_id,
    run_id,
    run_id,
    run
  )
}

editor_set_data <- function(
    x,
    exp_id,
    run_id,
    react_id,
    old_target_id,
    new_target_id,
    data_obj) {
  
  react <- editor_get_react(
    x,
    exp_id,
    run_id,
    react_id
  )
  
  react <- editor_collection_put(
    react,
    "data",
    old_target_id,
    new_target_id,
    data_obj
  )
  
  editor_set_react(
    x,
    exp_id,
    run_id,
    react_id,
    react_id,
    react
  )
}

editor_remove_experiment <- function(x, exp_id) {
  editor_collection_drop(
    x,
    "experiment",
    exp_id
  )
}

editor_remove_run <- function(x, exp_id, run_id) {
  experiment <- editor_collection_get(
    x,
    "experiment",
    exp_id
  )
  
  experiment <- editor_collection_drop(
    experiment,
    "run",
    run_id
  )
  
  editor_set_experiment(
    x,
    exp_id,
    exp_id,
    experiment
  )
}

editor_remove_react <- function(x, exp_id, run_id, react_id) {
  run <- editor_get_run(
    x,
    exp_id,
    run_id
  )
  
  run <- editor_collection_drop(
    run,
    "react",
    react_id
  )
  
  editor_set_run(
    x,
    exp_id,
    run_id,
    run_id,
    run
  )
}

editor_remove_data <- function(
    x,
    exp_id,
    run_id,
    react_id,
    target_id) {
  
  react <- editor_get_react(
    x,
    exp_id,
    run_id,
    react_id
  )
  
  react <- editor_collection_drop(
    react,
    "data",
    target_id
  )
  
  editor_set_react(
    x,
    exp_id,
    run_id,
    react_id,
    react_id,
    react
  )
}

editor_optional_numeric <- function(x) {
  x <- editor_empty_to_null(x)
  
  if (is.null(x)) {
    return(NA_real_)
  }
  
  value <- suppressWarnings(
    as.numeric(x)
  )
  
  if (
    !length(value) ||
    anyNA(value)
  ) {
    stop(
      sprintf(
        "Expected numeric value, got '%s'.",
        paste(x, collapse = ";")
      ),
      call. = FALSE
    )
  }
  
  value
}

editor_optional_integer <- function(x) {
  value <- editor_optional_numeric(x)
  
  if (length(value) == 1L && is.na(value)) {
    return(NA_integer_)
  }
  
  if (any(value != as.integer(value))) {
    stop(
      "Expected an integer value.",
      call. = FALSE
    )
  }
  
  as.integer(value)
}

editor_numeric_vector <- function(x) {
  x <- editor_empty_to_null(x)
  
  if (is.null(x)) {
    return(NA_real_)
  }
  
  if (length(x) == 1L) {
    x <- strsplit(
      as.character(x),
      ";",
      fixed = TRUE
    )[[1L]]
  }
  
  x <- trimws(x)
  
  if (!length(x) || any(!nzchar(x))) {
    return(NA_real_)
  }
  
  value <- suppressWarnings(
    as.numeric(x)
  )
  
  if (anyNA(value)) {
    stop(
      "meltTemp must contain numbers separated by ';'.",
      call. = FALSE
    )
  }
  
  value
}

editor_value_chr <- function(x, default = "") {
  if (is.null(x)) {
    return(default)
  }
  
  # RDML7 uses small S7 wrapper/value classes. Never coerce the S7 object
  # itself with as.character(); unwrap its properties first.
  if (editor_is_s7(x)) {
    properties <- editor_s7_props(x)
    
    for (property in c("value", "id", "sampleType", "type", "name")) {
      if (
        !is.null(properties) &&
        property %in% names(properties)
      ) {
        return(
          editor_value_chr(
            properties[[property]],
            default
          )
        )
      }
    }
    
    if (
      !is.null(properties) &&
      length(properties) == 1L
    ) {
      return(
        editor_value_chr(
          properties[[1L]],
          default
        )
      )
    }
    
    if (!is.null(properties) && length(properties)) {
      for (value in properties) {
        if (
          is.atomic(value) &&
          length(value) == 1L &&
          !is.na(value)
        ) {
          return(as.character(value))
        }
      }
    }
    
    return(default)
  }
  
  if (!length(x) || all(is.na(x))) {
    return(default)
  }
  
  if (is.list(x)) {
    values <- vapply(
      x,
      editor_value_chr,
      character(1),
      default = default
    )
    values <- unique(values[nzchar(values)])

    if (!length(values)) {
      return(default)
    }

    return(paste(values, collapse = ", "))
  }
  
  value <- x[[1L]]
  
  if (
    is.null(value) ||
    length(value) == 0L ||
    is.na(value)
  ) {
    return(default)
  }
  
  as.character(value)
}

editor_sample_type_chr <- function(sample, target_id, default = "") {
  types <- editor_list_prop(sample, "type")

  if (!length(types)) {
    return(default)
  }

  target_ids <- vapply(
    types,
    function(type) {
      editor_id_chr(editor_prop(type, "targetId"))
    },
    character(1)
  )
  selected <- which(target_ids == target_id)

  if (!length(selected)) {
    selected <- which(!nzchar(target_ids))
  }

  if (!length(selected)) {
    selected <- 1L
  }

  editor_value_chr(
    editor_prop(types[[selected[[1L]]]], "sampleType"),
    default
  )
}

editor_make_id_ref <- function(id) {
  id <- editor_empty_to_null(id)
  
  if (is.null(id)) {
    return(NULL)
  }
  
  RDML7::idReferenceType(id)
}

editor_make_id_refs <- function(ids) {
  ids <- ids[
    !is.na(ids) &
      nzchar(ids)
  ]
  
  lapply(
    ids,
    RDML7::idReferenceType
  )
}

editor_construct_react_id <- function(id) {
  # RDML files from different importers may expose reaction keys as positions
  # while the schema-level react id is numeric. Prefer the dedicated
  # constructor, falling back to idType only if an RDML7 development build has
  # already widened react IDs.
  numeric_id <- suppressWarnings(
    as.integer(id)
  )
  
  if (!is.na(numeric_id)) {
    value <- try(
      RDML7::reactIdType(
        numeric_id
      ),
      silent = TRUE
    )
    
    if (!inherits(value, "try-error")) {
      return(value)
    }
  }
  
  value <- try(
    RDML7::reactIdType(
      id
    ),
    silent = TRUE
  )
  
  if (!inherits(value, "try-error")) {
    return(value)
  }
  
  RDML7::idType(id)
}

editor_curve_key <- function(
    exp_id,
    run_id,
    react_id,
    target_id) {
  
  paste(
    exp_id,
    run_id,
    react_id,
    target_id,
    sep = "\034"
  )
}

editor_curve_catalog <- function(
    x,
    dp_type = c(
      "adp",
      "mdp"
    )) {
  
  dp_type <- match.arg(
    dp_type
  )
  
  rows <- list()
  index <- 0L
  
  experiments <- editor_list_prop(
    x,
    "experiment"
  )
  
  for (exp_id in names(experiments)) {
    experiment <- experiments[[exp_id]]
    runs <- editor_list_prop(
      experiment,
      "run"
    )
    
    for (run_id in names(runs)) {
      run <- runs[[run_id]]
      pcr_format <- editor_prop(
        run,
        "pcrFormat"
      )
      reacts <- editor_list_prop(
        run,
        "react"
      )
      
      for (react_id in names(reacts)) {
        react <- reacts[[react_id]]
        sample_id <- editor_id_chr(
          editor_prop(
            react,
            "sample"
          )
        )
        
        sample_obj <- editor_collection_get(
          x,
          "sample",
          sample_id
        )
        
        data_list <- editor_list_prop(
          react,
          "data"
        )
        
        for (target_key in names(data_list)) {
          data_obj <- data_list[[target_key]]
          curve <- editor_prop(
            data_obj,
            dp_type
          )
          
          if (
            is.null(curve) ||
            !editor_is_s7(curve)
          ) {
            next
          }
          
          fpoints <- editor_prop(
            curve,
            "fpoints"
          )
          
          if (
            is.null(fpoints) ||
            !NROW(fpoints)
          ) {
            next
          }
          
          target_id <- editor_id_chr(
            editor_prop(
              data_obj,
              "targetId"
            )
          )
          
          if (!nzchar(target_id)) {
            target_id <- target_key
          }

          sample_type <- editor_sample_type_chr(
            sample_obj,
            target_id
          )
          
          target_obj <- editor_collection_get(
            x,
            "target",
            target_id
          )
          
          dye_id <- editor_id_chr(
            editor_prop(
              target_obj,
              "dyeId"
            )
          )
          
          index <- index + 1L
          
          rows[[index]] <- data.frame(
            expId = exp_id,
            runId = run_id,
            reactId = react_id,
            position = RDML7:::.rdmlReactPosition(
              react,
              pcr_format
            ),
            sample = sample_id,
            sampleType = sample_type,
            target = target_id,
            targetDyeId = dye_id,
            cq = {
              cq <- editor_prop(
                data_obj,
                "cq"
              )
              
              if (
                is.null(cq) ||
                !length(cq)
              ) {
                NA_real_
              } else {
                suppressWarnings(
                  as.numeric(cq[[1L]])
                )
              }
            },
            quantFluor = {
              qf <- editor_prop(
                data_obj,
                "quantFluor"
              )
              
              if (
                is.null(qf) ||
                !length(qf)
              ) {
                NA_real_
              } else {
                suppressWarnings(
                  as.numeric(qf[[1L]])
                )
              }
            },
            curveKey = editor_curve_key(
              exp_id,
              run_id,
              react_id,
              target_id
            ),
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  
  if (!length(rows)) {
    return(
      data.frame(
        expId = character(),
        runId = character(),
        reactId = character(),
        position = character(),
        sample = character(),
        sampleType = character(),
        target = character(),
        targetDyeId = character(),
        cq = numeric(),
        quantFluor = numeric(),
        curveKey = character(),
        stringsAsFactors = FALSE
      )
    )
  }
  
  out <- do.call(
    rbind,
    rows
  )
  
  rownames(out) <- NULL
  out
}

editor_get_curve_points <- function(
    x,
    exp_id,
    run_id,
    react_id,
    target_id,
    dp_type = c(
      "adp",
      "mdp"
    )) {
  
  dp_type <- match.arg(
    dp_type
  )
  
  data_obj <- editor_get_data(
    x,
    exp_id,
    run_id,
    react_id,
    target_id
  )
  
  if (is.null(data_obj)) {
    return(NULL)
  }
  
  curve <- editor_prop(
    data_obj,
    dp_type
  )
  
  if (
    is.null(curve) ||
    !editor_is_s7(curve)
  ) {
    return(NULL)
  }
  
  fpoints <- editor_prop(
    curve,
    "fpoints"
  )
  
  if (is.null(fpoints)) {
    return(NULL)
  }
  
  as.data.frame(
    fpoints
  )
}

editor_set_curve_points <- function(
    x,
    exp_id,
    run_id,
    react_id,
    target_id,
    dp_type = c(
      "adp",
      "mdp"
    ),
    points) {
  
  dp_type <- match.arg(
    dp_type
  )
  
  data_obj <- editor_get_data(
    x,
    exp_id,
    run_id,
    react_id,
    target_id
  )
  
  if (is.null(data_obj)) {
    stop(
      "Data object not found.",
      call. = FALSE
    )
  }
  
  curve <- editor_prop(
    data_obj,
    dp_type
  )
  
  if (
    is.null(curve) ||
    !editor_is_s7(curve)
  ) {
    stop(
      sprintf(
        "Data object has no %s curve.",
        dp_type
      ),
      call. = FALSE
    )
  }
  
  curve <- S7::set_props(
    curve,
    fpoints = points
  )
  
  data_obj <- do.call(
    S7::set_props,
    c(
      list(data_obj),
      setNames(
        list(curve),
        dp_type
      )
    )
  )
  
  editor_set_data(
    x,
    exp_id,
    run_id,
    react_id,
    target_id,
    target_id,
    data_obj
  )
}

editor_set_data_values <- function(
    x,
    exp_id,
    run_id,
    react_id,
    target_id,
    values) {
  
  data_obj <- editor_get_data(
    x,
    exp_id,
    run_id,
    react_id,
    target_id
  )
  
  if (is.null(data_obj)) {
    stop(
      "Data object not found.",
      call. = FALSE
    )
  }
  
  values <- values[
    !vapply(
      values,
      is.null,
      logical(1)
    )
  ]
  
  data_obj <- do.call(
    S7::set_props,
    c(
      list(data_obj),
      values
    )
  )
  
  editor_set_data(
    x,
    exp_id,
    run_id,
    react_id,
    target_id,
    target_id,
    data_obj
  )
}

editor_curve_long_from_catalog <- function(
    x,
    catalog,
    dp_type = c(
      "adp",
      "mdp"
    )) {
  
  dp_type <- match.arg(
    dp_type
  )
  
  coordinate <- if (
    dp_type == "adp"
  ) {
    "cyc"
  } else {
    "tmp"
  }
  
  rows <- vector(
    "list",
    nrow(catalog)
  )
  
  for (i in seq_len(nrow(catalog))) {
    meta <- catalog[i, , drop = FALSE]
    
    points <- editor_get_curve_points(
      x,
      meta$expId,
      meta$runId,
      meta$reactId,
      meta$target,
      dp_type
    )
    
    if (
      is.null(points) ||
      !(coordinate %in% names(points)) ||
      !("fluor" %in% names(points))
    ) {
      next
    }
    
    n <- nrow(points)
    
    rows[[i]] <- cbind(
      meta[
        rep(
          1L,
          n
        ),
        ,
        drop = FALSE
      ],
      coordinate = as.numeric(
        points[[coordinate]]
      ),
      fluor = as.numeric(
        points$fluor
      ),
      stringsAsFactors = FALSE
    )
  }
  
  rows <- Filter(
    Negate(is.null),
    rows
  )
  
  if (!length(rows)) {
    return(
      data.frame()
    )
  }
  
  out <- do.call(
    rbind,
    rows
  )
  
  names(out)[
    names(out) == "coordinate"
  ] <- coordinate
  
  rownames(out) <- NULL
  out
}

editor_require_package <- function(package, purpose) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(
      sprintf(
        "Package '%s' is required for %s.",
        package,
        purpose
      ),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}

editor_extract_signal <- function(result, n) {
  if (is.null(result)) {
    stop(
      "Preprocessing returned no data.",
      call. = FALSE
    )
  }
  
  if (is.numeric(result) && length(result) == n) {
    return(
      as.numeric(result)
    )
  }
  
  if (
    is.matrix(result) ||
    is.data.frame(result)
  ) {
    result <- as.data.frame(
      result
    )
    
    numeric_cols <- vapply(
      result,
      is.numeric,
      logical(1)
    )
    
    candidates <- result[
      numeric_cols
    ]
    
    for (column in rev(candidates)) {
      if (length(column) == n) {
        return(
          as.numeric(column)
        )
      }
    }
  }
  
  if (is.list(result)) {
    for (item in result) {
      value <- try(
        editor_extract_signal(
          item,
          n
        ),
        silent = TRUE
      )
      
      if (!inherits(value, "try-error")) {
        return(value)
      }
    }
  }
  
  stop(
    "Could not extract a processed fluorescence vector.",
    call. = FALSE
  )
}

editor_preprocess_adp <- function(
    points,
    smoothing = TRUE,
    smoothing_method = "savgol",
    normalization_method = "none") {
  
  editor_require_package(
    "chipPCR",
    "qPCR preprocessing"
  )
  
  x <- as.numeric(
    points$cyc
  )
  y <- as.numeric(
    points$fluor
  )
  
  if (isTRUE(smoothing)) {
    smoothed <- chipPCR::smoother(
      x,
      y,
      method = smoothing_method
    )
    
    y <- editor_extract_signal(
      smoothed,
      length(y)
    )
  }
  
  y <- chipPCR::normalizer(
    y,
    method.norm = normalization_method
  )
  
  out <- points
  out$fluor <- as.numeric(y)
  out
}

editor_validate_cq <- function(cq, quant_fluor, cycles) {
  valid_cycles <- as.numeric(cycles)
  valid_cycles <- valid_cycles[is.finite(valid_cycles)]
  max_cycle <- if (length(valid_cycles)) {
    max(valid_cycles)
  } else {
    NA_real_
  }

  if (
    !length(cq) ||
    !is.finite(cq) ||
    !is.finite(max_cycle) ||
    cq > max_cycle
  ) {
    return(list(cq = NA_real_, quantFluor = NA_real_))
  }

  if (!length(quant_fluor) || !is.finite(quant_fluor)) {
    quant_fluor <- NA_real_
  }

  list(cq = cq, quantFluor = quant_fluor)
}

editor_calc_cq <- function(
    points,
    method = c(
      "none",
      "th",
      "sdm"
    ),
    threshold = 0,
    auto_threshold = FALSE) {
  
  method <- match.arg(
    method
  )
  
  if (method == "none") {
    return(
      list(
        cq = NULL,
        quantFluor = NULL
      )
    )
  }
  
  x <- as.numeric(
    points$cyc
  )
  y <- as.numeric(
    points$fluor
  )

  if (method == "th") {
    editor_require_package(
      "chipPCR",
      "threshold Cq calculation"
    )
    
    if (
      !isTRUE(auto_threshold) &&
      (
        !is.finite(threshold) ||
        threshold < min(y, na.rm = TRUE) ||
        threshold > max(y, na.rm = TRUE)
      )
    ) {
      return(list(cq = NA_real_, quantFluor = NA_real_))
    }

    result <- chipPCR::th.cyc(
      x,
      y,
      r = threshold,
      auto = auto_threshold
    )
    
    values <- suppressWarnings(
      as.numeric(result)
    )
    
    cq <- values[[1L]]
    quant_fluor <- if (
      length(values) >= 2L
    ) {
      values[[2L]]
    } else {
      NA_real_
    }
    
    return(editor_validate_cq(cq, quant_fluor, x))
  }
  
  editor_require_package(
    "MBmca",
    "SDM Cq calculation"
  )
  
  result <- MBmca::diffQ2(
    data.frame(
      cyc = x,
      fluor = y
    ),
    inder = TRUE,
    warn = FALSE
  )
  
  cq <- suppressWarnings(
    as.numeric(
      result$xTm1.2.D2[[1L]]
    )
  )
  
  if (!length(cq) || !is.finite(cq)) {
    return(editor_validate_cq(cq, NA_real_, x))
  }
  
  quant_fluor <- stats::approx(
    x,
    y,
    xout = cq,
    rule = 2
  )$y[[1L]]
  
  editor_validate_cq(cq, quant_fluor, x)
}

editor_detect_hook <- function(
    points,
    method = c(
      "none",
      "hookreg",
      "hookregNL",
      "both"
    )) {
  
  method <- match.arg(
    method
  )
  
  if (method == "none") {
    return(
      list(
        hook = NA,
        method = "none"
      )
    )
  }
  
  editor_require_package(
    "PCRedux",
    "hook-effect detection"
  )
  
  x <- as.numeric(
    points$cyc
  )
  y <- as.numeric(
    points$fluor
  )
  
  hook_reg <- FALSE
  hook_nl <- FALSE
  
  if (method %in% c("hookreg", "both")) {
    result <- PCRedux::hookreg(
      x = x,
      y = y
    )
    
    hook_reg <- isTRUE(
      as.logical(
        result[["hook"]]
      )
    )
  }
  
  if (method %in% c("hookregNL", "both")) {
    result <- PCRedux::hookregNL(
      x = x,
      y = y
    )
    
    hook_nl <- isTRUE(
      as.logical(
        result[["hook"]]
      )
    )
  }
  
  detected_method <- if (
    hook_reg && hook_nl
  ) {
    "both"
  } else if (hook_reg) {
    "hookreg"
  } else if (hook_nl) {
    "hookregNL"
  } else {
    method
  }
  
  list(
    hook = hook_reg || hook_nl,
    method = detected_method
  )
}

editor_preprocess_mdp <- function(
    points,
    background_adjust = FALSE,
    background_range = c(
      50,
      55
    ),
    min_max = FALSE,
    df_fact = 0.95) {
  
  editor_require_package(
    "MBmca",
    "melting-curve preprocessing"
  )
  
  x <- as.numeric(
    points$tmp
  )
  y <- as.numeric(
    points$fluor
  )

  # MBmca multiplies the automatically selected spline degrees of freedom by
  # df.fact without checking smooth.spline()'s upper bound. At the upper end
  # of the UI range this can exceed the number of unique temperatures.
  valid_spline_points <- is.finite(x) & is.finite(y)
  spline_df <- stats::smooth.spline(
    x[valid_spline_points],
    y[valid_spline_points]
  )$df
  max_df <- length(unique(x[valid_spline_points]))
  safe_df_fact <- max(
    0.6,
    min(
      as.numeric(df_fact),
      (max_df - sqrt(.Machine$double.eps)) / spline_df
    )
  )
  
  bg <- if (
    isTRUE(background_adjust)
  ) {
    as.numeric(
      background_range
    )
  } else {
    NULL
  }
  
  result <- MBmca::mcaSmoother(
    x,
    y,
    bgadj = isTRUE(
      background_adjust
    ),
    bg = bg,
    minmax = isTRUE(
      min_max
    ),
    df.fact = safe_df_fact
  )
  
  signal <- editor_extract_signal(
    result,
    length(y)
  )
  
  out <- points
  out$fluor <- signal
  out
}

editor_melting_derivative <- function(points) {
  x <- as.numeric(
    points$tmp
  )
  y <- as.numeric(
    points$fluor
  )
  
  if (
    requireNamespace(
      "MBmca",
      quietly = TRUE
    )
  ) {
    result <- try(
      MBmca::diffQ(
        cbind(
          x,
          y
        ),
        verbose = FALSE,
        warn = FALSE
      )$xy,
      silent = TRUE
    )
    
    if (
      !inherits(
        result,
        "try-error"
      ) &&
      !is.null(result)
    ) {
      result <- as.data.frame(
        result
      )
      
      if (ncol(result) >= 2L) {
        deriv <- as.numeric(
          result[[2L]]
        )
        
        if (length(deriv) == length(y)) {
          return(deriv)
        }
      }
    }
  }
  
  if (length(x) < 2L) {
    return(
      rep(
        NA_real_,
        length(x)
      )
    )
  }
  
  slope <- -diff(y) / diff(x)
  
  c(
    slope[[1L]],
    slope
  )
}

editor_compact_plotly_legend <- function(
    plot,
    color_by = "none",
    line_by = "none",
    keep_names = "Cq") {
  plot <- plotly::plotly_build(plot)
  aesthetics <- unique(c(color_by, line_by))
  aesthetic_count <- sum(aesthetics != "none")
  seen <- character()

  for (i in seq_along(plot$x$data)) {
    trace <- plot$x$data[[i]]
    trace_name <- trace$name

    if (
      is.null(trace_name) ||
      !length(trace_name) ||
      trace_name %in% keep_names
    ) {
      next
    }

    if (!aesthetic_count) {
      plot$x$data[[i]]$showlegend <- FALSE
      next
    }

    parts <- strsplit(
      trace_name,
      "<br\\s*/?>",
      perl = TRUE
    )[[1L]]
    legend_name <- paste(
      head(parts, aesthetic_count),
      collapse = " / "
    )

    plot$x$data[[i]]$name <- legend_name
    plot$x$data[[i]]$legendgroup <- legend_name
    plot$x$data[[i]]$showlegend <- !(legend_name %in% seen)
    seen <- unique(c(seen, legend_name))
  }

  plot
}

editor_curve_table_stats <- function(catalog) {
  if (!nrow(catalog)) {
    return(catalog)
  }
  
  key <- interaction(
    catalog$sample,
    catalog$target,
    drop = TRUE
  )
  
  cq_mean <- ave(
    catalog$cq,
    key,
    FUN = function(x) {
      if (all(is.na(x))) {
        NA_real_
      } else {
        mean(
          x,
          na.rm = TRUE
        )
      }
    }
  )
  
  cq_sd <- ave(
    catalog$cq,
    key,
    FUN = function(x) {
      x <- x[!is.na(x)]
      
      if (length(x) < 2L) {
        NA_real_
      } else {
        stats::sd(x)
      }
    }
  )
  
  qf_mean <- ave(
    catalog$quantFluor,
    key,
    FUN = function(x) {
      if (all(is.na(x))) {
        NA_real_
      } else {
        mean(
          x,
          na.rm = TRUE
        )
      }
    }
  )
  
  catalog$cqMean <- cq_mean
  catalog$cqSd <- cq_sd
  catalog$quantFluorMean <- qf_mean
  catalog
}

editor_subset_catalog <- function(
    catalog,
    experiment = NULL,
    run = NULL,
    targets = NULL,
    positions = NULL) {
  
  out <- catalog
  
  if (
    !is.null(experiment) &&
    length(experiment) &&
    nzchar(experiment)
  ) {
    out <- out[
      out$expId %in% experiment,
      ,
      drop = FALSE
    ]
  }
  
  if (
    !is.null(run) &&
    length(run) &&
    nzchar(run)
  ) {
    out <- out[
      out$runId %in% run,
      ,
      drop = FALSE
    ]
  }
  
  if (
    !is.null(targets) &&
    length(targets)
  ) {
    out <- out[
      out$target %in% targets,
      ,
      drop = FALSE
    ]
  }
  
  if (
    !is.null(positions) &&
    length(positions)
  ) {
    out <- out[
      out$position %in% positions,
      ,
      drop = FALSE
    ]
  }
  
  out
}


editor_plate_dimensions_from_positions <- function(positions) {
  positions <- unique(
    as.character(
      positions
    )
  )
  
  positions <- positions[
    !is.na(positions) &
      nzchar(positions)
  ]
  
  # Conventional A1 / A01 PCR-well notation.
  matched <- regexec(
    "^([A-Za-z]+)0*([0-9]+)$",
    positions
  )
  
  parts <- regmatches(
    positions,
    matched
  )
  
  valid <- lengths(parts) == 3L
  
  if (length(parts) && all(valid)) {
    row_names <- toupper(
      vapply(
        parts,
        `[[`,
        character(1),
        2L
      )
    )
    
    columns <- as.integer(
      vapply(
        parts,
        `[[`,
        character(1),
        3L
      )
    )
    
    # Convert Excel-style letters to a row number.
    row_number <- function(label) {
      chars <- utf8ToInt(label) - utf8ToInt("A") + 1L
      
      as.integer(sum(
        chars *
          26L ^ rev(
            seq_along(chars) - 1L
          )
      ))
    }
    
    max_row <- max(
      vapply(
        row_names,
        row_number,
        integer(1)
      ),
      na.rm = TRUE
    )
    
    max_col <- max(
      columns,
      na.rm = TRUE
    )
    
    # Prefer conventional complete plate dimensions. This avoids rendering
    # a 1 x 12 "plate" when an RDES file contains data only in row A.
    if (
      max_row <= 8L &&
      max_col <= 12L
    ) {
      return(
        list(
          rows = 8L,
          columns = 12L,
          rowLabel = "ABC",
          columnLabel = "123"
        )
      )
    }
    
    if (
      max_row <= 16L &&
      max_col <= 24L
    ) {
      return(
        list(
          rows = 16L,
          columns = 24L,
          rowLabel = "ABC",
          columnLabel = "123"
        )
      )
    }
    
    if (
      max_row <= 32L &&
      max_col <= 48L
    ) {
      return(
        list(
          rows = 32L,
          columns = 48L,
          rowLabel = "ABC",
          columnLabel = "123"
        )
      )
    }
    
    return(
      list(
        rows = max_row,
        columns = max_col,
        rowLabel = "ABC",
        columnLabel = "123"
      )
    )
  }
  
  # Numeric positions: use a conventional 96-well layout when possible.
  numeric_positions <- suppressWarnings(
    as.integer(
      positions
    )
  )
  
  if (
    length(numeric_positions) &&
    all(!is.na(numeric_positions))
  ) {
    maximum <- max(
      numeric_positions
    )
    
    if (maximum <= 96L) {
      return(
        list(
          rows = 8L,
          columns = 12L,
          rowLabel = "ABC",
          columnLabel = "123"
        )
      )
    }
    
    if (maximum <= 384L) {
      return(
        list(
          rows = 16L,
          columns = 24L,
          rowLabel = "ABC",
          columnLabel = "123"
        )
      )
    }
    
    if (maximum <= 1536L) {
      return(
        list(
          rows = 32L,
          columns = 48L,
          rowLabel = "ABC",
          columnLabel = "123"
        )
      )
    }
  }
  
  # Last-resort conventional plate.
  list(
    rows = 8L,
    columns = 12L,
    rowLabel = "ABC",
    columnLabel = "123"
  )
}

editor_rdml7_pcr_format_values <- function(
    pcr_format,
    positions = character()) {
  
  inferred <- editor_plate_dimensions_from_positions(
    positions
  )
  
  if (is.null(pcr_format)) {
    return(inferred)
  }
  
  get_int <- function(name, fallback) {
    value <- editor_prop(
      pcr_format,
      name
    )
    
    value <- suppressWarnings(
      as.integer(
        value
      )
    )
    
    if (
      !length(value) ||
      is.na(value[[1L]])
    ) {
      fallback
    } else {
      value[[1L]]
    }
  }
  
  get_label <- function(name, fallback) {
    value <- editor_value_chr(
      editor_prop(
        pcr_format,
        name
      ),
      fallback
    )
    
    if (
      !length(value) ||
      is.na(value[[1L]]) ||
      !nzchar(value[[1L]])
    ) {
      fallback
    } else {
      value[[1L]]
    }
  }
  
  list(
    rows = get_int(
      "rows",
      inferred$rows
    ),
    columns = get_int(
      "columns",
      inferred$columns
    ),
    rowLabel = get_label(
      "rowLabel",
      inferred$rowLabel
    ),
    columnLabel = get_label(
      "columnLabel",
      inferred$columnLabel
    )
  )
}

editor_shinyMolBio_pcr_format <- function(
    pcr_format = NULL,
    positions = character()) {
  
  if (
    !requireNamespace(
      "shinyMolBio",
      quietly = TRUE
    )
  ) {
    return(NULL)
  }
  
  # shinyMolBio currently imports the original RDML package and its
  # pcrPlateInput() validates against the old R6 class "pcrFormatType".
  # Do not pass an RDML7 S7 pcrFormatType directly: create an isolated
  # compatibility object used only by the widget.
  if (
    !requireNamespace(
      "RDML",
      quietly = TRUE
    )
  ) {
    return(NULL)
  }
  
  values <- editor_rdml7_pcr_format_values(
    pcr_format,
    positions
  )
  
  old_namespace <- asNamespace(
    "RDML"
  )
  
  old_pcr_format_type <- get(
    "pcrFormatType",
    envir = old_namespace,
    inherits = FALSE
  )
  
  old_label_format_type <- get(
    "labelFormatType",
    envir = old_namespace,
    inherits = FALSE
  )
  
  old_pcr_format_type$new(
    values$rows,
    values$columns,
    old_label_format_type$new(
      values$rowLabel
    ),
    old_label_format_type$new(
      values$columnLabel
    )
  )
}

editor_can_use_shinyMolBio <- function() {
  requireNamespace(
    "shinyMolBio",
    quietly = TRUE
  ) &&
    requireNamespace(
      "RDML",
      quietly = TRUE
    )
}
