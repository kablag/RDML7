rdml7AnalysisSharedServer <- function(serverEnv) {
  if (!is.environment(serverEnv)) {
    stop("serverEnv must be an environment.", call. = FALSE)
  }

  evalq({
# ---------------------------------------------------------------------------
# qPCR and melting shared state
# ---------------------------------------------------------------------------

activeRawStore <- function(dp_type) {
  document <- isolate(
    input$rdmlFileSlct
  )

  if (
    is.null(document) ||
    !nzchar(document)
  ) {
    document <- ".active"
  }

  if (is.null(values$rawCurves[[document]])) {
    values$rawCurves[[document]] <- list(
      adp = list(),
      mdp = list()
    )
  }

  if (is.null(values$rawCurves[[document]][[dp_type]])) {
    values$rawCurves[[document]][[dp_type]] <- list()
  }

  values$rawCurves[[document]][[dp_type]]
}

setActiveRawStore <- function(dp_type, store) {
  document <- isolate(
    input$rdmlFileSlct
  )

  if (
    is.null(document) ||
    !nzchar(document)
  ) {
    document <- ".active"
  }

  if (is.null(values$rawCurves[[document]])) {
    values$rawCurves[[document]] <- list(
      adp = list(),
      mdp = list()
    )
  }

  values$rawCurves[[document]][[dp_type]] <- store
  invisible(NULL)
}

backupCurve <- function(meta, dp_type) {
  store <- activeRawStore(
    dp_type
  )

  key <- meta$curveKey[[1L]]

  if (is.null(store[[key]])) {
    store[[key]] <- editor_get_curve_points(
      values$rdml,
      meta$expId[[1L]],
      meta$runId[[1L]],
      meta$reactId[[1L]],
      meta$target[[1L]],
      dp_type
    )

    setActiveRawStore(
      dp_type,
      store
    )
  }

  invisible(NULL)
}

restoreCurves <- function(dp_type) {
  store <- activeRawStore(
    dp_type
  )

  if (!length(store)) {
    return(
      invisible(NULL)
    )
  }

  catalog <- editor_curve_catalog(
    values$rdml,
    dp_type
  )

  for (i in seq_len(nrow(catalog))) {
    meta <- catalog[i, , drop = FALSE]
    points <- store[[meta$curveKey[[1L]]]]

    if (is.null(points)) {
      next
    }

    values$rdml <- editor_set_curve_points(
      values$rdml,
      meta$expId[[1L]],
      meta$runId[[1L]],
      meta$reactId[[1L]],
      meta$target[[1L]],
      dp_type,
      points
    )
  }

  commitActive()
  invisible(NULL)
}


  }, envir = serverEnv)

  invisible(NULL)
}
