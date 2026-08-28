NULL

# Compact RDML summaries ----------------------------------------------------

#' Summarize an RDML object
#'
#' Counts experiments, runs, reactions, data entries, metadata objects,
#' amplification/melting curves, Cq values, and multi-Tm entries.
#'
#' @param x `rdmlType`.
#' @return An `rdmlSummary` list containing totals and a per-run table.
#' @seealso `rdmlValidate`, `asDendrogram`
#' @export
rdmlSummary <- function(x) {
  if (!S7::S7_inherits(x, rdmlType)) {
    .rdmlAbort(
      code = "invalidObject",
      message = "`x` must be an rdmlType object"
    )
  }

  experiments <- .rdmlPropList(x, "experiment")
  runRows <- list()
  runI <- 0L

  totalRuns <- 0L
  totalReacts <- 0L
  totalData <- 0L
  totalAdp <- 0L
  totalMdp <- 0L
  totalCq <- 0L
  totalMultiTm <- 0L

  for (experiment in experiments) {
    expId <- .rdmlIdChr(experiment$id)

    for (run in .rdmlPropList(experiment, "run")) {
      runId <- .rdmlIdChr(run$id)
      reacts <- .rdmlPropList(run, "react")
      dataList <- unlist(
        lapply(reacts, function(react) .rdmlPropList(react, "data")),
        recursive = FALSE
      )

      targetIds <- unique(vapply(
        dataList,
        function(dataObj) .rdmlIdChr(dataObj$targetId),
        character(1)
      ))

      nAdp <- sum(vapply(
        dataList,
        function(dataObj) .rdmlPresent(dataObj$adp),
        logical(1)
      ))

      nMdp <- sum(vapply(
        dataList,
        function(dataObj) .rdmlPresent(dataObj$mdp),
        logical(1)
      ))

      nCq <- sum(vapply(
        dataList,
        function(dataObj) .rdmlPresent(dataObj$cq),
        logical(1)
      ))

      nMultiTm <- sum(vapply(
        dataList,
        function(dataObj) {
          .rdmlPresent(dataObj$meltTemps) && length(dataObj$meltTemps) > 1L
        },
        logical(1)
      ))

      runI <- runI + 1L
      runRows[[runI]] <- data.table::data.table(
        expId = expId,
        runId = runId,
        reacts = length(reacts),
        targets = length(targetIds),
        data = length(dataList),
        adp = nAdp,
        mdp = nMdp,
        cq = nCq,
        multiTm = nMultiTm
      )

      totalRuns <- totalRuns + 1L
      totalReacts <- totalReacts + length(reacts)
      totalData <- totalData + length(dataList)
      totalAdp <- totalAdp + nAdp
      totalMdp <- totalMdp + nMdp
      totalCq <- totalCq + nCq
      totalMultiTm <- totalMultiTm + nMultiTm
    }
  }

  runs <- if (length(runRows)) {
    data.table::rbindlist(runRows)
  } else {
    data.table::data.table(
      expId = character(),
      runId = character(),
      reacts = integer(),
      targets = integer(),
      data = integer(),
      adp = integer(),
      mdp = integer(),
      cq = integer(),
      multiTm = integer()
    )
  }

  structure(
    list(
      version = x$version,
      experiments = length(experiments),
      runs = totalRuns,
      reacts = totalReacts,
      data = totalData,
      samples = length(.rdmlPropList(x, "sample")),
      targets = length(.rdmlPropList(x, "target")),
      dyes = length(.rdmlPropList(x, "dye")),
      adp = totalAdp,
      mdp = totalMdp,
      cq = totalCq,
      multiTm = totalMultiTm,
      byRun = runs
    ),
    class = "rdmlSummary"
  )
}


#' @export
print.rdmlSummary <- function(x, ...) {
  cat(sprintf("<RDML %s>\n", x$version))
  cat(sprintf("Experiments:          %d\n", x$experiments))
  cat(sprintf("Runs:                 %d\n", x$runs))
  cat(sprintf("Reactions:            %d\n", x$reacts))
  cat(sprintf("Data entries:         %d\n", x$data))
  cat(sprintf("Samples:              %d\n", x$samples))
  cat(sprintf("Targets:              %d\n", x$targets))
  cat(sprintf("Dyes:                 %d\n", x$dyes))
  cat(sprintf("Amplification curves: %d\n", x$adp))
  cat(sprintf("Melting curves:       %d\n", x$mdp))
  cat(sprintf("Cq values:            %d\n", x$cq))

  if (x$multiTm > 0L) {
    cat(sprintf("Multi-Tm entries:     %d\n", x$multiTm))
  }

  if (nrow(x$byRun)) {
    cat("\nBy run:\n")
    print(
      as.data.frame(x$byRun),
      row.names = FALSE
    )
  }

  invisible(x)
}


S7::method(print, rdmlType) <- function(x, ...) {
  summary <- rdmlSummary(x)
  cat(sprintf(
    "<RDML %s> %d experiment(s), %d run(s), %d reaction(s), %d target(s)\n",
    summary$version,
    summary$experiments,
    summary$runs,
    summary$reacts,
    summary$targets
  ))
  cat(sprintf(
    "  curves: %d amplification, %d melting; Cq: %d\n",
    summary$adp,
    summary$mdp,
    summary$cq
  ))

  invisible(x)
}
