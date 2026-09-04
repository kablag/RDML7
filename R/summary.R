# summary.R ----------------------------------------------------------------
# summary() method for rdmlType
#
# This replaces the separate rdmlSummary() public API with a method for
# summary(). `totalMultiTm` is intentionally not calculated or returned.

.rdmlSummaryCounts <- function(object) {
  experiments <- .rdmlPropList(
    object,
    "experiment"
  )

  totalRuns <- 0L
  totalReacts <- 0L
  totalData <- 0L
  totalAdp <- 0L
  totalMdp <- 0L

  for (experiment in experiments) {
    runs <- .rdmlPropList(
      experiment,
      "run"
    )

    totalRuns <- totalRuns + length(runs)

    for (run in runs) {
      reacts <- .rdmlPropList(
        run,
        "react"
      )

      totalReacts <- totalReacts + length(reacts)

      for (react in reacts) {
        data <- .rdmlPropList(
          react,
          "data"
        )

        totalData <- totalData + length(data)

        for (datum in data) {
          totalAdp <- totalAdp + as.integer(
            .rdmlPresent(
              S7::prop(
                datum,
                "adp"
              )
            )
          )

          totalMdp <- totalMdp + as.integer(
            .rdmlPresent(
              S7::prop(
                datum,
                "mdp"
              )
            )
          )
        }
      }
    }
  }

  list(
    totalExperiments = length(experiments),
    totalRuns = totalRuns,
    totalReacts = totalReacts,
    totalData = totalData,
    totalSamples = length(
      .rdmlPropList(
        object,
        "sample"
      )
    ),
    totalTargets = length(
      .rdmlPropList(
        object,
        "target"
      )
    ),
    totalDyes = length(
      .rdmlPropList(
        object,
        "dye"
      )
    ),
    totalThermalCyclingConditions = length(
      .rdmlPropList(
        object,
        "thermalCyclingConditions"
      )
    ),
    totalAdp = totalAdp,
    totalMdp = totalMdp
  )
}


.rdmlSummaryScalar <- function(x) {
  if (.rdmlIsMissing(x)) {
    return(NA_character_)
  }

  if (
    is.atomic(x) &&
      length(x) == 1L
  ) {
    return(
      as.character(x)
    )
  }

  if (S7::S7_inherits(x)) {
    value <- tryCatch(
      S7::prop(
        x,
        "value"
      ),
      error = function(e) NULL
    )

    if (
      !is.null(value) &&
        is.atomic(value) &&
        length(value) == 1L
    ) {
      return(
        as.character(value)
      )
    }
  }

  as.character(x)[1L]
}


#' Summarize an RDML object
#'
#' Provides a compact structural summary of an [rdmlType] object.
#'
#' `summary()` reports the RDML version and dates together with counts of
#' experiments, runs, reactions, data elements, samples, targets, dyes,
#' thermal cycling conditions, amplification curves (`adp`), and melting
#' curves (`mdp`).
#'
#' The historical `totalMultiTm` field is no longer calculated or returned.
#'
#' @param object An [rdmlType] object.
#' @param ... Additional arguments. Currently unused.
#'
#' @return An object of class `summary.rdmlType`.
#'
#' @examples
#' \dontrun{
#' x <- readRDML("example.rdml")
#' summary(x)
#' }
S7::method(
  summary,
  rdmlType
) <- function(
    object,
    ...) {

  counts <- .rdmlSummaryCounts(
    object
  )

  out <- c(
    list(
      version = .rdmlSummaryScalar(
        S7::prop(
          object,
          "version"
        )
      ),
      dateMade = .rdmlSummaryScalar(
        S7::prop(
          object,
          "dateMade"
        )
      ),
      dateUpdated = .rdmlSummaryScalar(
        S7::prop(
          object,
          "dateUpdated"
        )
      )
    ),
    counts
  )

  class(out) <- c(
    "summary.rdmlType",
    "list"
  )

  out
}


#' @export
print.summary.rdmlType <- function(
    x,
    ...) {

  cat(
    "RDML summary\n"
  )

  cat(
    "  Version: ",
    if (is.na(x$version)) {
      "<unknown>"
    } else {
      x$version
    },
    "\n",
    sep = ""
  )

  if (!is.na(x$dateMade)) {
    cat(
      "  Date made: ",
      x$dateMade,
      "\n",
      sep = ""
    )
  }

  if (!is.na(x$dateUpdated)) {
    cat(
      "  Date updated: ",
      x$dateUpdated,
      "\n",
      sep = ""
    )
  }

  cat(
    "\nStructure\n",
    "  Experiments:                ",
    x$totalExperiments,
    "\n",
    "  Runs:                       ",
    x$totalRuns,
    "\n",
    "  Reactions:                  ",
    x$totalReacts,
    "\n",
    "  Data elements:              ",
    x$totalData,
    "\n",
    "  Samples:                    ",
    x$totalSamples,
    "\n",
    "  Targets:                    ",
    x$totalTargets,
    "\n",
    "  Dyes:                       ",
    x$totalDyes,
    "\n",
    "  Thermal cycling conditions: ",
    x$totalThermalCyclingConditions,
    "\n",
    sep = ""
  )

  cat(
    "\nFluorescence data\n",
    "  Amplification curves (adp): ",
    x$totalAdp,
    "\n",
    "  Melting curves (mdp):       ",
    x$totalMdp,
    "\n",
    sep = ""
  )

  invisible(x)
}
