#' Represent rdmlType structure as a dendrogram
#'
#' S7 port of PCRuniversum/RDML::RDML$asDendrogram().
#' The hierarchy is:
#'
#' experiment -> run -> target -> sampleType -> adp/mdp
#'
#' The leaf label contains the number of reactions with the corresponding
#' fluorescence-data type, matching the behaviour of the original package.
#'
#' @param x A `rdmlType` object.
#' @param plotDendrogram Logical; plot the dendrogram when `TRUE`.
#' @param ... Reserved for future use.
#'
#' @return A base R `dendrogram` object.
#' @export
#' @include generics.R RDML.asTable.R
S7::method(asDendrogram, rdmlType) <- function(
    x,
    plotDendrogram = TRUE,
    ...) {

  checkmate::assertFlag(plotDendrogram)

  cutText <- function(text) {
    text <- as.character(text)

    if (
      length(text) != 1L ||
      is.na(text)
    ) {
      return("NA")
    }

    if (!nzchar(text)) {
      return("")
    }

    if (nchar(text) > 9L) {
      return(
        paste0(
          substr(text, 1L, 3L),
          "...",
          substr(
            text,
            nchar(text) - 2L,
            nchar(text)
          )
        )
      )
    }

    text
  }

  # Avoid NA/empty list indices while keeping the displayed label separate.
  treeKey <- function(x) {
    x <- as.character(x)

    if (
      length(x) != 1L ||
      is.na(x)
    ) {
      return("NA")
    }

    if (!nzchar(x)) {
      return("<empty>")
    }

    x
  }

  totalTable <- asTable(x)

  tree <- list()
  attributes(tree) <- list(
    members = 0L,
    height = 5
  )
  class(tree) <- "dendrogram"

  if (!nrow(totalTable)) {
    if (plotDendrogram) {
      warning(
        "RDML object contains no fluorescence data",
        call. = FALSE
      )
    }

    return(tree)
  }

  required <- c(
    "expId",
    "runId",
    "target",
    "sampleType",
    "adp",
    "mdp"
  )

  missingColumns <- setdiff(
    required,
    names(totalTable)
  )

  if (length(missingColumns)) {
    stop(
      "asTable() result is missing required column(s): ",
      paste(missingColumns, collapse = ", "),
      call. = FALSE
    )
  }

  # Work on a private copy because data.table's := modifies by reference.
  totalTable <- data.table::copy(totalTable)

  totalTable[
    is.na(expId) | !nzchar(as.character(expId)),
    expId := "NA"
  ]
  totalTable[
    is.na(runId) | !nzchar(as.character(runId)),
    runId := "NA"
  ]
  totalTable[
    is.na(target) | !nzchar(as.character(target)),
    target := "NA"
  ]
  totalTable[
    is.na(sampleType) | !nzchar(as.character(sampleType)),
    sampleType := "NA"
  ]

  for (experId in unique(totalTable$expId)) {

    experKey <- treeKey(experId)

    tree[[experKey]] <- list()
    attributes(tree[[experKey]]) <- list(
      members = 0L,
      height = 4,
      edgetext = cutText(experId)
    )

    runIds <- unique(
      totalTable[
        expId == experId,
        runId
      ]
    )

    for (rId in runIds) {

      runKey <- treeKey(rId)

      tree[[experKey]][[runKey]] <- list()
      attributes(tree[[experKey]][[runKey]]) <- list(
        members = 0L,
        height = 3,
        edgetext = cutText(rId)
      )

      targets <- unique(
        totalTable[
          expId == experId &
            runId == rId,
          target
        ]
      )

      for (trgt in targets) {

        targetKey <- treeKey(trgt)

        tree[[experKey]][[runKey]][[targetKey]] <- list()
        attributes(
          tree[[experKey]][[runKey]][[targetKey]]
        ) <- list(
          members = 0L,
          height = 2,
          edgetext = cutText(trgt)
        )

        sampleTypes <- unique(
          totalTable[
            expId == experId &
              runId == rId &
              target == trgt,
            sampleType
          ]
        )

        for (stype in sampleTypes) {

          stypeKey <- treeKey(stype)

          tree[[experKey]][[runKey]][[targetKey]][[stypeKey]] <- list()

          attributes(
            tree[[experKey]][[runKey]][[targetKey]][[stypeKey]]
          ) <- list(
            members = 0L,
            height = 1,
            edgetext = cutText(stype)
          )

          subset <- totalTable[
            expId == experId &
              runId == rId &
              target == trgt &
              sampleType == stype
          ]

          for (expType in c("adp", "mdp")) {

            nRows <- sum(
              subset[[expType]] %in% TRUE,
              na.rm = TRUE
            )

            if (nRows == 0L) {
              next
            }

            tree[[experKey]][[runKey]][[targetKey]][[stypeKey]][[expType]] <-
              list()

            attributes(
              tree[[experKey]][[runKey]][[targetKey]][[stypeKey]][[expType]]
            ) <- list(
              members = 1L,
              height = 0,
              edgetext = expType,
              label = nRows,
              leaf = TRUE
            )

            attributes(
              tree[[experKey]][[runKey]][[targetKey]][[stypeKey]]
            )$members <-
              attributes(
                tree[[experKey]][[runKey]][[targetKey]][[stypeKey]]
              )$members + 1L

            attributes(
              tree[[experKey]][[runKey]][[targetKey]]
            )$members <-
              attributes(
                tree[[experKey]][[runKey]][[targetKey]]
              )$members + 1L

            attributes(
              tree[[experKey]][[runKey]]
            )$members <-
              attributes(
                tree[[experKey]][[runKey]]
              )$members + 1L

            attributes(
              tree[[experKey]]
            )$members <-
              attributes(
                tree[[experKey]]
              )$members + 1L

            attributes(tree)$members <-
              attributes(tree)$members + 1L
          }
        }
      }
    }
  }

  if (plotDendrogram) {

    suppressWarnings(
      plot(
        rev(tree),
        center = TRUE,
        horiz = TRUE,
        yaxt = "n"
      )
    )

    xtick <- seq(
      0,
      5,
      by = 0.5
    )

    axis(
      side = 1,
      at = xtick,
      lty = "blank",
      las = 2,
      labels = FALSE
    )

    text(
      xtick,
      par("usr")[3] - 0.2,
      labels = c(
        "Number\nof reacts",
        "Data type",
        "",
        "Sample\ntype",
        "",
        "Target\n(gene)",
        "",
        "Run ID",
        "",
        "Experiment ID",
        ""
      ),
      srt = 45,
      pos = 1,
      xpd = TRUE
    )
  }

  tree
}
