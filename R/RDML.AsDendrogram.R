#' Represent rdmlType structure as a dendrogram
#'
#' S7 port of PCRuniversum/RDML::RDML$AsDendrogram().
#' The hierarchy is:
#'
#' experiment -> run -> target -> sample.type -> adp/mdp
#'
#' The leaf label contains the number of reactions with the corresponding
#' fluorescence-data type, matching the behaviour of the original package.
#'
#' @param x A `rdmlType` object.
#' @param plot.dendrogram Logical; plot the dendrogram when `TRUE`.
#' @param ... Reserved for future use.
#'
#' @return A base R `dendrogram` object.
#' @export
#' @include generics.R RDML.AsTable.R
S7::method(AsDendrogram, rdmlType) <- function(
    x,
    plot.dendrogram = TRUE,
    ...) {

  checkmate::assertFlag(plot.dendrogram)

  cut.text <- function(text) {
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
  tree.key <- function(x) {
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

  total.table <- AsTable(x)

  tree <- list()
  attributes(tree) <- list(
    members = 0L,
    height = 5
  )
  class(tree) <- "dendrogram"

  if (!nrow(total.table)) {
    if (plot.dendrogram) {
      warning(
        "RDML object contains no fluorescence data",
        call. = FALSE
      )
    }

    return(tree)
  }

  required <- c(
    "exp.id",
    "run.id",
    "target",
    "sample.type",
    "adp",
    "mdp"
  )

  missing.columns <- setdiff(
    required,
    names(total.table)
  )

  if (length(missing.columns)) {
    stop(
      "AsTable() result is missing required column(s): ",
      paste(missing.columns, collapse = ", "),
      call. = FALSE
    )
  }

  # Work on a private copy because data.table's := modifies by reference.
  total.table <- data.table::copy(total.table)

  total.table[
    is.na(exp.id) | !nzchar(as.character(exp.id)),
    exp.id := "NA"
  ]
  total.table[
    is.na(run.id) | !nzchar(as.character(run.id)),
    run.id := "NA"
  ]
  total.table[
    is.na(target) | !nzchar(as.character(target)),
    target := "NA"
  ]
  total.table[
    is.na(sample.type) | !nzchar(as.character(sample.type)),
    sample.type := "NA"
  ]

  for (exper.id in unique(total.table$exp.id)) {

    exper.key <- tree.key(exper.id)

    tree[[exper.key]] <- list()
    attributes(tree[[exper.key]]) <- list(
      members = 0L,
      height = 4,
      edgetext = cut.text(exper.id)
    )

    run.ids <- unique(
      total.table[
        exp.id == exper.id,
        run.id
      ]
    )

    for (r.id in run.ids) {

      run.key <- tree.key(r.id)

      tree[[exper.key]][[run.key]] <- list()
      attributes(tree[[exper.key]][[run.key]]) <- list(
        members = 0L,
        height = 3,
        edgetext = cut.text(r.id)
      )

      targets <- unique(
        total.table[
          exp.id == exper.id &
            run.id == r.id,
          target
        ]
      )

      for (trgt in targets) {

        target.key <- tree.key(trgt)

        tree[[exper.key]][[run.key]][[target.key]] <- list()
        attributes(
          tree[[exper.key]][[run.key]][[target.key]]
        ) <- list(
          members = 0L,
          height = 2,
          edgetext = cut.text(trgt)
        )

        sample.types <- unique(
          total.table[
            exp.id == exper.id &
              run.id == r.id &
              target == trgt,
            sample.type
          ]
        )

        for (stype in sample.types) {

          stype.key <- tree.key(stype)

          tree[[exper.key]][[run.key]][[target.key]][[stype.key]] <- list()

          attributes(
            tree[[exper.key]][[run.key]][[target.key]][[stype.key]]
          ) <- list(
            members = 0L,
            height = 1,
            edgetext = cut.text(stype)
          )

          subset <- total.table[
            exp.id == exper.id &
              run.id == r.id &
              target == trgt &
              sample.type == stype
          ]

          for (exp.type in c("adp", "mdp")) {

            n.rows <- sum(
              subset[[exp.type]] %in% TRUE,
              na.rm = TRUE
            )

            if (n.rows == 0L) {
              next
            }

            tree[[exper.key]][[run.key]][[target.key]][[stype.key]][[exp.type]] <-
              list()

            attributes(
              tree[[exper.key]][[run.key]][[target.key]][[stype.key]][[exp.type]]
            ) <- list(
              members = 1L,
              height = 0,
              edgetext = exp.type,
              label = n.rows,
              leaf = TRUE
            )

            attributes(
              tree[[exper.key]][[run.key]][[target.key]][[stype.key]]
            )$members <-
              attributes(
                tree[[exper.key]][[run.key]][[target.key]][[stype.key]]
              )$members + 1L

            attributes(
              tree[[exper.key]][[run.key]][[target.key]]
            )$members <-
              attributes(
                tree[[exper.key]][[run.key]][[target.key]]
              )$members + 1L

            attributes(
              tree[[exper.key]][[run.key]]
            )$members <-
              attributes(
                tree[[exper.key]][[run.key]]
              )$members + 1L

            attributes(
              tree[[exper.key]]
            )$members <-
              attributes(
                tree[[exper.key]]
              )$members + 1L

            attributes(tree)$members <-
              attributes(tree)$members + 1L
          }
        }
      }
    }
  }

  if (plot.dendrogram) {

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
