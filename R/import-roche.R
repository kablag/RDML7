# Roche helpers ------------------------------------------------------------

.getDilutionsRoche <- function(uniqFolder) {
  path <- file.path(uniqFolder, "calculated_data.xml")
  if (!file.exists(path)) return(NA)

  rdmlDoc <- xml2::read_xml(path)
  if (length(xml2::xml_ns(rdmlDoc)) != 9L) return(NULL)

  rdmlEnv$ns <- xml2::xml_ns_rename(
    xml2::xml_ns(rdmlDoc),
    d1 = "calc", d2 = "analys", d3 = "quant"
  )

  concs <- .getNumericVector(rdmlDoc, "//quant:absQuantDataSource/quant:standard")
  if (length(concs) == 0L) {
    concs <- .getNumericVector(rdmlDoc, "//quant:relQuantDataSource/quant:standard")
    concsGuids <- .getTextVector(
      rdmlDoc,
      "//quant:relQuantDataSource/standard/../quant:graphId"
    )
  } else {
    concsGuids <- .getTextVector(
      rdmlDoc,
      "//quant:absQuantDataSource/quant:standard/../quant:graphId"
    )
  }

  if (is.null(concs)) return(NULL)

  names(concs) <- concsGuids
  concs <- sort(concs, decreasing = TRUE)

  positions <- .getTextVector(
    rdmlDoc,
    "//quant:standardPoints/quant:standardPoint/quant:position"
  )
  positions <- vapply(positions, .fromPositionToId, numeric(1))

  dyeNames <- .getTextVector(
    rdmlDoc,
    "//quant:standardPoints/quant:standardPoint/quant:dyeName"
  )
  positionsGuids <- .getTextVector(
    rdmlDoc,
    "//quant:standardPoints/quant:standardPoint/quant:graphIds/quant:guid"
  )

  positionsTable <- matrix(
    c(dyeNames, positions),
    ncol = length(positions),
    nrow = 2L,
    byrow = TRUE,
    dimnames = list(c("dye.name", "position"), positionsGuids)
  )
  positionsTable <- positionsTable[
    , order(match(colnames(positionsTable), names(concs))), drop = FALSE
  ]
  positionsTable <- rbind(positionsTable, conc = concs)

  dyes <- unique(positionsTable["dye.name", ])
  dilutions <- lapply(dyes, function(dye) {
    idx <- which(positionsTable["dye.name", ] == dye)
    out <- concs[idx]
    names(out) <- positionsTable["position", idx]
    out
  })
  names(dilutions) <- dyes

  if (!length(dilutions)) NULL else dilutions
}

.getConditionsRoche <- function(uniqFolder) {
  path <- file.path(uniqFolder, "app_data.xml")
  if (!file.exists(path)) return(NA)

  rdmlDoc <- xml2::read_xml(path)
  rdmlEnv$ns <- xml2::xml_ns_rename(xml2::xml_ns(rdmlDoc), d1 = "lc96")

  nodes <- xml2::xml_find_all(
    rdmlDoc,
    "/lc96:rocheLC96AppExtension/lc96:experiment/lc96:run/lc96:react/lc96:condition/..",
    ns = rdmlEnv$ns
  )
  reacts <- xml2::xml_attr(nodes, "id")
  conditions <- .getTextVector(nodes, "lc96:condition")
  if (!length(conditions)) return(NULL)
  names(conditions) <- reacts
  conditions
}

.getRefGenesRoche <- function(uniqFolder) {
  path <- file.path(uniqFolder, "module_data.xml")
  if (!file.exists(path)) return(NA)

  rdmlDoc <- xml2::read_xml(path)
  rdmlEnv$ns <- xml2::xml_ns_rename(xml2::xml_ns(rdmlDoc), d3 = "rel")
  ref <- xml2::xml_find_all(
    rdmlDoc,
    "//rel:geneSettings/rel:relQuantGeneSettings",
    ns = rdmlEnv$ns
  )
  if (!length(ref)) NULL else ref
}
