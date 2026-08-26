# Roche helpers ------------------------------------------------------------

GetDilutionsRoche <- function(uniq.folder) {
  path <- file.path(uniq.folder, "calculated_data.xml")
  if (!file.exists(path)) return(NA)

  rdml.doc <- xml2::read_xml(path)
  if (length(xml2::xml_ns(rdml.doc)) != 9L) return(NULL)

  rdml.env$ns <- xml2::xml_ns_rename(
    xml2::xml_ns(rdml.doc),
    d1 = "calc", d2 = "analys", d3 = "quant"
  )

  concs <- getNumericVector(rdml.doc, "//quant:absQuantDataSource/quant:standard")
  if (length(concs) == 0L) {
    concs <- getNumericVector(rdml.doc, "//quant:relQuantDataSource/quant:standard")
    concs.guids <- getTextVector(
      rdml.doc,
      "//quant:relQuantDataSource/standard/../quant:graphId"
    )
  } else {
    concs.guids <- getTextVector(
      rdml.doc,
      "//quant:absQuantDataSource/quant:standard/../quant:graphId"
    )
  }

  if (is.null(concs)) return(NULL)

  names(concs) <- concs.guids
  concs <- sort(concs, decreasing = TRUE)

  positions <- getTextVector(
    rdml.doc,
    "//quant:standardPoints/quant:standardPoint/quant:position"
  )
  positions <- vapply(positions, FromPositionToId, numeric(1))

  dye.names <- getTextVector(
    rdml.doc,
    "//quant:standardPoints/quant:standardPoint/quant:dyeName"
  )
  positions.guids <- getTextVector(
    rdml.doc,
    "//quant:standardPoints/quant:standardPoint/quant:graphIds/quant:guid"
  )

  positions.table <- matrix(
    c(dye.names, positions),
    ncol = length(positions),
    nrow = 2L,
    byrow = TRUE,
    dimnames = list(c("dye.name", "position"), positions.guids)
  )
  positions.table <- positions.table[
    , order(match(colnames(positions.table), names(concs))), drop = FALSE
  ]
  positions.table <- rbind(positions.table, conc = concs)

  dyes <- unique(positions.table["dye.name", ])
  dilutions <- lapply(dyes, function(dye) {
    idx <- which(positions.table["dye.name", ] == dye)
    out <- concs[idx]
    names(out) <- positions.table["position", idx]
    out
  })
  names(dilutions) <- dyes

  if (!length(dilutions)) NULL else dilutions
}

GetConditionsRoche <- function(uniq.folder) {
  path <- file.path(uniq.folder, "app_data.xml")
  if (!file.exists(path)) return(NA)

  rdml.doc <- xml2::read_xml(path)
  rdml.env$ns <- xml2::xml_ns_rename(xml2::xml_ns(rdml.doc), d1 = "lc96")

  nodes <- xml2::xml_find_all(
    rdml.doc,
    "/lc96:rocheLC96AppExtension/lc96:experiment/lc96:run/lc96:react/lc96:condition/..",
    ns = rdml.env$ns
  )
  reacts <- xml2::xml_attr(nodes, "id")
  conditions <- getTextVector(nodes, "lc96:condition")
  if (!length(conditions)) return(NULL)
  names(conditions) <- reacts
  conditions
}

GetRefGenesRoche <- function(uniq.folder) {
  path <- file.path(uniq.folder, "module_data.xml")
  if (!file.exists(path)) return(NA)

  rdml.doc <- xml2::read_xml(path)
  rdml.env$ns <- xml2::xml_ns_rename(xml2::xml_ns(rdml.doc), d3 = "rel")
  ref <- xml2::xml_find_all(
    rdml.doc,
    "//rel:geneSettings/rel:relQuantGeneSettings",
    ns = rdml.env$ns
  )
  if (!length(ref)) NULL else ref
}
