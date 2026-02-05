rdml.env <- new.env(parent = emptyenv())

# XML parsing helpers -------------------------------------------------
getTextValue <- function(tree, path, ns = rdml.env$ns) {
  node <- xml_find_first(tree, path, ns)
  if (class(node) == "xml_missing")
    #return(NULL)
    return(NA)
  xml_text(node)
}

getTextVector <- function(tree, path, ns = rdml.env$ns) {
  xml_text(xml_find_all(tree, path, ns))
}

getLogicalValue <- function(tree, path, ns = rdml.env$ns) {
  node <- xml_find_first(tree, path, ns)
  if (class(node) == "xml_missing")
    return(NULL)
  as.logical(xml_text(node))
}

getNumericValue <- function(tree, path, ns = rdml.env$ns) {
  node <- xml_find_first(tree, path, ns)
  if (class(node) == "xml_missing")
    return(NULL)
  as.numeric(xml_text(node))
}

# getNumericVector <- function(tree, path, ns = rdml.env$ns) {
#   node.set <- xml_find_all(tree, path, ns)
#   if (length(node.set) == 0)
#     return(NULL)
#   list.mapv(node.set,
#             as.numeric(xml_text(.)))
# }

getNumericVector <- function(tree, path, ns = rdml.env$ns) {
  as.numeric(
    xml_text(xml_find_all(tree, path, ns)))
}

getIntegerValue <- function(tree, path, ns = rdml.env$ns) {
  node <- xml_find_first(tree, path, ns)
  if (class(node) == "xml_missing")
    return(NULL)
  xml_integer(node)
}

getIntegerVector <- function(tree, path, ns = rdml.env$ns) {
  xml_integer(xml_find_all(tree, path, ns))
}

genId <- function(node) {
  idType(id = xml_attr(node, "id"))
}

genIdRef <- function(node) {
  idReferenceType(id = xml_attr(node, "id"))
}

as.numeric <- function(val) {
  out <- tryCatch(
    base::as.numeric(val),
    warning = function(w) {
      base::as.numeric(gsub(",", ".", val))
    }
  )
  if (length(out))
    return(out)
  NULL
}

as.integer <- function(val) {
  out <- base::as.integer(val)
  if (length(out))
    return(out)
  NULL
}

# Misc functions -------------------------------------------------
ns <- NULL

FromPositionToId <- 
  function(react.id, 
           pcrFormat = pcrFormatType(
             rows = 8L, columns = 12L, 
             rowLabel = labelFormatType("ABC"), 
             columnLabel = labelFormatType("123"))) {
    row <- which(LETTERS ==
                   gsub("([A-Z])[0-9]+", "\\1", react.id))
    col <- as.integer(gsub("[A-Z]([0-9]+)", "\\1", react.id))
    # as.character((row - 1) * 12 + col)
    (row - 1) * pcrFormat$columns + col
  }

GetIds <- function(l) {
  unname(sapply(l, function(el) el$id))
}

# Gets concentrations (quantity) of each
# dilution from XML for Roche
GetDilutionsRoche <- function(uniq.folder)
{
  # cat("\nParsing Roche standards data...")
  if (!file.exists(paste0(uniq.folder,"/calculated_data.xml"))) {
    # cat("NO SUCH FILE")
    return(NA)
  }
  rdml.doc <- read_xml(paste0(uniq.folder,"/calculated_data.xml"))
  if (length(xml_ns(rdml.doc)) != 9) {
    return(NULL)
  }
  rdml.env$ns <- xml_ns_rename(xml_ns(rdml.doc), d1 = "calc", d2 = "analys", d3 = "quant")
  # xml_ns_strip(rdml.doc)
  concs <- getNumericVector(rdml.doc, "//quant:absQuantDataSource/quant:standard")
  if (length(concs) == 0) {
    concs <- getNumericVector(rdml.doc, "//quant:relQuantDataSource/quant:standard")
    concs.guids <-
      getTextVector(rdml.doc, "//quant:relQuantDataSource/standard/../quant:graphId")
  } else {
    concs.guids <-
      getTextVector(rdml.doc, "//quant:absQuantDataSource/quant:standard/../quant:graphId")
  }
  if (is.null(concs))
    return(NULL)
  names(concs) <- concs.guids
  concs <- sort(concs, decreasing = TRUE)
  positions <-
    getTextVector(rdml.doc,
                  "//quant:standardPoints/quant:standardPoint/quant:position")
  positions <- sapply(positions, FromPositionToId)
  dye.names <- getTextVector(rdml.doc,
                             "//quant:standardPoints/quant:standardPoint/quant:dyeName")
  positions.guids <-
    getTextVector(rdml.doc,
                  "//quant:standardPoints/quant:standardPoint/quant:graphIds/quant:guid")
  positions.table <- matrix(c(dye.names,
                              positions),
                            ncol = length(positions),
                            nrow = 2,
                            byrow = TRUE,
                            dimnames = list(c("dye.name","position"),
                                            positions.guids))
  positions.table <- positions.table[,
                                     order(match(colnames(positions.table), names(concs)))]
  positions.table <- rbind(positions.table, conc = concs)
  dyes <- unique(positions.table["dye.name",])
  dilutions <- list.map(dyes,
                        dye ~ {
                          dye.group.indecies <- which(positions.table["dye.name",] == dye)
                          concs.by.dye <- concs[dye.group.indecies]
                          names(concs.by.dye) <- positions.table["position",
                                                                 dye.group.indecies]
                          concs.by.dye
                        })
  if (length(dilutions) == 0) {
    return(NULL)
  }
  names(dilutions) <- dyes
  return(dilutions)
}

GetConditionsRoche <- function(uniq.folder)
{
  # cat("\nParsing Roche conditions data...")
  if (!file.exists(paste0(uniq.folder, "/app_data.xml"))) {
    # cat("NO SUCH FILE")
    return(NA)
  }
  rdml.doc <- read_xml(paste0(uniq.folder, "/app_data.xml"))
  rdml.env$ns <- xml_ns_rename(xml_ns(rdml.doc), d1 = "lc96")
  nodes <- xml_find_all(rdml.doc,
                        "/lc96:rocheLC96AppExtension/lc96:experiment/lc96:run/lc96:react/lc96:condition/..",
                        ns = rdml.env$ns)
  reacts <- xml_attr(nodes, "id")
  conditions <- getTextVector(nodes, "lc96:condition")
  if (length(conditions) == 0) {
    # cat("NONE")
    return(NULL)
  }
  names(conditions) <- reacts
  # cat("OK")
  return(conditions)
}

GetRefGenesRoche <- function(uniq.folder)
{
  # cat("\nParsing Roche reference genes data...")
  if (!file.exists(paste0(uniq.folder, "/module_data.xml"))) {
    # cat("NO SUCH FILE")
    return(NA)
  }
  rdml.doc <- read_xml(paste0(uniq.folder, "/module_data.xml"))
  rdml.env$ns <- xml_ns_rename(xml_ns(rdml.doc), d3 = "rel")
  ref <- xml_find_all(rdml.doc,
                      "//rel:geneSettings/rel:relQuantGeneSettings",
                      ns = rdml.env$ns)
  if (length(ref) == 0) {
    # cat("NONE")
    return(NULL)
  }
  return(ref)
}

# RDML init -------------------------------------------------
#' Creates new instance of \code{RDML} class object
#'
#' This function has been designed to import data from RDML v1.1 and v1.2 format
#' files or from \code{xls} file generated by \emph{Applied Biosystems 7500}. To
#' import from \code{xls} this file have to contain \code{Sample Setup} and
#' \code{Multicomponent Data} sheets!
#'
#' File format options: \describe{\item{auto}{Tries to detect format by
#' extension. \code{.xlsx} -- \code{excel}, \code{.xls} -- \code{abi},
#' \code{.csv} -- \code{csv}, other -- \code{rdml}}\item{abi}{Reads \code{.xls}
#' files generated by \emph{ABI 7500 v.2}. To create such files use File>Export;
#' check 'Sample Setup' and 'Multicomponent Data'; select 'One File'}
#' \item{excel}{\code{.xls} or \code{.xslx} file with sheets 'description',
#' 'adp', 'mdp'. See example file \code{table.xlsx}}\item{csv}{\code{.csv} file
#' with first column 'cyc' or 'tmp' and fluorescence data in other columns}
#' \item{rdml}{\code{.rdml} or \code{.lc96p} files}}
#'
#'
#' @section Warning: Although the format RDML claimed as data exchange format,
#'   the specific implementation of the format at devices from real
#'   manufacturers differ significantly. Currently this function is checked
#'   against RDML data from devices: \emph{Bio-Rad CFX96}, \emph{Roche
#'   LightCycler 96} and \emph{Applied Biosystems StepOne}.
#' @param filename \code{string} -- path to file
#' @param show.progress \code{logical} -- show loading progress bar if
#'   \code{TRUE}
#' @param conditions.sep separator for condition defined at sample name
#' @param format \code{string} -- input file format. Possible values
#'   \code{auto}, \code{rdml}, \code{abi}, \code{excel}, \code{csv}. See
#'   Details.
#' @author Konstantin A. Blagodatskikh <k.blag@@yandex.ru>, Stefan Roediger
#'   <stefan.roediger@@b-tu.de>, Michal Burdukiewicz
#'   <michalburdukiewicz@@gmail.com>
#' @docType methods
#' @name new
#' @aliases RDML.new
#' @rdname new-method
#' @importFrom tools file_ext
#' @importFrom readxl read_excel
#' @importFrom lubridate ymd_hms ymd
#' @include RDML.R
#' @examples
#' \dontrun{
#' ## Import from RDML file
#' PATH <- path.package("RDML")
#' filename <- paste(PATH, "/extdata/", "lc96_bACTXY.rdml", sep ="")
#' lc96 <- RDML$new(filename)
#'
#' ## Some kind of overview for lc96
#' lc96$AsTable(name.pattern = sample[[react$sample$id]]$description)
#' lc96$AsDendrogram()
#' }



#' Read RDML file and return S7 rdmlType object
#' @export
rdml_read <- function(filename,
                      show.progress = TRUE,
                      conditions.sep = NULL,
                      cluster = NULL,
                      format = "auto") {
  if (missing(filename)) stop("filename is required")
  checkmate::assertString(filename)
  
  # local storage for rdmlType properties
  rdml_obj <- list(
    version = NA_character_,
    dateMade = as.character(NA),
    dateUpadted = as.character(NA),
    id = list(),
    experimenter = list(),
    documentation = list(),
    dye = list(),
    sample = list(),
    target = list(),
    thermalCyclingConditions = list(),
    experiment = list()
  )
  
  
  as_table_rdml <- function(rdml_obj) {
    # Minimal mapping used by Roche-import postprocessing:
    # returns data.table with columns react.id and sample (sample id as string)
    if (!requireNamespace("data.table", quietly = TRUE)) {
      stop("Package 'data.table' is required for Roche postprocessing")
    }
    dt <- data.table::data.table(react.id = integer(), sample = character())
    if (length(rdml_obj$experiment) == 0) return(dt)
    for (exp in rdml_obj$experiment) {
      if (is.null(exp@run) || length(exp@run) == 0) next
      for (run in exp@run) {
        if (is.null(run@react) || length(run@react) == 0) next
        for (react in run@react) {
          # react@sample is idReferenceType; store its ref as character
          smp <- tryCatch(react@sample@ref, error = function(e) NA_character_)
          dt <- data.table::rbindlist(list(dt, data.table::data.table(
            react.id = as.integer(react@id),
            sample = as.character(smp)
          )), use.names = TRUE)
        }
      }
    }
    dt
  }
  
  
  fromRDML <- function() {
    rdml.env$ns <- NULL
    # Unzips RDML to unique folder to get inner XML content.
    # Unique folder is needed to prevent file ovewriting
    # by parallel function usage.
    uniq.folder <- tempfile() #paste0(tempdir(), UUIDgenerate())
    # cat(sprintf("Unzipping %s...", filename))
    unzipped.rdml <- unzip(filename, exdir = uniq.folder)
    dilutions.r <- NULL
    ref.genes.r <- NULL
    
    # tryCatch({
    # Roche use more than one file at RDML zip.
    # One of the files store dilutions information.
    if (length(unzipped.rdml) > 1) {
      # cat("\nParsing Roche(?) data...")
      rdml.doc <- read_xml(paste0(uniq.folder,"/rdml_data.xml"))
      dilutions.r <- GetDilutionsRoche(uniq.folder)
      conditions.r <- GetConditionsRoche(uniq.folder)
      ref.genes.r <- GetRefGenesRoche(uniq.folder)
      rdml.env$ns <- xml_ns_rename(xml_ns(rdml.doc), "d1" = "rdml")
    } else {
      # cat("\nParsing data...")
      rdml.doc <- read_xml(unzipped.rdml)
      rdml.env$ns <- xml_ns(rdml.doc)
      if (!("rdml" %in% names(rdml.env$ns))) {
        rdml.env$ns <- xml_ns_rename(xml_ns(rdml.doc), d1 = "rdml")
      }
    }
    # },
    # error = function(e) { stop(e) },
    # finally = unlink(uniq.folder, recursive = TRUE)
    # )
    unlink(uniq.folder, recursive = TRUE)
    
    # dateMade -----------------------------------------------------------------
    # cat("\nGetting dateMade")
    rdml_obj$dateMade <- getTextValue(rdml.doc, "/rdml:rdml/rdml:dateMade")
    
    # dateUpdated -----------------------------------------------------------------
    # cat("\nGetting dateUpdated")
    rdml_obj$dateUpadted <- getTextValue(rdml.doc, "/rdml:rdml/rdml:dateUpdated")
    
    # id -----------------------------------------------------------------
    # cat("\nGetting id")
    rdml_obj$id <-
      list.map(rdml.doc %>>% xml_find_all("/rdml:rdml/rdml:id", rdml.env$ns),
               id ~
                 rdmlIdType(publisher = getTextValue(tree = id, path = "rdml:publisher"), serialNumber = getTextValue(tree = id, path = "rdml:serialNumber"), MD5Hash = getTextValue(tree = id, path = "rdml:MD5Hash"))
      ) %>>%
      list.names(.$publisher)
    # cat("\nGetting experementer")
    rdml_obj$experimenter <- {
      # experimenter.list <-
      list.map(rdml.doc %>>%
                 xml_find_all("/rdml:rdml/rdml:experimenter", rdml.env$ns),
               experimenter ~
                 experimenterType(id = genId(experimenter), firstName = getTextValue(experimenter, "rdml:firstName"), lastName = getTextValue(experimenter, "rdml:lastName"), email = getTextValue(experimenter, "rdml:email"), labName = getTextValue(experimenter, "rdml:labName"), labAddress = getTextValue(experimenter, "rdml:labAddress"))) %>>%
        list.names(.$id@id)
    }
    
    # documentation -----------------------------------------------------------------
    # cat("\nGetting documentation")
    rdml_obj$documentation <- {
      # documentation.list <-
      list.map(rdml.doc %>>%
                 xml_find_all("/rdml:rdml/rdml:documentation", rdml.env$ns),
               documentation ~
                 documentationType(id = genId(documentation), text = getTextValue(documentation, "rdml:text"))) %>>%
        list.names(.$id@id)
    }
    
    # dye -----------------------------------------------------------------
    # cat("\nGetting dye")
    rdml_obj$dye <- {
      list.map(rdml.doc %>>%
                 xml_find_all("/rdml:rdml/rdml:dye", rdml.env$ns),
               dye ~ dyeType(id = genId(dye), description = getTextValue(dye, "rdml:description"))) %>>%
        list.names(.$id@id)
    }
    
    # sample -----------------------------------------------------------------
    # cat("\nGetting sample")
    rdml_obj$sample <-
      list.map(rdml.doc %>>%
                 xml_find_all("/rdml:rdml/rdml:sample", rdml.env$ns),
               sample ~
                 {
                   type <- getTextValue(sample, "rdml:type")
                   
                   # remove Roche omitted ('ntp') samples
                   if(type == "ntp")
                     return(NULL)
                   id <- xml_attr(sample, "id")
                   sampleType(id = idType(id = id), description = getTextValue(sample, "rdml:description"), documentation =
                                lapply(sample %>>%
                                         xml_find_all("rdml:documentation", rdml.env$ns),
                                       function(doc) genIdRef(doc)), xRef =
                                list.map(sample %>>%
                                           xml_find_all("rdml:xRef", rdml.env$ns),
                                         xRef ~ xRefType(name = getTextValue(xRef, "rdml:name"), id = getTextValue(xRef, "rdml:id"))), annotation = c(
                                           list.map(sample %>>%
                                                      xml_find_all("rdml:annotation", rdml.env$ns),
                                                    annotation ~ annotationType(property = getTextValue(annotation, "rdml:property"), value = getTextValue(annotation, "rdml:value"))),
                                           if (!is.null(conditions.sep)) {
                                             val <- gsub(sprintf("^.*%s(.*)$",
                                                                 conditions.sep),
                                                         "\\1", id)
                                             if (length(val) != 0) {
                                               annotationType(property = "condition", value = val)
                                             }
                                           }), type = sampleTypeType(value = type), interRunCalibrator =
                                getLogicalValue(sample, "rdml:interRunCalibrator"), quantity =
                                tryCatch(
                                  quantityType(value = getNumericValue(sample, "rdml:quantity/rdml:value"), unit = quantityUnitType(value = getTextValue(sample, "rdml:quantity/rdml:unit"))),
                                  error = function(e) NULL
                                ), calibratorSample =
                                getLogicalValue(sample, "rdml:calibaratorSample"), cdnaSynthesisMethod = cdnaSynthesisMethodType(enzyme = getTextValue(sample, "rdml:cdnaSynthesisMethod/rdml:enzyme"), primingMethod =
                                                                                                                                   getNumericValue(sample, "rdml:cdnaSynthesisMethod/rdml:primingMethod"), dnaseTreatment = getLogicalValue(sample, "rdml:cdnaSynthesisMethod/rdml:dnaseTreatment"), thermalCyclingConditions =
                                                                                                                                   tryCatch(
                                                                                                                                     genIdRef(xml_find_first(sample,
                                                                                                                                                             "rdml:cdnaSynthesisMethod/rdml:thermalCyclingConditions",
                                                                                                                                                             ns = rdml.env$ns)),
                                                                                                                                     error = function(e) NULL)), templateQuantity =
                                tryCatch(
                                  templateQuantityType(conc = getNumericValue(sample, "rdml:templateQuantity/rdml:conc"), nucleotide = nucleotideType(value = getTextValue(sample, "rdml:templateQuantity/rdml:nucleotide"))),
                                  error = function(e) NULL
                                ))
                 }) %>>%
      list.filter(!is.null(.)) %>>%
      list.names(.$id@id)
    
    # target -----------------------------------------------------------------
    # cat("\nGetting target")
    rdml_obj$target <-
      list.map(rdml.doc %>>%
                 xml_find_all("/rdml:rdml/rdml:target", rdml.env$ns),
               target ~ {
                 targetType(
                   id = xml_attr(target, "id") %>>%
                     (id ~ idType(ifelse(length(unzipped.rdml) > 1 &&
                                           length(rdml_obj$id) != 0 &&
                                           rdml_obj$id[[1]]$publisher == "Roche Diagnostics",
                                         {
                                           gsub("@(.+)$", "\\1",
                                                regmatches(id, gregexpr("@(.+)$", id))[[1]])
                                         },
                                         id))),
                   description = getTextValue(target, "rdml:description"),
                   documentation =
                     lapply(target %>>%
                              xml_find_all("rdml:documentation", rdml.env$ns),
                            function(doc) genIdRef(doc)),
                   xRef =
                     list.map(target %>>%
                                xml_find_all("rdml:xRef", rdml.env$ns),
                              xRef ~
                                xRefType(name = getTextValue(xRef, "rdml:name"), 
                                         id = getTextValue(xRef, "rdml:id"))), type = targetTypeType(value = getTextValue(target, "rdml:type")), 
                   amplificationEfficiencyMethod =
                     getTextValue(target, "rdml:amplificationEfficiencyMethod"), amplificationEfficiency =
                     getNumericValue(target, "rdml:amplificationEfficiency"), amplificationEfficiencySE =
                     getNumericValue(target, "rdml:amplificationEfficiencySE"), detectionLimit =
                     getNumericValue(target, "rdml:detectionLimit"), dyeId =
                     tryCatch(
                       target %>>%
                         xml_find_first("rdml:dyeId", rdml.env$ns) %>>%
                         genIdRef(),
                       # StepOne stores dyeId as xml value
                       error = function(e)
                         idReferenceType(id = getTextValue(target, "rdml:dyeId"))
                     ), # dyeId = NA, 
                   sequences = 
                     new_object(sequencesType,
                                forwardPrimer =
                                  tryCatch(
                                    oligoType(threePrimeTag =
                                                getTextValue(target, "rdml:sequences/rdml:forwardPrimer/rdml:threePrimeTag"), fivePrimeTag =
                                                getTextValue(target, "rdml:sequences/rdml:forwardPrimer/rdml:fivePrimeTag"), sequence =
                                                getTextValue(target, "rdml:sequences/rdml:forwardPrimer/rdml:sequence")),
                                    error = function(e) NULL
                                  ), 
                                reversePrimer =
                                  tryCatch(
                                    oligoType(threePrimeTag =
                                                getTextValue(target, "rdml:sequences/rdml:reversePrimer/rdml:threePrimeTag"), fivePrimeTag =
                                                getTextValue(target, "rdml:sequences/rdml:reversePrimer/rdml:fivePrimeTag"), sequence =
                                                getTextValue(target, "rdml:sequences/rdml:reversePrimer/rdml:sequence")),
                                    error = function(e) NULL
                                  ), 
                                probe1 =
                                  tryCatch(
                                    oligoType(threePrimeTag =
                                                getTextValue(target, "rdml:sequences/rdml:probe1/rdml:threePrimeTag"), fivePrimeTag =
                                                getTextValue(target, "rdml:sequences/rdml:probe1/rdml:fivePrimeTag"), sequence =
                                                getTextValue(target, "rdml:sequences/rdml:probe1/rdml:sequence")),
                                    error = function(e) NULL
                                  ), 
                                probe2 =
                                  tryCatch(
                                    oligoType(threePrimeTag =
                                                getTextValue(target, "rdml:sequences/rdml:probe2/rdml:threePrimeTag"), fivePrimeTag =
                                                getTextValue(target, "rdml:sequences/rdml:probe2/rdml:fivePrimeTag"), sequence =
                                                getTextValue(target, "rdml:sequences/rdml:probe2/rdml:sequence")),
                                    error = function(e) NULL
                                  ), 
                                amplicon =
                                  tryCatch(
                                    oligoType(threePrimeTag =
                                                getTextValue(target, "rdml:sequences/rdml:amplicon/rdml:threePrimeTag"), fivePrimeTag =
                                                getTextValue(target, "rdml:sequences/rdml:amplicon/rdml:fivePrimeTag"), sequence =
                                                getTextValue(target, "rdml:sequences/rdml:amplicon/rdml:sequence")),
                                    error = function(e) NULL
                                  )), 
                   commercialAssay =
                     tryCatch(
                       commercialAssayType(company =
                                             getTextValue(target, "rdml:commercialAssay/rdml:company"), orderNumber =
                                             getTextValue(target, "rdml:commercialAssay/rdml:orderNumber"))
                       ,
                       error = function(e) NULL
                     )
                 )
               }
      ) %>>%
      list.names(.$id@id)
    
    # thermalCyclingConditions -------------------------------------------------
    # cat("\nGetting thermalCyclingConditions")
    rdml_obj$thermalCyclingConditions <-
      list.map(rdml.doc %>>%
                 xml_find_all("/rdml:rdml/rdml:thermalCyclingConditions", rdml.env$ns),
               tcc ~ {
                 thermalCyclingConditionsType(id = genId(tcc), description = getTextValue(tcc, "rdml:description"), documentation =
                                                lapply(tcc %>>%
                                                         xml_find_all("rdml:documentation", rdml.env$ns),
                                                       function(doc) genIdRef(doc)), lidTemperature =
                                                getNumericValue(tcc, "rdml:lidTemperature"), experimenter =
                                                list.map(tcc %>>%
                                                           xml_find_all("rdml:experimenter", rdml.env$ns),
                                                         experimenter ~ genIdRef(experimenter)
                                                ), step = list.map(tcc %>>%
                                                                     xml_find_all("rdml:step", rdml.env$ns),
                                                                   step ~ {
                                                                     stepType(nr = getIntegerValue(step, "rdml:nr"), description = getTextValue(step, "rdml:description"), temperature = {
                                                                       tryCatch(
                                                                         temperatureType(temperature =
                                                                                           getNumericValue(step, "rdml:temperature/rdml:temperature"), duration =
                                                                                           getIntegerValue(step, "rdml:temperature/rdml:duration"), temperatureChange =
                                                                                           getNumericValue(step, "rdml:temperature/rdml:temperatureChange"), durationChange =
                                                                                           getIntegerValue(step, "rdml:temperature/rdml:durationChange"), measure = measureType(value = getTextValue(step, "rdml:temperature/rdml:measure")), ramp =
                                                                                           getNumericValue(step, "rdml:temperature/rdml:ramp")),
                                                                         error = function(e) NULL)}, gradient = {
                                                                           tryCatch(
                                                                             gradientType(highTemperature =
                                                                                            getNumericValue(step, "rdml:gradient/rdml:highTemperature"), lowTemperature =
                                                                                            getNumericValue(step, "rdml:gradient/rdml:lowTemperature"), duration =
                                                                                            getIntegerValue(step, "rdml:gradient/rdml:duration"), temperatureChange =
                                                                                            getNumericValue(step, "rdml:gradient/rdml:temperatureChange"), durationChange =
                                                                                            getIntegerValue(step, "rdml:gradient/rdml:durationChange"), measure = measureType(value = getTextValue(step, "rdml:gradient/rdml:measure")), ramp =
                                                                                            getNumericValue(step, "rdml:gradient/rdml:ramp")),
                                                                             error = function(e) NULL)
                                                                         }, loop = {
                                                                           tryCatch(
                                                                             loopType(goto = getIntegerValue(step, "rdml:loop/rdml:goto"), # should be called "repeat" but this is reserved word
                                                                                      repeat.n = getIntegerValue(step, "rdml:loop/rdml:repeat")),
                                                                             error = function(e) NULL)}, pause = {
                                                                               tryCatch(
                                                                                 pauseType(temperature =
                                                                                             getNumericValue(step, "rdml:pause/rdml:temperature")),
                                                                                 error = function(e) NULL)}, lidOpen = {
                                                                                   if(is.null(step[["lidOpen"]]))
                                                                                     NULL
                                                                                   else
                                                                                     new_object(lidOpenType)
                                                                                 })
                                                                   }
                                                ))
               }) %>>%
      list.names(.$id@id)
    #     names(tcc.list) <- GetIds(tcc.list)
    #     tcc.list
    # data -------------------------------------------------
    GetData <- function(data) {
      tar.id <-
        data %>>%
        xml_find_first("rdml:tar", rdml.env$ns) %>>%
        xml_attr("id")
      dataC <- as.character(data)
      dataType(tar = idReferenceType(id = if (length(unzipped.rdml) > 1 &&
                                              length(rdml_obj$id) != 0 &&
                                              rdml_obj$id[[1]]$publisher == "Roche Diagnostics")
        gsub("@(.+)$", "\\1",
             regmatches(tar.id, gregexpr("@(.+)$", tar.id))[[1]])
        else
        {
          if (is.na(tar.id))
            "NA"
          else
            tar.id
        }), cq = getNumericValue(data, "rdml:cq"), excl = getTextValue(data, "rdml:excl"), adp = {
          fpoints <- str_match_all(dataC,
                                   "<adp>\\\\?n?\\s*<cyc>(.*)</cyc>\\\\?n?\\s*<tmp>(.*)</tmp>\\\\?n?\\s*<fluor>(.*)</fluor>\\\\?n?\\s*</adp>")[[1]][, -1]
          
          if (length(fpoints)) {
            # check for duplicate cycles. Occures in StepOne RDML files.
            # if (base::anyDuplicated(fpoints$cyc)) {
            #   fpoints <- fpoints[-base::duplicated(fpoints$cyc)]
            #   warning("Duplicate cycles removed")
            # }
            dpAmpCurveType(# data.table(cyc = cyc, tmp = tmp, fluor = fluor)
              data.table(cyc = as.numeric(fpoints[, 1]), 
                         tmp = as.numeric(fpoints[, 2]), 
                         fluor = as.numeric(fpoints[, 3])))
          } else {
            fpoints <- str_match_all(dataC,
                                     "<adp>\\\\?n?\\s*<cyc>(.*)</cyc>\\\\?n?\\s*<fluor>(.*)</fluor>\\\\?n?\\s*</adp>")[[1]][, -1]
            if (length(fpoints)) {
              dpAmpCurveType(data.table(cyc = as.numeric(fpoints[, 1]),
                                        fluor = as.numeric(fpoints[, 2])))
            } else {
              NULL
            }
          }
        }, mdp = {
          fpoints <- str_match_all(dataC,
                                   "<mdp>\\\\?n?\\s*<tmp>(.*)</tmp>\\\\?n?\\s*<fluor>(.*)</fluor>\\\\?n?\\s*</mdp>")[[1]][, -1]
          if (length(fpoints)) {
            dpMeltingCurveType(data.table(tmp = as.numeric(fpoints[, 1]),
                                          fluor = as.numeric(fpoints[, 2])))
          } else {
            NULL
          }
        }, endPt = getNumericValue(data, "rdml:endPt"), bgFluor = getNumericValue(data, "rdml:bgFluor"), bgFluorSlp = getNumericValue(data, "rdml:bgFluorSp"), quantFluor = getNumericValue(data, "rdml:quantFluor"))
    }
    
    # react -------------------------------------------------
    GetReact <- function(react,
                         pcrFormat = pcrFormatType(
                           rows = 8L, columns = 12L, 
                           rowLabel = labelFormatType("ABC"), 
                           columnLabel = labelFormatType("123"))) {
      react.id <- xml_attr(react, "id")
      react.id.corrected <- tryCatch(
        as.integer(react.id),
        warning = function(w) {
          # if react.id is 'B1' not '13'
          # like in StepOne
          FromPositionToId(react.id, pcrFormat)
        }
      )
      #     cat(sprintf("\nreact: %i", react.id))
      sample <-
        react %>>%
        xml_find_first("rdml:sample", rdml.env$ns) %>>%
        xml_attr("id")
      
      if(length(unzipped.rdml) > 1 &&
         length(rdml_obj$id) != 0 &&
         rdml_obj$id[[1]]$publisher == "Roche Diagnostics") {
        # remove Roche omitted ('ntp') samples
        if(is.null(rdml_obj$sample[[sample]]))
          return(NULL)
        # Better names for Roche
        sample <- rdml_obj$sample[[sample]]$description
      }
      
      reactType(id = idType(id = react.id.corrected), #sample.id
                #       # will be calculated at the end of init
                #       position = NA, sample = idReferenceType(id = sample), data = {
                list.map(react %>>%
                           xml_find_all("rdml:data", rdml.env$ns),
                         data ~ GetData(data)
                )
                # }
      )
    }
    
    # run -------------------------------------------------
    GetRun <- function(run, experiment.id) {
      run.id <- xml_attr(run, "id")
      pcrFormat <- {
        pcrFormatStr <- getTextValue(run, "rdml:pcrFormat")
        # Quantstudio pcrFormat
        if (!is.null(pcrFormatStr) && grepl("well", pcrFormatStr)) {
          if (grepl("96-well", pcrFormatStr)) {
            pcrFormatType(rows = 8, version = 12, rowLabel = labelFormatType(value = "ABC"), columnLabel = labelFormatType(value = "123"))
          } else {
            pcrFormatType(rows = 16, version = 24, rowLabel = labelFormatType(value = "ABC"), columnLabel = labelFormatType(value = "123"))
          }
        } else {# correct RDML pcrFormat
          rows <- getIntegerValue(run, "rdml:pcrFormat/rdml:rows")
          # check for absent of 'pcrFormat' like in StepOne
          if (!is.null(rows) && !is.na(rows)) {
            pcrFormatType(rows = rows, version = getIntegerValue(run, "rdml:pcrFormat/rdml:columns"), rowLabel = labelFormatType(value = getTextValue(run, "rdml:pcrFormat/rdml:rowLabel")), columnLabel = labelFormatType(value = getTextValue(run, "rdml:pcrFormat/rdml:columnLabel")))
          } else {
            pcrFormatType(rows = 8, version = 12, rowLabel = labelFormatType(value = "ABC"), columnLabel = labelFormatType(value = "123"))
          }
        }
      }
      if (show.progress)
        cat(sprintf("\n\trun: %s\n", run.id))
      runType(id = idType(id = run.id), description = getTextValue(run, "rdml:description"), documentation =
                lapply(run %>>%
                         xml_find_all("rdml:documentation", rdml.env$ns),
                       function(doc) genIdRef(doc)), experimenter =
                list.map(run %>>%
                           xml_find_all("rdml:experimenter", rdml.env$ns),
                         experimenter ~ genIdRef(experimenter)
                ), instrument = getTextValue(run, "rdml:instrument"), dataCollectionSoftware =
                tryCatch(
                  dataCollectionSoftwareType(name = getTextValue(run, "rdml:dataCollectionSoftware/rdml:name"), version = getTextValue(run, "rdml:dataCollectionSoftware/rdml:version")),
                  error = function(e) NULL), backgroundDeterminationMethod =
                getTextValue(run, "rdml:backgroundDeterminationMethod"), cqDetectionMethod =
                cqDetectionMethodType(value = getTextValue(run, "rdml:cqDetectionMethod")), thermalCyclingConditions =
                tryCatch(
                  run %>>%
                    xml_find_first("rdml:thermalCyclingConditions", rdml.env$ns) %>>%
                    genIdRef(),
                  error = function(e) NULL), pcrFormat = pcrFormat, runDate = getTextValue(run, "rdml:runDate"), react = 
                list.map(run %>>%
                           xml_find_all("rdml:react", rdml.env$ns),
                         react ~
                           GetReact(react,
                                    pcrFormat)
                ) %>>%
                list.filter(!is.null(.)))
    }
    
    # experiment -------------------------------------------------
    GetExperiment <- function(experiment) {
      experiment.id <- xml_attr(experiment, "id")
      if (show.progress)
        cat(sprintf("\nLoading experiment: %s", experiment.id))
      experimentType(id = idType(id = experiment.id), description = getTextValue(experiment, "rdml:description"), documentation =
                       lapply(experiment %>>%
                                xml_find_all("rdml:documentation", rdml.env$ns),
                              function(doc) genIdRef(doc)), run =
                       list.map(experiment %>>%
                                  xml_find_all("rdml:run", rdml.env$ns),
                                run ~ GetRun(run)
                       ))
    }
    
    
    rdml_obj$experiment <-
      list.map(rdml.doc %>>%
                 xml_find_all("rdml:experiment", rdml.env$ns),
               experiment ~ GetExperiment(experiment)
      ) %>>%
      list.names(.$id@id)
    
    # Combine CFX96 runs to one (by default Bio-Rad use separate run for each dye!---
    if (!is.null(rdml_obj$id) &&
        length(rdml_obj$id) != 0 &&
        !is.null(rdml_obj$id[[1]]$publisher) &&
        rdml_obj$id[[1]]$publisher == "Bio-Rad Laboratories, Inc." &&
        length(rdml_obj$experiment[[1]]$run) > 1) {
      if (show.progress)
        cat("\nCombining Bio-Rad runs\n")
      first.run <- rdml_obj$experiment[[1]]$run[[1]]
      for (run.i in 2:length(rdml_obj$experiment[[1]]$run)){
        current.run <- rdml_obj$experiment[[1]]$run[[run.i]]
        for (react in current.run$react){
          react.id <- as.character(react$id@id)
          if (is.null(first.run$react[[react.id]])) {
            first.run$react[[react.id]] <- react
          } else {
            first.run$react[[react.id]]$data <- c(
              first.run$react[[react.id]]$data,
              react$data[[1]]
            )
          }
        }
      }
      # delete copied runs
      for (run.i in length(rdml_obj$experiment[[1]]$run):2){
        rdml_obj$experiment[[1]]$run[[run.i]] <- NULL
      }
      rdml_obj$experiment[[1]]$run[[1]]$id <- idType(id = "Combined Run")
    }
    
    # Roche LC96 extra parsing -------------------------------------------------
    # parse original!!! Roche files
    if (length(unzipped.rdml) > 1 &&
        length(rdml_obj$id) != 0 &&
        rdml_obj$id[[1]]$publisher == "Roche Diagnostics") {
      for (i in 1:length(rdml_obj$sample)) {
        rdml_obj$sample[[i]]$id <- idType(id = rdml_obj$sample[[i]]$description)
      }
      rdml_obj$sample <- list.names(rdml_obj$sample,
                                    .$id@id)
      
      # cat("Adding Roche ref genes\n")
      if (!is.null(ref.genes.r) &&
          !is.na(ref.genes.r) &&
          length(ref.genes.r) != 0) {
        ns <- xml_ns_rename(xml_ns(ref.genes.r), d3 = "rel")
        list.iter(ref.genes.r,
                  ref.gene ~ {
                    geneName <- getTextValue(ref.gene, "rel:geneName",
                                             ns = ns)
                    geneI <- grep(
                      sprintf("^%s$", geneName),
                      names(rdml_obj$target))
                    rdml_obj$target[[geneI]]$type <-
                      targetTypeType(ifelse(getLogicalValue(ref.gene, "rel:isReference",
                                                            ns = ns),
                                            "ref",
                                            "toi"))
                  })
      }
      # return()
      tbl <- as_table_rdml(rdml_obj) %>>%
        setkey(react.id)
      # cat("Adding Roche quantities\n")
      for (target in dilutions.r %>>% names()) {
        for (r.id in dilutions.r[[target]] %>>% names()) {
          sample.name <- tbl[react.id == as.integer(r.id), sample][1]
          rdml_obj$sample[[sample.name]]$quantity <-
            quantityType(value = unname(dilutions.r[[1]][r.id]), unit = quantityUnitType(value = "other"))
          rdml_obj$sample[[sample.name]]$annotation <-
            c(rdml_obj$sample[[sample.name]]$annotation,
              annotationType(property = sprintf("Roche_quantity_at_%s_%s",
                                                target,
                                                r.id), value = as.character(dilutions.r[[target]][r.id])))
        }
      }
      
      # cat("Adding Roche conditions\n")
      for (r.id in conditions.r %>>% names()) {
        sample.name <- tbl[as.integer(r.id), sample,
                           mult = "first"]
        rdml_obj$sample[[sample.name]]$annotation <-
          c(rdml_obj$sample[[sample.name]]$annotation,
            annotationType(property = sprintf("Roche_condition_at_%s",r.id), value = conditions.r[r.id]))
      }
    }
    
  }
  
  if (format == "auto") {
    format <- tools::file_ext(filename)
    if (identical(format, "")) format <- "rdml"
  }
  
  # At the moment this S7 reader supports RDML/RDML-XML only
  if (!format %in% c("rdml", "xml")) {
    stop("Unsupported format for S7 rdml_read(): ", format,
         ". Use an RDML (.rdml/.xml) file.")
  }
  
  fromRDML()
  
  do.call(S7::new_object, c(list(rdmlType), rdml_obj))
}

