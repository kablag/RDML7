## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----load---------------------------------------------------------------------
library(RDML7)

## ----example-file-------------------------------------------------------------
filename <- system.file(
  "extdata",
  "lc96_bACTXY.rdml",
  package = "RDML7"
)

filename

## ----read-rdml----------------------------------------------------------------
lc96 <- readRDML(filename)

## ----explicit-format, eval=FALSE----------------------------------------------
# lc96 <- readRDML(
#   filename,
#   format = "rdml"
# )

## ----formats------------------------------------------------------------------
formats <- listRDMLFormats()
formats

## ----detect-format------------------------------------------------------------
detectRDMLFormat(
  filename,
  operation = "read"
)

## ----s7-access----------------------------------------------------------------
lc96$version

lc96$experiment[[1]]$id$id

## ----keyed-lists--------------------------------------------------------------
expname <- names(lc96$experiment)[1]

experiment <- lc96$experiment[[expname]]

names(experiment$run)

## ----summary, eval=FALSE------------------------------------------------------
# summary(lc96)

## ----validate, eval=FALSE-----------------------------------------------------
# validateRDML(lc96)
# 
# isValidRDML(lc96)

## ----dendrogram, eval=FALSE---------------------------------------------------
# asDendrogram(lc96)

## ----as-table-----------------------------------------------------------------
tbl <- asTable(lc96)

head(tbl)

## ----as-table-columns---------------------------------------------------------
tbl_small <- asTable(
  lc96,
  columns = c(
    "expId",
    "runId",
    "position",
    "sample",
    "sampleType",
    "target"
  )
)

head(tbl_small)

## ----as-table-pattern---------------------------------------------------------
tbl <- asTable(
  lc96,
  namePattern =
    "{expId}_{runId}_{position}_{sample}_{sampleType}_{target}"
)

head(tbl$fdataName)

## ----custom-column------------------------------------------------------------
request <- asTable(
  lc96,
  columns = c(
    "expId",
    "runId",
    "position",
    "reactId",
    "sample",
    "sampleType",
    "target"
  ),
  sampleTarget = paste(
    sample,
    target,
    sep = ":"
))

head(request)

## ----custom-s7-column, eval=FALSE---------------------------------------------
# tbl <- asTable(
#   lc96,
#   cq = data$cq,
#   namePattern =
#     "{position}:{sample}_{target}-{cq}"
# )

## ----concentration-column, eval=FALSE-----------------------------------------
# 
# r96 <- readRDML(system.file(
#   "extdata",
#   "cd53_efficiency.r96",
#   package = "RDML7"
# ))
# concValues <- c("1", "0.5", "0.1", "0.01")
# 
# tbl <- asTable(
#   r96,
#   concentration = {
#     parts <- strsplit(sample, "_", fixed = TRUE)[[1]]
# 
#     value <- parts[parts %in% concValues]
# 
#     if (length(value)) {
#       as.numeric(value[[1]])
#     } else {
#       NA_real_
#     }
#   }
# )
# 
# head(tbl)

## ----get-adp------------------------------------------------------------------
amp <- getFData(lc96, request)

head(amp)

## ----get-mdp------------------------------------------------------------------
biorad <- readRDML(system.file(
  "extdata",
  "BioRad_qPCR_melt.rdml",
  package = "RDML7"
))
melt <- getFData(
  biorad,
  dpType = "mdp"
)

head(melt)

## ----get-long-----------------------------------------------------------------
ampLong <- getFData(lc96, longTable = TRUE)

head(ampLong)

## ----filtering----------------------------------------------------------------
stdRequest <- request[
  request$sampleType == "std",
]

stdAmp <- getFData(
  lc96,
  request = stdRequest,
  dpType = "adp",
  longTable = TRUE
)

head(stdAmp)

## ----plot-curves, eval=FALSE--------------------------------------------------
# library(ggplot2)
# 
# ggplot(
#   stdAmp,
#   aes(
#     x = cyc,
#     y = fluor,
#     group = fdataName,
#     colour = target
#   )
# ) +
#   geom_line()

## ----create-from-tables-------------------------------------------------------
fdata <- data.frame(
  cyc = 1:6,
  c1 = c(10, 11, 13, 18, 35, 80),
  c2 = c(9, 10, 12, 16, 29, 65)
)

description <- data.frame(
  fdataName = c("c1", "c2"),
  expId = c("exp1", "exp1"),
  runId = c("run1", "run1"),
  reactId = c("1", "2"),
  sample = c("sample1", "sample2"),
  sampleType = c("unkn", "unkn"),
  target = c("gene1", "gene1"),
  targetDyeId = c("FAM", "FAM"),
  stringsAsFactors = FALSE
)

sim <- rdmlType()

sim <- setFData(
  sim,
  fdata = fdata,
  description = description,
  fdataType = "adp"
)

asTable(sim)

## ----set-conflicts, eval=FALSE------------------------------------------------
# sim <- setFData(
#   sim,
#   fdata,
#   description,
#   fdataType = "adp",
#   conflict = "replace"
# )

## ----vendor-import, eval=FALSE------------------------------------------------
# x <- readRDML("instrument-export-file")

## ----vendor-import-explicit, eval=FALSE---------------------------------------
# x <- readRDML(
#   "instrument-export-file",
#   format = "registered-format-name"
# )

## ----show-formats-again-------------------------------------------------------
listRDMLFormats()

## ----rdes-files---------------------------------------------------------------
rdesAmp <- system.file(
  "extdata",
  "RDES_v1_0_example_amplification.tsv",
  package = "RDML7"
)

rdesMelt <- system.file(
  "extdata",
  "RDES_v1_0_example_melting.tsv",
  package = "RDML7"
)

c(rdesAmp, rdesMelt)

## ----read-rdes, eval=FALSE----------------------------------------------------
# rdes <- readRDML(rdesAmp)

## ----merge, eval=FALSE--------------------------------------------------------
# file1 <- system.file(
#   "extdata",
#   "lc96_bACTXY.rdml",
#   package = "RDML7"
# )
# 
# file2 <- system.file(
#   "extdata",
#   "stepone_std.rdml",
#   package = "RDML7"
# )
# 
# base <- readRDML(file1)
# incoming <- readRDML(file2)
# 
# merged <- mergeRDMLs(
#   base,
#   incoming,
#   dataConflict = "error"
# )
# 
# asDendrogram(merged)

## ----merge-directory, eval=FALSE----------------------------------------------
# files <- list.files(
#   system.file("extdata", package = "RDML7"),
#   full.names = TRUE
# )
# 
# files <- files[!file.info(files)$isdir]
# 
# rdmls <- lapply(files, function(file) readRDML(file))
# merged <- mergeRDMLs(
#     rdmls,
#     dataConflict = "error"
#   )
# 
# merged <- mergeRDMLs(
#     rdmls
#   )
# rm(rdmls)
# 
# asDendrogram(merged)
# rm(merged)

## ----as-xml, eval=FALSE-------------------------------------------------------
# xml <- asXML(r96)
# 
# cat(
#   substr(
#     xml,
#     1,
#     500
#   )
# )

## ----write-rdml, eval=FALSE---------------------------------------------------
# writeRDML(
#   lc96,
#   "lc96-copy.rdml",
#   overwrite = TRUE
# )

## ----export-workflow, eval=FALSE----------------------------------------------
# validation <- validateRDML(lc96)
# 
# if (isValidRDML(lc96)) {
#   writeRDML(
#     lc96,
#     "validated.rdml",
#     overwrite = TRUE
#   )
# }

## ----register-format-form, eval=FALSE-----------------------------------------
# registerRDMLFormat(
#   name,
#   extensions = character(),
#   reader = NULL,
#   writer = NULL,
# )

## ----register-format-example, eval=TRUE---------------------------------------
# simple-qpcr-reader.R -----------------------------------------------------
#
# Example third-party reader for RDML7's minimal format registry.

readSimpleQPCR <- function(fileName, ...) {
  lines <- readLines(
    fileName,
    warn = FALSE,
    encoding = "UTF-8"
  )

  section <- function(name) {
    start <- match(
      paste0("[", name, "]"),
      lines
    )

    if (is.na(start)) {
      stop(
        "Missing [",
        name,
        "] section",
        call. = FALSE
      )
    }

    nextSections <- grep(
      "^\\[[^]]+\\]$",
      lines
    )

    end <- nextSections[
      nextSections > start
    ]

    end <- if (length(end)) {
      end[[1L]] - 1L
    } else {
      length(lines)
    }

    out <- lines[
      seq.int(
        start + 1L,
        end
      )
    ]

    out[nzchar(trimws(out))]
  }

  # [meta] ---------------------------------------------------------------
  metaLines <- section("meta")

  meta <- strsplit(
    metaLines,
    "=",
    fixed = TRUE
  )

  meta <- stats::setNames(
    vapply(
      meta,
      function(x) {
        paste(
          x[-1L],
          collapse = "="
        )
      },
      character(1)
    ),
    vapply(
      meta,
      `[[`,
      character(1),
      1L
    )
  )

  requiredMeta <- c(
    "experiment",
    "run",
    "target",
    "dye"
  )

  missingMeta <- setdiff(
    requiredMeta,
    names(meta)
  )

  if (length(missingMeta)) {
    stop(
      "Missing metadata: ",
      paste(
        missingMeta,
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  # [reactions] ----------------------------------------------------------
  reactions <- utils::read.delim(
    text = paste(
      section("reactions"),
      collapse = "\n"
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  requiredReactions <- c(
    "fdataName",
    "reactId",
    "sample",
    "sampleType",
    "quantity"
  )

  missingReactions <- setdiff(
    requiredReactions,
    names(reactions)
  )

  if (length(missingReactions)) {
    stop(
      "Missing reaction columns: ",
      paste(
        missingReactions,
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  # [adp] ----------------------------------------------------------------
  fdata <- utils::read.delim(
    text = paste(
      section("adp"),
      collapse = "\n"
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if (!identical(names(fdata)[[1L]], "cyc")) {
    stop(
      "First [adp] column must be `cyc`",
      call. = FALSE
    )
  }

  missingCurves <- setdiff(
    reactions$fdataName,
    names(fdata)
  )

  if (length(missingCurves)) {
    stop(
      "Missing fluorescence columns: ",
      paste(
        missingCurves,
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  description <- data.frame(
    fdataName = reactions$fdataName,
    expId = meta[["experiment"]],
    runId = meta[["run"]],
    reactId = as.character(
      reactions$reactId
    ),
    sample = reactions$sample,
    sampleType = reactions$sampleType,
    target = meta[["target"]],
    targetDyeId = meta[["dye"]],
    quantity = as.numeric(
      reactions$quantity
    ),
    stringsAsFactors = FALSE
  )

  buildRDMLFromFData(
    fdata = fdata,
    description = description,
    fdataType = "adp",
    publisher = "simpleqpcr"
  )
}


registerRDMLFormat(
  name = "simpleqpcr",
  extensions = "sqpcr",
  reader = readSimpleQPCR
)


## ----custom-format-dispatch, eval=TRUE----------------------------------------
sqpcr <- system.file(
  "extdata/sqpcr",
  "cd53_efficiency.sqpcr",
  package = "RDML7"
)
sqpcr <- readRDML(sqpcr)
summary(sqpcr)

## ----unregister-format, eval=TRUE---------------------------------------------
unregisterRDMLFormat("simpleqpcr")

## ----rdml-edit, eval=FALSE----------------------------------------------------
# editRDML(lc96)

## ----downstream, eval=TRUE----------------------------------------------------
curves <- getFData(
  lc96,
  dpType = "adp",
  longTable = FALSE
)

# The first column contains cycle coordinates.
cycles <- curves[[1]]

# Remaining columns contain fluorescence series and can be passed to
# modelling, preprocessing, Cq estimation, or visualisation code.

## ----workflow, eval=FALSE-----------------------------------------------------
# library(RDML7)
# 
# # 1. Import
# x <- readRDML("experiment.rdml")
# 
# # 2. Inspect and validate
# summary(x)
# validateRDML(x)
# 
# # 3. Build the curve index / metadata table
# meta <- asTable(
#   x,
#   namePattern =
#     "{expId}_{runId}_{position}_{sample}_{sampleType}_{target}"
# )
# 
# # 4. Select curves
# selected <- meta[
#   meta$sampleType == "std" &
#     meta$adp,
# ]
# 
# # 5. Extract fluorescence
# amp <- getFData(
#   x,
#   request = selected,
#   dpType = "adp",
#   longTable = TRUE
# )
# 
# # 6. Analyse or modify data
# # ...
# 
# # 7. Write a new RDML file if needed
# writeRDML(
#   x,
#   "experiment-processed.rdml",
#   overwrite = TRUE
# )

