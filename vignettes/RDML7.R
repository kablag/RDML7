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
lc96

## ----explicit-format, eval=FALSE----------------------------------------------
# lc96 <- readRDML(
#   filename,
#   format = "rdml"
# )

## ----formats------------------------------------------------------------------
formats <- rdmlFormats()
formats

## ----detect-format------------------------------------------------------------
rdmlDetectFormat(
  filename,
  operation = "read"
)

## ----s7-access----------------------------------------------------------------
lc96@version

names(lc96@experiment)

experiment <- lc96@experiment[[1]]
experiment@id@id

## ----keyed-lists--------------------------------------------------------------
names(lc96@experiment)

experiment <- lc96@experiment[[1]]

names(experiment@run)

## ----summary, eval=FALSE------------------------------------------------------
# rdmlSummary(lc96)

## ----validate, eval=FALSE-----------------------------------------------------
# validation <- validateRDML(lc96)
# validation
# 
# rdmlIsValid(lc96)

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
tbl <- asTable(
  lc96,
  columns = c(
    "expId",
    "runId",
    "position",
    "sample",
    "target"
  ),
  sampleTarget = paste(
    sample,
    target,
    sep = ":"
  )
)

head(tbl)

## ----custom-s7-column, eval=FALSE---------------------------------------------
# tbl <- asTable(
#   lc96,
#   cq = S7::prop(data, "cq"),
#   namePattern =
#     "{expId}_{runId}_{position}_{sample}_{target}_{cq}"
# )

## ----concentration-column, eval=FALSE-----------------------------------------
# concValues <- c(
#   "500", "50", "10", "5", "1",
#   "0.5", "0.1", "0.01"
# )
# 
# tbl <- asTable(
#   lc96,
#   concentration = {
#     parts <- strsplit(
#       sample,
#       "_",
#       fixed = TRUE
#     )[[1]]
# 
#     value <- parts[
#       parts %in% concValues
#     ]
# 
#     if (length(value)) {
#       as.numeric(value[[1]])
#     } else {
#       NA_real_
#     }
#   }
# )

## ----get-adp------------------------------------------------------------------
request <- asTable(lc96)

amp <- getFData(
  lc96,
  request = request,
  dpType = "adp"
)

head(amp)

## ----get-mdp------------------------------------------------------------------
melt <- getFData(
  lc96,
  request = request,
  dpType = "mdp"
)

head(melt)

## ----get-long-----------------------------------------------------------------
ampLong <- getFData(
  lc96,
  request = request,
  dpType = "adp",
  longTable = TRUE
)

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
rdmlFormats()

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

## ----merge-directory, eval=FALSE----------------------------------------------
# files <- dir(
#   "path/to/files",
#   full.names = TRUE
# )
# 
# merged <- rdmlType()
# 
# for (file in files) {
#   current <- readRDML(file)
# 
#   merged <- mergeRDMLs(
#     merged,
#     current,
#     dataConflict = "error"
#   )
# }

## ----as-xml, eval=FALSE-------------------------------------------------------
# xml <- asXML(lc96)
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
# if (rdmlIsValid(lc96)) {
#   writeRDML(
#     lc96,
#     "validated.rdml",
#     overwrite = TRUE
#   )
# }

## ----register-format-form, eval=FALSE-----------------------------------------
# rdmlRegisterFormat(
#   name,
#   extensions = character(),
#   reader = NULL,
#   writer = NULL,
#   sniff = NULL,
#   aliases = character(),
#   priority = 0L,
#   apiVersion = 1L,
#   capabilities = character(),
#   overwrite = FALSE
# )

## ----register-format-example, eval=FALSE--------------------------------------
# myReader <- function(fileName, ...) {
#   # Parse the external format and return either:
#   #   1. an rdmlType object, or
#   #   2. an rdmlImportData object.
# }
# 
# mySniffer <- function(fileName) {
#   # Return a confidence score between 0 and 1.
#   0
# }
# 
# rdmlRegisterFormat(
#   name = "myformat",
#   extensions = "myqpcr",
#   reader = myReader,
#   sniff = mySniffer,
#   aliases = "my-device",
#   priority = 10L
# )

## ----custom-format-dispatch, eval=FALSE---------------------------------------
# readRDML("experiment.myqpcr")

## ----unregister-format, eval=FALSE--------------------------------------------
# rdmlUnregisterFormat("myformat")

## ----canonical-importer, eval=FALSE-------------------------------------------
# series <- rdmlImportSeries(
#   fdataType = "adp",
#   fdata = fdata,
#   description = description
# )
# 
# parsed <- rdmlImportData(
#   series = list(series),
#   publisher = "Example vendor",
#   serialNumber = "12345",
#   format = "myformat"
# )
# 
# x <- rdmlBuildImport(parsed)

## ----rdml-edit, eval=FALSE----------------------------------------------------
# rdmlEdit(lc96)

## ----downstream, eval=FALSE---------------------------------------------------
# curves <- getFData(
#   lc96,
#   dpType = "adp",
#   longTable = FALSE
# )
# 
# # The first column contains cycle coordinates.
# cycles <- curves[[1]]
# 
# # Remaining columns contain fluorescence series and can be passed to
# # modelling, preprocessing, Cq estimation, or visualisation code.

## ----workflow, eval=FALSE-----------------------------------------------------
# library(RDML7)
# 
# # 1. Import
# x <- readRDML("experiment.rdml")
# 
# # 2. Inspect and validate
# rdmlSummary(x)
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

