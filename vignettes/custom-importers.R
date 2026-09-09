## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----registry, eval=FALSE-----------------------------------------------------
# registerRDMLFormat(
#   name = "myformat",
#   extensions = "abc",
#   reader = readMyFormat
# )

## ----explicit-format, eval=FALSE----------------------------------------------
# x <- readRDML(
#   "experiment.abc",
#   format = "myformat"
# )

## ----fdata-example------------------------------------------------------------
fdata <- data.frame(
  cyc = 1:5,
  r1 = c(100, 102, 110, 180, 650),
  r2 = c(101, 103, 112, 190, 690)
)

fdata

## ----description-example------------------------------------------------------
description <- data.frame(
  fdataName = c("r1", "r2"),
  expId = c("exp1", "exp1"),
  runId = c("run1", "run1"),
  reactId = c("1", "2"),
  sample = c("sample1", "sample2"),
  sampleType = c("std", "unkn"),
  target = c("CD53", "CD53"),
  targetDyeId = c("FAM", "FAM"),
  stringsAsFactors = FALSE
)

description

## ----simple-reader, eval=FALSE------------------------------------------------
# readMyFormat <- function(fileName, ...) {
# 
#   # Vendor-specific parsing
#   fdata <- ...
#   description <- ...
# 
#   buildRDMLFromFData(
#     fdata = fdata,
#     description = description,
#     fdataType = "adp"
#   )
# }

## ----avoid-schema-construction, eval=FALSE------------------------------------
# # Do not do this in a normal file parser:
# experimentType(
#   run = list(
#     runType(
#       react = list(
#         reactType(
#           data = list(
#             dataType(...)
#           )
#         )
#       )
#     )
#   )
# )

## ----simple-text-reader, eval=FALSE-------------------------------------------
# readSimpleQPCR <- function(fileName, ...) {
# 
#   lines <- readLines(
#     fileName,
#     warn = FALSE,
#     encoding = "UTF-8"
#   )
# 
#   getSection <- function(name) {
#     marker <- paste0("[", name, "]")
#     start <- match(marker, lines)
# 
#     if (is.na(start)) {
#       stop(
#         "Missing section ",
#         marker,
#         call. = FALSE
#       )
#     }
# 
#     sections <- grep(
#       "^\\[[^]]+\\]$",
#       lines
#     )
# 
#     nextSection <- sections[
#       sections > start
#     ]
# 
#     end <- if (length(nextSection)) {
#       nextSection[[1L]] - 1L
#     } else {
#       length(lines)
#     }
# 
#     out <- lines[
#       seq.int(
#         start + 1L,
#         end
#       )
#     ]
# 
#     out[
#       nzchar(trimws(out))
#     ]
#   }
# 
#   # Metadata -------------------------------------------------------------
#   metaParts <- strsplit(
#     getSection("meta"),
#     "=",
#     fixed = TRUE
#   )
# 
#   meta <- stats::setNames(
#     vapply(
#       metaParts,
#       function(x) {
#         paste(
#           x[-1L],
#           collapse = "="
#         )
#       },
#       character(1)
#     ),
#     vapply(
#       metaParts,
#       `[[`,
#       character(1),
#       1L
#     )
#   )
# 
#   # Reaction metadata ----------------------------------------------------
#   reactions <- utils::read.delim(
#     text = paste(
#       getSection("reactions"),
#       collapse = "\n"
#     ),
#     stringsAsFactors = FALSE,
#     check.names = FALSE
#   )
# 
#   # Amplification curves -------------------------------------------------
#   fdata <- utils::read.delim(
#     text = paste(
#       getSection("adp"),
#       collapse = "\n"
#     ),
#     stringsAsFactors = FALSE,
#     check.names = FALSE
#   )
# 
#   description <- data.frame(
#     fdataName = reactions$fdataName,
#     expId = meta[["experiment"]],
#     runId = meta[["run"]],
#     reactId = as.character(
#       reactions$reactId
#     ),
#     sample = reactions$sample,
#     sampleType = reactions$sampleType,
#     target = meta[["target"]],
#     targetDyeId = meta[["dye"]],
#     quantity = as.numeric(
#       reactions$quantity
#     ),
#     stringsAsFactors = FALSE
#   )
# 
#   buildRDMLFromFData(
#     fdata = fdata,
#     description = description,
#     fdataType = "adp",
#     publisher = "simpleqpcr"
#   )
# }

## ----register-simple-reader, eval=FALSE---------------------------------------
# registerRDMLFormat(
#   name = "simpleqpcr",
#   extensions = "sqpcr",
#   reader = readSimpleQPCR
# )

## ----use-simple-reader, eval=FALSE--------------------------------------------
# x <- readRDML(
#   "cd53_efficiency.sqpcr"
# )
# 
# summary(x)
# 
# asTable(x)
# 
# amp <- getFData(
#   x,
#   dpType = "adp",
#   longTable = TRUE
# )

## ----complex-adp, eval=FALSE--------------------------------------------------
# ampSeries <- newRDMLImportSeries(
#   fdataType = "adp",
#   fdata = ampFdata,
#   description = ampDescription
# )

## ----complex-mdp, eval=FALSE--------------------------------------------------
# meltSeries <- newRDMLImportSeries(
#   fdataType = "mdp",
#   fdata = meltFdata,
#   description = meltDescription
# )

## ----complex-import-data, eval=FALSE------------------------------------------
# parsed <- newRDMLImportData(
#   series = list(
#     ampSeries,
#     meltSeries
#   ),
#   publisher = "Example vendor",
#   serialNumber = "12345",
#   format = "myformat"
# )

## ----build-complex, eval=FALSE------------------------------------------------
# x <- buildRDMLImport(
#   parsed
# )

## ----complex-reader, eval=FALSE-----------------------------------------------
# readMyComplexFormat <- function(fileName, ...) {
# 
#   # Parse the source file.
#   ampFdata <- ...
#   ampDescription <- ...
# 
#   meltFdata <- ...
#   meltDescription <- ...
# 
#   ampSeries <- newRDMLImportSeries(
#     fdataType = "adp",
#     fdata = ampFdata,
#     description = ampDescription
#   )
# 
#   meltSeries <- newRDMLImportSeries(
#     fdataType = "mdp",
#     fdata = meltFdata,
#     description = meltDescription
#   )
# 
#   newRDMLImportData(
#     series = list(
#       ampSeries,
#       meltSeries
#     ),
#     publisher = "Example vendor",
#     serialNumber = "12345",
#     format = "mycomplexformat"
#   )
# }

## ----register-complex-reader, eval=FALSE--------------------------------------
# registerRDMLFormat(
#   name = "mycomplexformat",
#   extensions = "mcf",
#   reader = readMyComplexFormat
# )

## ----use-complex-reader, eval=FALSE-------------------------------------------
# x <- readRDML(
#   "experiment.mcf"
# )

## ----simple-again, eval=FALSE-------------------------------------------------
# buildRDMLFromFData(
#   fdata,
#   description,
#   fdataType = "adp"
# )

## ----overengineered-simple, eval=FALSE----------------------------------------
# series <- newRDMLImportSeries(
#   "adp",
#   fdata,
#   description
# )
# 
# parsed <- newRDMLImportData(
#   series = list(series)
# )
# 
# buildRDMLImport(parsed)

## ----importer-validation, eval=FALSE------------------------------------------
# registerRDMLFormat(
#   name = "myformat",
#   extensions = "abc",
#   reader = readMyFormat
# )
# 
# x <- readRDML(
#   "example.abc"
# )
# 
# summary(x)
# 
# tbl <- asTable(x)
# tbl
# 
# validateRDML(x)
# 
# amp <- getFData(
#   x,
#   dpType = "adp",
#   longTable = TRUE
# )

## ----importer-validation-mdp, eval=FALSE--------------------------------------
# melt <- getFData(
#   x,
#   dpType = "mdp",
#   longTable = TRUE
# )

## ----reader-test, eval=FALSE--------------------------------------------------
# test_that("myformat reader imports amplification curves", {
# 
#   x <- readRDML(
#     test_path(
#       "data",
#       "example.abc"
#     )
#   )
# 
#   expect_true(
#     S7::S7_inherits(
#       x,
#       rdmlType
#     )
#   )
# 
#   tbl <- asTable(x)
# 
#   expect_true(
#     all(tbl$adp)
#   )
# 
#   expect_setequal(
#     unique(tbl$target),
#     "CD53"
#   )
# 
#   amp <- getFData(
#     x,
#     dpType = "adp"
#   )
# 
#   expect_gt(
#     nrow(amp),
#     0L
#   )
# })

## ----complex-reader-test, eval=FALSE------------------------------------------
# test_that("complex reader imports both amplification and melting data", {
# 
#   x <- readRDML(
#     test_path(
#       "data",
#       "example.mcf"
#     )
#   )
# 
#   tbl <- asTable(x)
# 
#   expect_true(
#     any(tbl$adp)
#   )
# 
#   expect_true(
#     any(tbl$mdp)
#   )
# })

