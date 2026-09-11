## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", eval = FALSE)
library(RDML)

## -----------------------------------------------------------------------------
# rdesamp <- system.file(
#   "extdata",
#   "RDES_v1_0_example_amplification.tsv",
#   package = "RDML7")
# amp <- readRDML(rdesamp)

## -----------------------------------------------------------------------------
# amp <- readRDML(
#   rdesamp,
#   format = "rdes",
#   expId = "RDES",
#   runId = "run1"
# )

## -----------------------------------------------------------------------------
# rdesmelt <- system.file(
#   "extdata",
#   "RDES_v1_0_example_melting.tsv",
#   package = "RDML7")
# melt <- readRDML(rdesmelt)
# 
# request <- asTable(melt)
# request <- request[request$mdp, ]
# 
# meltData <- getFData(
#   melt,
#   request = request,
#   dpType = "mdp",
#   longTable = TRUE
# )

## -----------------------------------------------------------------------------
# both <- readRDML(
#   rdesamp,
#   format = "rdes",
#   companionFile = rdesmelt,
#   expId = "exp1",
#   runId = "run1"
# )
# 
# asDendrogram(both)

## -----------------------------------------------------------------------------
# rdesmeltsemi <- system.file(
#   "extdata",
#   "RDES_v1_0_example_melting_semicolon.tsv",
#   package = "RDML7")
# meltsemi <- readRDML(
#   rdesmeltsemi,
#   format = "rdes"
# )
# meltsemi$experiment$RDES$run$RDES_v1_0_example_melting_semicolon$react$A1$data$`Exon 1`$meltTemp

## -----------------------------------------------------------------------------
# readRDML(
#   rdesmeltsemi,
#   format = "rdes",
#   strict = TRUE
# )

## -----------------------------------------------------------------------------
# writeRDML(
#   both,
#   "run_amplification.tsv",
#   format = "rdes",
#   expId = "exp1",
#   runId = "run1",
#   rdesType = "adp"
# )
# 
# writeRDML(
#   both,
#   "run_melting.tsv",
#   format = "rdes",
#   expId = "exp1",
#   runId = "run1",
#   rdesType = "mdp"
# )

## -----------------------------------------------------------------------------
# paths <- writeRDML(
#   both,
#   "runboth.tsv",
#   format = "rdes",
#   expId = "exp1",
#   runId = "run1",
#   rdesType = "both"
# )
# 
# paths["amplification"]
# paths["melting"]

