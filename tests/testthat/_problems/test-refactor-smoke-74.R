# Extracted from test-refactor-smoke.R:74

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "RDML", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
sample <- sampleType(
    id = idType("S1"),
    type = list(
      sampleTargetType(
        targetId = idReferenceType("ACTB"),
        sampleType = sampleTypeType("unkn")
      )
    ),
    quantity = list(
      quantityType(
        targetId = idReferenceType("ACTB"),
        value = 1,
        unit = quantityUnitType("ng")
      )
    )
  )
