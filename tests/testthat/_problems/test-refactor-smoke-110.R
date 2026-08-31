# Extracted from test-refactor-smoke.R:110

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "RDML", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
d <- dataType(
    targetId = idReferenceType("EvaGreen"),
    cq = 27.8,
    adp = dpAmpCurveType(
      data.table::data.table(
        cyc = 1:2,
        tmp = c(65, 65),
        fluor = c(1.1, 2.2)
      )
    ),
    mdp = dpMeltingCurveType(
      data.table::data.table(
        tmp = c(70, 71),
        fluor = c(10, 9)
      )
    )
  )
