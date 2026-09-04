# Extracted from test_rdml_new.R:22

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "RDML7", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
path <- system.file(
      "extdata",
      package = "RDML7"
    )
files <- c(
      "BioRad_qPCR_melt.rdml",
      "stepone_std.rdml",
      "lc96_bACTXY.rdml"
    )
for (file in files) {
      
      x <- readRDML(
        file.path(path, file),
        showProgress = FALSE,
        loss = "allow"
      )
      
      testthat::expect_true(
        S7::S7_inherits(
          x,
          rdmlType
        ),
        info = file
      )
      
      testthat::expect_identical(
        x$version,
        "1.3",
        info = file
      )
    }
