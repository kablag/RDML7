testthat::test_that(
  "RDML files can be imported",
  {
    
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
      
      x <- rdmlRead(
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
  }
)


testthat::test_that(
  "CSV can be imported",
  {
    
    filename <- system.file(
      "extdata",
      "from_tables",
      "fdata.csv",
      package = "RDML7"
    )
    
    x <- rdmlRead(
      filename,
      showProgress = FALSE,
      loss = "allow"
    )
    
    testthat::expect_true(
      S7::S7_inherits(
        x,
        rdmlType
      )
    )
    
    testthat::expect_identical(
      x$version,
      "1.3"
    )
  }
)


testthat::test_that(
  "Excel can be imported",
  {
    
    filename <- system.file(
      "extdata",
      "from_tables",
      "table.xlsx",
      package = "RDML7"
    )
    
    x <- rdmlRead(
      filename,
      showProgress = FALSE,
      loss = "allow"
    )
    
    testthat::expect_true(
      S7::S7_inherits(
        x,
        rdmlType
      )
    )
    
    testthat::expect_identical(
      x$version,
      "1.3"
    )
  }
)


testthat::test_that(
  "ABI 7500 EDS can be imported",
  {
    
    filename <- system.file(
      "extdata",
      "from_abi7500",
      "sce.eds",
      package = "RDML7"
    )
    
    x <- rdmlRead(
      filename,
      showProgress = FALSE,
      loss = "allow"
    )
    
    testthat::expect_true(
      S7::S7_inherits(
        x,
        rdmlType
      )
    )
    
    testthat::expect_identical(
      x$version,
      "1.3"
    )
  }
)