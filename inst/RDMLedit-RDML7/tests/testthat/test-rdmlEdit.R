test_that("RDMLedit application is installed", {
  appDir <- system.file(
    "RDMLedit",
    package = "RDML7"
  )

  expect_true(
    nzchar(appDir)
  )

  expect_true(
    file.exists(
      file.path(
        appDir,
        "ui.R"
      )
    )
  )

  expect_true(
    file.exists(
      file.path(
        appDir,
        "server.R"
      )
    )
  )

  expect_true(
    file.exists(
      file.path(
        appDir,
        "helpers.R"
      )
    )
  )
})
