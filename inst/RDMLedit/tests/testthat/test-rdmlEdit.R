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

  modules <- c(
    file.path(
      "ui",
      c(
        "files.R", "metadata.R", "experiment.R", "qpcr.R", "melting.R",
        "store.R", "help.R", "log.R"
      )
    ),
    file.path(
      "server",
      c(
        "files.R", "rdml-id.R", "experimenter.R", "documentation.R",
        "dye.R", "sample.R", "target.R", "experiment.R", "object-tree.R",
        "analysis-shared.R", "qpcr.R", "melting.R", "store.R", "log.R"
      )
    )
  )
  expect_true(all(file.exists(file.path(appDir, "modules", modules))))
})
