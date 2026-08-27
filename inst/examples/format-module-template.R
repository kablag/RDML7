# RDML user format module template
#
# Canonical module API uses camelCase.
# Legacy `rdml_module()` factories are still accepted.

rdmlModule <- function() {

  reader <- function(
      fileName,
      ...) {

    # description columns for the canonical API:
    #
    # fdataName
    # expId
    # runId
    # reactId
    # sample
    # sampleType
    # target
    # targetDyeId
    #
    # Preferred: return parsed intermediate data and let RDML build the tree.
    # return(
    #   rdmlImportData(
    #     series = list(
    #       rdmlImportSeries(
    #         fdataType = "adp",
    #         fdata = fdata,
    #         description = description
    #       )
    #     ),
    #     publisher = "My instrument",
    #     format = "my-format"
    #   )
    # )

    stop("Implement reader()")
  }

  writer <- function(
      x,
      fileName,
      overwrite = FALSE,
      ...) {

    stop("Implement writer()")
  }

  sniff <- function(fileName) {
    0
  }

  list(
    name = "my-format",
    extensions = "foo",
    reader = reader,
    writer = writer,
    sniff = sniff,
    apiVersion = 1L,
    capabilities = c("adp", "intermediate")
  )
}
