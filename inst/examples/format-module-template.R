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
    # return(
    #   rdmlFromFData(
    #     fdata = fdata,
    #     description = description,
    #     fdataType = "adp",
    #     publisher = "My instrument"
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
    sniff = sniff
  )
}
