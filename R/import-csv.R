# fromCSV importer -----------------------------------------------------------

.rdml_import_csv <- function(filename, show.progress = TRUE) {
  fromCSV <- function() {
    pcrdata <- utils::read.csv(
      filename,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    if (ncol(pcrdata) < 2L) {
      stop("CSV must contain a coordinate column and fluorescence columns", call. = FALSE)
    }

    fdata.names <- names(pcrdata)[-1L]
    first_name <- tolower(names(pcrdata)[1L])
    data.type <- if (first_name %in% c("tmp", "temperature")) "mdp" else "adp"

    descr <- data.frame(
      fdata.name = fdata.names,
      exp.id = "exp1",
      run.id = "run1",
      react.id = seq_along(fdata.names),
      sample = fdata.names,
      target = "unkn",
      target.dyeId = "unkn",
      sample.type = "unkn",
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    x <- .rdml_new_import()
    .rdml_set_fdata_import(x, pcrdata, descr, data.type)
  }

  fromCSV()
}
