# fromExcel importer -----------------------------------------------------------

.rdml_import_excel <- function(filename, show.progress = TRUE) {
  fromExcel <- function() {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package 'readxl' is required for Excel import", call. = FALSE)
    }

    descr <- as.data.frame(
      readxl::read_excel(filename, sheet = "description"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    # Bio-Rad export layout.
    if ("Well" %in% names(descr)) {
      if (!("Starting Quantity (SQ)" %in% names(descr))) {
        descr[["Starting Quantity (SQ)"]] <- NA_real_
      }

      required <- c("Well", "Sample", "Content", "Target", "Fluor")
      miss <- setdiff(required, names(descr))
      if (length(miss)) {
        stop(
          "Bio-Rad Excel description is missing: ",
          paste(miss, collapse = ", "),
          call. = FALSE
        )
      }

      descr <- data.frame(
        fdata.name = as.character(descr[["Well"]]),
        exp.id = "Exp1",
        run.id = "Run1",
        react.id = seq_len(nrow(descr)),
        sample = as.character(descr[["Sample"]]),
        sample.type = tolower(as.character(descr[["Content"]])),
        target = as.character(descr[["Target"]]),
        target.dyeId = as.character(descr[["Fluor"]]),
        quantity = suppressWarnings(base::as.numeric(descr[["Starting Quantity (SQ)"]])),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      # Unkn-1 -> unkn, Std-2 -> std, etc.
      descr$sample.type <- sub("-.*$", "", descr$sample.type)
      # A01 -> A1.
      descr$fdata.name <- sub(
        "^([A-Za-z]+)0+([1-9].*)$",
        "\\1\\2",
        descr$fdata.name
      )
    }

    read_numeric_sheet <- function(sheet) {
      tryCatch({
        z <- as.data.frame(
          readxl::read_excel(filename, sheet = sheet),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        z[] <- lapply(z, function(v) suppressWarnings(base::as.numeric(v)))
        z
      }, error = function(e) NULL)
    }

    adp_data <- read_numeric_sheet("adp")
    mdp_data <- read_numeric_sheet("mdp")

    if (is.null(adp_data) && is.null(mdp_data)) {
      stop("Excel file contains neither 'adp' nor 'mdp' sheet", call. = FALSE)
    }

    x <- .rdml_new_import()
    if (!is.null(adp_data)) {
      x <- .rdml_set_fdata_import(x, adp_data, descr, "adp")
    }
    if (!is.null(mdp_data)) {
      x <- .rdml_set_fdata_import(x, mdp_data, descr, "mdp")
    }
    x
  }

  fromExcel()
}
