# fromFQDexport importer -----------------------------------------------------------

.rdml_import_fqd <- function(filename, show.progress = TRUE) {
  fromFQDexport <- function() {
    if (!requireNamespace("data.table", quietly = TRUE)) {
      stop("Package 'data.table' is required for FQD-96a import", call. = FALSE)
    }

    content <- readChar(
      filename,
      nchars = file.info(filename)$size,
      useBytes = TRUE
    )
    inpstr <- strsplit(content, "Quan\\.", perl = TRUE)[[1L]]
    if (length(inpstr) < 4L) {
      stop("Unsupported FQD-96a text export", call. = FALSE)
    }

    description <- data.table::fread(
      input = inpstr[[4L]],
      fill = TRUE,
      sep = "\t",
      skip = 2,
      blank.lines.skip = TRUE,
      header = TRUE
    )
    description <- description[Well != "Well"]

    expected_desc_names <- c(
      "position", "sample.id", "sample", "sample.type",
      "target", "target.dyeId", "cq", "cq.mean", "cq.sd",
      "quantity", "quantity.avg", "quantity.sd", "V13", "fdata.name"
    )
    if (ncol(description) != length(expected_desc_names)) {
      stop(
        "Unexpected FQD description column count: ", ncol(description),
        " (expected ", length(expected_desc_names), ")",
        call. = FALSE
      )
    }
    data.table::setnames(description, expected_desc_names)
    description[, fdata.name := paste(position, target, sep = "_")]

    num_cols <- c(
      "cq", "cq.mean", "cq.sd", "quantity", "quantity.avg", "quantity.sd"
    )
    description[, (num_cols) := lapply(.SD, .rdml_as_numeric), .SDcols = num_cols]
    description[, `:=`(
      exp.id = "exp1",
      run.id = "raw_data",
      sample = ifelse(sample == "", "unkn_s", sample),
      react.id = vapply(position, FromPositionToId, numeric(1))
    )]

    type_map <- c(
      Unknown = "unkn",
      Standard = "std",
      Negative = "ntc",
      Positive = "pos"
    )
    mapped <- unname(type_map[description$sample.type])
    description$sample.type[!is.na(mapped)] <- mapped[!is.na(mapped)]

    make_fdata <- function(txt) {
      z <- data.table::fread(
        input = txt,
        fill = TRUE,
        sep = "\t",
        skip = 2,
        blank.lines.skip = TRUE,
        header = TRUE
      )
      z <- z[Well != "Well"]
      drop <- c("Well", "Property", "Std. Con.", "Target", "Dye")
      keep <- setdiff(names(z), drop)
      if (!length(keep)) {
        stop("FQD fluorescence table has no cycle columns", call. = FALSE)
      }

      m <- t(as.matrix(z[, ..keep]))
      m_num <- matrix(
        suppressWarnings(base::as.numeric(m)),
        nrow = nrow(m),
        ncol = ncol(m),
        dimnames = dimnames(m)
      )
      if (ncol(m_num) != nrow(description)) {
        stop(
          "FQD fluorescence/description column mismatch",
          call. = FALSE
        )
      }

      out <- data.frame(cyc = seq_len(nrow(m_num)), check.names = FALSE)
      for (j in seq_len(ncol(m_num))) {
        out[[description$fdata.name[[j]]]] <- m_num[, j]
      }
      out
    }

    rawfdata <- make_fdata(inpstr[[2L]])
    processedfdata <- make_fdata(inpstr[[3L]])

    x <- .rdml_new_import()
    x <- .rdml_set_fdata_import(x, rawfdata, description, "adp")

    description_processed <- data.table::copy(description)
    description_processed[, run.id := "processed_data"]
    x <- .rdml_set_fdata_import(x, processedfdata, description_processed, "adp")
    x
  }

  fromFQDexport()
}
