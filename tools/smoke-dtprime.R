args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  stop("Usage: Rscript tools/smoke-dtprime.R path/to/file.r96", call. = FALSE)
}

file <- normalizePath(args[[1L]], mustWork = TRUE)
x <- RDML::rdml_read(file)

expected <- c("exp_2000", "exp_400", "combined")
actual <- names(x$experiment)
cat("Experiments:", paste(actual, collapse = ", "), "\n")

missing <- setdiff(expected, actual)
if (length(missing)) {
  stop("Missing DTprime experiment(s): ", paste(missing, collapse = ", "), call. = FALSE)
}

combined <- x$experiment$combined
runs <- names(combined$run)
if (!length(runs)) stop("combined experiment contains no runs", call. = FALSE)

reacts <- combined$run[[runs[[1L]]]]$react
cat("Combined reactions:", length(reacts), "\n")
if (!length(reacts)) stop("combined run contains no reactions", call. = FALSE)

first <- reacts[[1L]]$data[[1L]]
if (!S7::S7_inherits(first$adp, RDML::dpAmpCurveType)) {
  stop("first data item has no ADP curve", call. = FALSE)
}
cat("First ADP points:", nrow(first$adp$fpoints), "\n")

# P2 table/navigation checks ------------------------------------------------
tbl <- RDML::AsTable(x)
cat("AsTable rows:", nrow(tbl), "\n")

for (nm in c("exp.id", "run.id", "target", "sample.type")) {
  if (anyNA(tbl[[nm]])) stop("AsTable contains NA in ", nm, call. = FALSE)
}
if (anyDuplicated(tbl$fdata.name)) {
  stop("AsTable fdata.name is not unique", call. = FALSE)
}

f <- RDML::GetFData(x)
cat("GetFData dimensions:", paste(dim(f), collapse = " x "), "\n")

# Dendrogram should expose real experiment ids, not NA placeholders.
d <- RDML::AsDendrogram(x, plot.dendrogram = FALSE)
if (!all(expected %in% names(d))) {
  stop(
    "Dendrogram experiment names are incomplete: ",
    paste(names(d), collapse = ", "),
    call. = FALSE
  )
}

cat("DTprime P2 smoke test: OK\n")
