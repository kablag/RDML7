NULL

# Extensible RDML format registry --------------------------------------------
#
# File formats are runtime capabilities, not S7 object classes.  This registry
# maps format names/extensions to reader/writer functions.  Built-in vendor
# importers remain in their own import-*.R files; users may register additional
# handlers without modifying RDML7 itself.
#
# This file intentionally contains the registry, dispatchers, and built-in
# registrations together.  `format-registry.R` already loads after all import-*.R
# files in DESCRIPTION/Collate, so this avoids introducing another load-order
# dependency while keeping vendor parsers separate.

.rdmlFormatRegistry <- new.env(parent = emptyenv())
assign(
  ".order",
  character(),
  envir = .rdmlFormatRegistry
)


.rdmlNormalizeFormatName <- function(x, arg = "format") {
  checkmate::assertString(x)
  x <- tolower(trimws(x))

  if (!nzchar(x)) {
    stop("`", arg, "` must not be empty", call. = FALSE)
  }

  x
}


.rdmlNormalizeExtensions <- function(x) {
  if (is.null(x) || !length(x)) {
    return(character())
  }

  if (!is.character(x) || anyNA(x)) {
    stop("`extensions` must be a character vector without NA", call. = FALSE)
  }

  x <- tolower(trimws(x))
  x <- sub("^\\.", "", x)
  x <- x[nzchar(x)]

  unique(x)
}


.rdmlFormatSpecs <- function() {
  order <- get(
    ".order",
    envir = .rdmlFormatRegistry,
    inherits = FALSE
  )

  if (!length(order)) {
    return(list())
  }

  order <- order[
    vapply(
      order,
      exists,
      logical(1),
      envir = .rdmlFormatRegistry,
      inherits = FALSE
    )
  ]

  lapply(
    order,
    function(name) {
      get(
        name,
        envir = .rdmlFormatRegistry,
        inherits = FALSE
      )
    }
  )
}


.rdmlRegisterFormat <- function(
    name,
    extensions = character(),
    reader = NULL,
    writer = NULL,
    builtin = FALSE) {

  name <- .rdmlNormalizeFormatName(name, "name")
  extensions <- .rdmlNormalizeExtensions(extensions)
  checkmate::assertFlag(builtin)

  if (
    !is.null(reader) &&
      !is.function(reader)
  ) {
    stop("`reader` must be a function or NULL", call. = FALSE)
  }

  if (
    !is.null(writer) &&
      !is.function(writer)
  ) {
    stop("`writer` must be a function or NULL", call. = FALSE)
  }

  if (
    is.null(reader) &&
      is.null(writer)
  ) {
    stop(
      "A format must provide at least one of `reader` or `writer`",
      call. = FALSE
    )
  }

  if (
    exists(
      name,
      envir = .rdmlFormatRegistry,
      inherits = FALSE
    )
  ) {
    stop(
      "RDML format already registered: ",
      name,
      call. = FALSE
    )
  }

  spec <- list(
    name = name,
    extensions = extensions,
    reader = reader,
    writer = writer,
    builtin = builtin
  )

  assign(
    name,
    spec,
    envir = .rdmlFormatRegistry
  )

  order <- get(
    ".order",
    envir = .rdmlFormatRegistry,
    inherits = FALSE
  )

  assign(
    ".order",
    c(order, name),
    envir = .rdmlFormatRegistry
  )

  invisible(spec)
}

#' Register a file-format reader and/or writer
#'
#' Register a named file format. Automatic dispatch uses the file extension.
#' If several formats support the same extension and operation, the first
#' registered matching format is used by default. Users can select another
#' registered format explicitly with `format = "name"`.
#'
#' @param name Unique format name used by `format=`.
#' @param extensions Character vector of file extensions, with or without
#'   a leading dot.
#' @param reader Reader function or `NULL`.
#' @param writer Writer function or `NULL`.
#' @return Registered format specification invisibly.
#' @seealso `loadRDMLModule`, `listRDMLFormats`, `readRDML`
#' @examples
#' rdmlFile <- system.file("extdata", "lc96_bACTXY.rdml", package = "RDML7")
#' demoFile <- tempfile(fileext = ".rdmldemo")
#' file.copy(rdmlFile, demoFile)
#'
#' registerRDMLFormat(
#'   name = "rdml-demo",
#'   extensions = "rdmldemo",
#'   reader = function(fileName, ...) {
#'     readRDML(fileName, format = "rdml", ...)
#'   }
#' )
#' detectRDMLFormat(demoFile, "read")
#' demo <- readRDML(demoFile, showProgress = FALSE)
#'
#' unregisterRDMLFormat("rdml-demo")
#' unlink(demoFile)
#' @export
registerRDMLFormat <- function(
    name,
    extensions = character(),
    reader = NULL,
    writer = NULL) {

  .rdmlRegisterFormat(
    name = name,
    extensions = extensions,
    reader = reader,
    writer = writer,
    builtin = FALSE
  )
}

#' Unregister a file format
#'
#' @param name Registered format name.
#' @param force Allow removal of a built-in format.
#' @return `TRUE` invisibly when removed.
#' @seealso `registerRDMLFormat`, `listRDMLFormats`
#' @export
unregisterRDMLFormat <- function(name, force = FALSE) {
  name <- .rdmlNormalizeFormatName(name, "name")
  checkmate::assertFlag(force)

  if (
    !exists(
      name,
      envir = .rdmlFormatRegistry,
      inherits = FALSE
    )
  ) {
    stop(
      "Unknown RDML format: ",
      name,
      call. = FALSE
    )
  }

  spec <- get(
    name,
    envir = .rdmlFormatRegistry,
    inherits = FALSE
  )

  if (isTRUE(spec$builtin) && !force) {
    stop(
      "Built-in format '",
      name,
      "' cannot be unregistered without `force = TRUE`",
      call. = FALSE
    )
  }

  rm(
    list = name,
    envir = .rdmlFormatRegistry
  )


  order <- get(
    ".order",
    envir = .rdmlFormatRegistry,
    inherits = FALSE
  )

  assign(
    ".order",
    order[order != name],
    envir = .rdmlFormatRegistry
  )

  invisible(TRUE)
}


#' List registered RDML file formats
#'
#' Formats are returned in registration order. For automatic dispatch, the
#' first format supporting a given extension and operation is the default.
#'
#' @return Data frame with format name, extensions, and reader/writer
#'   availability.
#' @seealso `registerRDMLFormat`, `detectRDMLFormat`
#' @export
listRDMLFormats <- function() {
  specs <- .rdmlFormatSpecs()

  if (!length(specs)) {
    return(
      data.frame(
        format = character(),
        extensions = character(),
        read = logical(),
        write = logical(),
        stringsAsFactors = FALSE
      )
    )
  }

  out <- do.call(
    rbind,
    lapply(
      specs,
      function(spec) {
        data.frame(
          format = spec$name,
          extensions = paste(
            spec$extensions,
            collapse = ", "
          ),
          read = !is.null(spec$reader),
          write = !is.null(spec$writer),
          stringsAsFactors = FALSE
        )
      }
    )
  )

  rownames(out) <- NULL
  out
}


.rdmlFormatSupports <- function(spec, operation) {
  switch(
    operation,
    read = !is.null(spec$reader),
    write = !is.null(spec$writer),
    stop("Unknown format operation: ", operation, call. = FALSE)
  )
}


.rdmlResolveExplicitFormat <- function(
    format,
    operation) {

  name <- .rdmlNormalizeFormatName(format, "format")

  if (
    !exists(
      name,
      envir = .rdmlFormatRegistry,
      inherits = FALSE
    )
  ) {
    stop(
      "Unknown RDML format: ",
      format,
      ". Registered formats: ",
      paste(
        vapply(
          .rdmlFormatSpecs(),
          function(spec) spec$name,
          character(1)
        ),
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  spec <- get(
    name,
    envir = .rdmlFormatRegistry,
    inherits = FALSE
  )

  if (!.rdmlFormatSupports(spec, operation)) {
    stop(
      "Format '",
      name,
      "' does not support ",
      operation,
      call. = FALSE
    )
  }

  spec
}


.rdmlResolveAutoFormat <- function(
    fileName,
    operation) {

  ext <- tolower(
    tools::file_ext(fileName)
  )

  if (!nzchar(ext)) {
    stop(
      "Cannot determine ",
      operation,
      " format for a file without an extension: ",
      fileName,
      ". Specify `format=` explicitly.",
      call. = FALSE
    )
  }

  candidates <- Filter(
    function(spec) {
      .rdmlFormatSupports(
        spec,
        operation
      ) &&
        ext %in% spec$extensions
    },
    .rdmlFormatSpecs()
  )

  if (!length(candidates)) {
    stop(
      "No registered ",
      operation,
      " format for extension .",
      ext,
      ". Use `listRDMLFormats()` to list available handlers or ",
      "specify `format=` explicitly.",
      call. = FALSE
    )
  }

  # Registration order defines the default for an extension.
  candidates[[1L]]
}


.rdmlResolveFormat <- function(
    fileName,
    format = "auto",
    operation = c("read", "write")) {

  operation <- match.arg(operation)
  format <- .rdmlNormalizeFormatName(format, "format")

  if (identical(format, "auto")) {
    return(
      .rdmlResolveAutoFormat(
        fileName,
        operation
      )
    )
  }

  .rdmlResolveExplicitFormat(
    format,
    operation
  )
}


#' Detect the format handler selected for a path
#'
#' @param fileName File path.
#' @param operation `"read"` or `"write"`.
#' @return Registered format name.
#' @seealso `listRDMLFormats`, `readRDML`, `writeRDML`
#' @examples
#' amplificationFile <- system.file(
#'   "extdata", "lc96_bACTXY.rdml", package = "RDML7"
#' )
#' meltingFile <- system.file(
#'   "extdata", "BioRad_qPCR_melt.rdml", package = "RDML7"
#' )
#' detectRDMLFormat(amplificationFile, operation = "read")
#' detectRDMLFormat(meltingFile, operation = "write")
#' @export
detectRDMLFormat <- function(
    fileName,
    operation = c("read", "write")) {

  checkmate::assertString(fileName)
  operation <- match.arg(operation)

  .rdmlResolveFormat(
    fileName,
    format = "auto",
    operation = operation
  )$name
}


.rdmlCallHandler <- function(fun, args) {
  fml <- names(formals(fun))

  if (is.null(fml)) {
    return(
      do.call(fun, args)
    )
  }

  # Compatibility for third-party handlers written against the old API.
  # Only rename when the canonical formal is absent and the legacy formal is
  # explicitly declared by the handler.
  legacyArgMap <- c(
    fileName = "filename",
    showProgress = "show.progress",
    conditionsSep = "conditions.sep",
    fdataType = "fdata.type",
    serialNumber = "serial.number",
    namePattern = "name.pattern",
    longTable = "long.table",
    sampleType = "sample.type",
    targetDyeId = "target.dyeId",
    dpType = "dp.type",
    plotDendrogram = "plot.dendrogram",
    dataConflict = "data.conflict"
  )

  for (canonicalName in names(legacyArgMap)) {
    legacyName <- legacyArgMap[[canonicalName]]

    if (
      canonicalName %in% names(args) &&
      !(canonicalName %in% fml) &&
      legacyName %in% fml
    ) {
      names(args)[
        names(args) == canonicalName
      ] <- legacyName
    }
  }

  if ("..." %in% fml) {
    return(
      do.call(fun, args)
    )
  }

  args <- args[
    intersect(
      names(args),
      fml
    )
  ]

  do.call(fun, args)
}


# Built-in writer helpers ---------------------------------------------------

.rdmlWriteXmlFile <- function(
    x,
    fileName,
    overwrite = FALSE,
    ...) {

  fileName <- .rdmlOutputPath(fileName)

  if (file.exists(fileName) && !overwrite) {
    stop(
      "Output file already exists: ",
      fileName,
      call. = FALSE
    )
  }

  tree <- asXML(x, loss = "allow")

  writeLines(
    enc2utf8(tree),
    con = fileName,
    useBytes = TRUE
  )

  invisible(fileName)
}


.rdmlWriteArchive <- function(
    x,
    fileName,
    overwrite = FALSE,
    ...) {

  fileName <- .rdmlOutputPath(fileName)

  if (file.exists(fileName) && !overwrite) {
    stop(
      "Output file already exists: ",
      fileName,
      call. = FALSE
    )
  }

  tree <- asXML(x, loss = "allow")

  tmpdir <- tempfile("rdml-write-")

  if (!dir.create(tmpdir)) {
    stop(
      "Failed to create temporary RDML directory",
      call. = FALSE
    )
  }

  on.exit(
    unlink(
      tmpdir,
      recursive = TRUE,
      force = TRUE
    ),
    add = TRUE
  )

  xmlFile <- file.path(
    tmpdir,
    "rdml_data.xml"
  )

  writeLines(
    enc2utf8(tree),
    con = xmlFile,
    useBytes = TRUE
  )

  zipFile <- tempfile(fileext = ".zip")

  on.exit(
    unlink(
      zipFile,
      force = TRUE
    ),
    add = TRUE
  )

  oldWd <- getwd()

  on.exit(
    setwd(oldWd),
    add = TRUE
  )

  setwd(tmpdir)

  zipStatus <- utils::zip(
    zipfile = zipFile,
    files = "rdml_data.xml"
  )

  setwd(oldWd)

  if (
    !file.exists(zipFile) ||
    (
      !is.null(zipStatus) &&
      length(zipStatus) == 1L &&
      is.numeric(zipStatus) &&
      zipStatus != 0
    )
  ) {
    stop(
      "Failed to create RDML ZIP archive",
      call. = FALSE
    )
  }

  if (
    !file.copy(
      zipFile,
      fileName,
      overwrite = overwrite
    )
  ) {
    stop(
      "Failed to write RDML file: ",
      fileName,
      call. = FALSE
    )
  }

  invisible(fileName)
}


# Built-in format registration ---------------------------------------------

.rdmlRegisterBuiltinFormats <- function() {

  .rdmlRegisterFormat(
    name = "abi",
    extensions = "eds",
    reader = function(
        fileName,
        showProgress = TRUE,
        ...) {
      .rdmlImportAbi(
        fileName,
        showProgress
      )
    },
    builtin = TRUE
  )

  .rdmlRegisterFormat(
    name = "rotorgene",
    extensions = "rex",
    reader = function(
        fileName,
        showProgress = TRUE,
        ...) {
      .rdmlImportRotorGene(
        fileName,
        showProgress
      )
    },
    builtin = TRUE
  )

  .rdmlRegisterFormat(
    name = "excel",
    extensions = c(
      "xlsx",
      "xls"
    ),
    reader = function(
        fileName,
        showProgress = TRUE,
        ...) {
      .rdmlImportExcel(
        fileName,
        showProgress
      )
    },
    builtin = TRUE
  )

  .rdmlRegisterFormat(
    name = "csv",
    extensions = "csv",
    reader = function(
        fileName,
        showProgress = TRUE,
        ...) {
      .rdmlImportCsv(
        fileName,
        showProgress
      )
    },
    builtin = TRUE
  )

  .rdmlRegisterFormat(
    name = "dtprime",
    extensions = "r96",
    reader = function(
        fileName,
        showProgress = TRUE,
        ...) {
      .rdmlImportDtprime(
        fileName,
        showProgress
      )
    },
    builtin = TRUE
  )

  .rdmlRegisterFormat(
    name = "fqd",
    extensions = "txt",
    reader = function(
        fileName,
        showProgress = TRUE,
        ...) {
      .rdmlImportFqd(
        fileName,
        showProgress
      )
    },
    builtin = TRUE
  )

  .rdmlRegisterFormat(
    name = "innova-qstd",
    extensions = "qstd",
    reader = function(
        fileName,
        showProgress = TRUE,
        ...) {
      .rdmlImportQstd(
        fileName,
        showProgress
      )
    },
    builtin = TRUE
  )

  .rdmlRegisterFormat(
    name = "rdes",
    extensions = c(
      "tsv",
      "csv",
      "txt"
    ),
    reader = function(
        fileName,
        showProgress = TRUE,
        companionFile = NULL,
        expId = "RDES",
        runId = NULL,
        strict = FALSE,
        ...) {
      .rdmlImportRdes(
        fileName = fileName,
        showProgress = showProgress,
        companionFile = companionFile,
        expId = expId,
        runId = runId,
        strict = strict
      )
    },
    writer = function(
        x,
        fileName,
        overwrite = FALSE,
        expId = NULL,
        runId = NULL,
        rdesType = c(
          "auto",
          "adp",
          "mdp",
          "both"
        ),
        ...) {
      .rdmlWriteRdes(
        x = x,
        fileName = fileName,
        overwrite = overwrite,
        expId = expId,
        runId = runId,
        rdesType = rdesType
      )
    },
    builtin = TRUE
  )

  .rdmlRegisterFormat(
    name = "rdml-xml",
    extensions = "xml",
    reader = function(
        fileName,
        showProgress = TRUE,
        conditionsSep = NULL,
        cluster = NULL,
        ...) {
      .rdmlImportRdml(
        fileName,
        showProgress,
        conditionsSep,
        cluster,
        format = "xml"
      )
    },
    writer = .rdmlWriteXmlFile,
    builtin = TRUE
  )

  .rdmlRegisterFormat(
    name = "rdml",
    extensions = c(
      "rdml",
      "rdm"
    ),
    reader = function(
        fileName,
        showProgress = TRUE,
        conditionsSep = NULL,
        cluster = NULL,
        ...) {
      .rdmlImportRdml(
        fileName,
        showProgress,
        conditionsSep,
        cluster,
        format = "rdml"
      )
    },
    writer = .rdmlWriteArchive,
    builtin = TRUE
  )

  # Roche LC96 archives are readable through the native RDML importer, but
  # RDML7 does not recreate Roche's additional vendor files on export.
  .rdmlRegisterFormat(
    name = "roche-lc96",
    extensions = "lc96p",
    reader = function(
        fileName,
        showProgress = TRUE,
        conditionsSep = NULL,
        cluster = NULL,
        ...) {
      .rdmlImportRdml(
        fileName,
        showProgress,
        conditionsSep,
        cluster,
        format = "rdml"
      )
    },
    builtin = TRUE
  )

  invisible(TRUE)
}


.rdmlRegisterBuiltinFormats()


# Public read/write dispatchers --------------------------------------------

#' Read qPCR data into an RDML object
#'
#' Selects a reader from the runtime registry. Readers may return `rdmlType`
#' directly or `rdmlImportData`, which is built automatically.
#'
#' @param fileName Input file.
#' @param showProgress Show importer progress.
#' @param conditionsSep Optional Roche condition separator retained for
#' compatibility.
#' @param cluster Reserved for compatibility.
#' @param format Registered name/alias/extension or `"auto"`.
#' @param loss Loss policy: `"warn"`, `"error"`, or `"allow"`.
#' @param ... Format-specific arguments. RDES supports `companionFile`,
#' `expId`, `runId`, and `strict`.
#' @return `rdmlType`.
#' @seealso `writeRDML`, `listRDMLFormats`, `validateRDML`
#' @export
readRDML <- function(
    fileName,
    showProgress = TRUE,
    conditionsSep = NULL,
    cluster = NULL,
    format = "auto",
    loss = c("warn", "error", "allow"),
    ...) {

  if (missing(fileName)) {
    stop(
      "fileName is required",
      call. = FALSE
    )
  }

  checkmate::assertString(fileName)
  checkmate::assertFlag(showProgress)
  loss <- match.arg(loss)

  if (!file.exists(fileName)) {
    stop(
      "Input file does not exist: ",
      fileName,
      call. = FALSE
    )
  }

  spec <- .rdmlResolveFormat(
    fileName,
    format = format,
    operation = "read"
  )

  result <- .rdmlCallHandler(
    spec$reader,
    c(
      list(
        fileName = fileName,
        showProgress = showProgress,
        conditionsSep = conditionsSep,
        cluster = cluster,
        loss = loss
      ),
      list(...)
    )
  )

  if (S7::S7_inherits(result, rdmlImportData)) {
    result <- buildRDMLImport(
      result,
      loss = loss
    )
  }

  if (!S7::S7_inherits(result, rdmlType)) {
    .rdmlAbort(
      code = "invalidReaderResult",
      message = paste0(
        "Reader '",
        spec$name,
        "' returned ",
        paste(class(result), collapse = "/"),
        " instead of rdmlType or rdmlImportData"
      ),
      format = spec$name
    )
  }

  result
}


#' Write an RDML object through the file-format registry
#'
#' @param x `rdmlType`.
#' @param fileName Destination path.
#' @param format Registered name/alias/extension or `"auto"`.
#' @param overwrite Replace an existing destination.
#' @param loss Loss policy: `"warn"`, `"error"`, or `"allow"`.
#' @param ... Format-specific arguments. RDES supports `expId`, `runId`, and
#' `rdesType = "auto"`, `"adp"`, `"mdp"`, or `"both"`.
#' @return Writer-specific result, normally an output path invisibly.
#' @seealso `readRDML`, `listRDMLFormats`, `newRDMLLossRecord`
#' @export
writeRDML <- function(
    x,
    fileName,
    format = "auto",
    overwrite = FALSE,
    loss = c("warn", "error", "allow"),
    ...) {

  if (
    !S7::S7_inherits(
      x,
      rdmlType
    )
  ) {
    stop(
      "`x` must be an rdmlType object",
      call. = FALSE
    )
  }

  checkmate::assertString(fileName)
  checkmate::assertFlag(overwrite)
  loss <- match.arg(loss)

  spec <- .rdmlResolveFormat(
    fileName,
    format = format,
    operation = "write"
  )

  .rdmlCallHandler(
    spec$writer,
    c(
      list(
        x = x,
        fileName = fileName,
        overwrite = overwrite,
        loss = loss
      ),
      list(...)
    )
  )
}


# Public helper for third-party importers ----------------------------------

#' Create an RDML object from fluorescence and metadata tables
#'
#' @param fdata Fluorescence table accepted by `setFData()`.
#' @param description CamelCase metadata table accepted by `setFData()`.
#' @param fdataType `"adp"` or `"mdp"`.
#' @param publisher Optional publisher/importer identifier.
#' @param serialNumber Top-level serial number.
#' @param ... Additional arguments forwarded to `buildRDMLImport()`.
#' @return `rdmlType`.
#' @seealso `setFData`, `rdmlImportData`
#' @examples
#' rdmlFile <- system.file("extdata", "lc96_bACTXY.rdml", package = "RDML7")
#' source <- readRDML(rdmlFile, showProgress = FALSE)
#' description <- asTable(source)
#' description <- description[seq_len(min(2L, nrow(description))), ]
#' fluorescence <- getFData(source, request = description, dpType = "adp")
#'
#' rebuilt <- buildRDMLFromFData(
#'   fdata = fluorescence,
#'   description = description,
#'   fdataType = "adp",
#'   publisher = "RDML7 example"
#' )
#' asTable(rebuilt)
#' @export
buildRDMLFromFData <- function(
    fdata,
    description,
    fdataType = "adp",
    publisher = NULL,
    serialNumber = "1",
    ...) {

  checkmate::assertChoice(
    fdataType,
    c(
      "adp",
      "mdp"
    )
  )

  importData <- newRDMLImportData(
    series = list(
      newRDMLImportSeries(
        fdataType = fdataType,
        fdata = data.table::as.data.table(fdata),
        description = data.table::as.data.table(description)
      )
    ),
    publisher = if (is.null(publisher)) NA_character_ else publisher,
    serialNumber = serialNumber,
    format = "fdata"
  )

  buildRDMLImport(
    importData,
    ...
  )
}


# User module loader --------------------------------------------------------

.rdmlModuleSpecs <- function(module) {
  if (
    is.list(module) &&
    !is.null(module$name)
  ) {
    return(
      list(module)
    )
  }

  if (
    is.list(module) &&
    length(module) &&
    all(
      vapply(
        module,
        function(x) {
          is.list(x) &&
            !is.null(x$name)
        },
        logical(1)
      )
    )
  ) {
    return(module)
  }

  stop(
    "`rdmlModule()` must return a format specification or a list of specifications",
    call. = FALSE
  )
}


#' Load file-format handlers from an R module
#'
#' The module defines `rdmlModule()` (legacy `rdml_module()` is also accepted)
#' and returns one or more specifications accepted by `registerRDMLFormat()`.
#' Each specification may contain only `name`, `extensions`, `reader`, and
#' `writer`.
#'
#' @param path Module R file.
#' @return Registered format names invisibly.
#' @seealso `registerRDMLFormat`, `listRDMLFormats`
#' @export
loadRDMLModule <- function(path) {
  checkmate::assertString(path)

  if (!file.exists(path)) {
    stop(
      "RDML module does not exist: ",
      path,
      call. = FALSE
    )
  }

  moduleEnv <- new.env(
    parent = environment(loadRDMLModule)
  )

  sys.source(
    path,
    envir = moduleEnv
  )

  factoryName <- if (
    exists(
      "rdmlModule",
      envir = moduleEnv,
      inherits = FALSE
    )
  ) {
    "rdmlModule"
  } else if (
    exists(
      "rdml_module",
      envir = moduleEnv,
      inherits = FALSE
    )
  ) {
    "rdml_module"
  } else {
    stop(
      "RDML module must define `rdmlModule()` ",
      "(legacy `rdml_module()` is also accepted)",
      call. = FALSE
    )
  }

  factory <- get(
    factoryName,
    envir = moduleEnv,
    inherits = FALSE
  )

  if (!is.function(factory)) {
    stop(
      "`rdmlModule` must be a function",
      call. = FALSE
    )
  }

  specs <- .rdmlModuleSpecs(
    factory()
  )

  allowed <- c(
    "name",
    "extensions",
    "reader",
    "writer"
  )

  registered <- character(
    length(specs)
  )

  for (i in seq_along(specs)) {
    spec <- specs[[i]]

    unknown <- setdiff(
      names(spec),
      allowed
    )

    if (length(unknown)) {
      stop(
        "Unknown field(s) in RDML module format specification: ",
        paste(
          unknown,
          collapse = ", "
        ),
        call. = FALSE
      )
    }

    do.call(
      registerRDMLFormat,
      spec
    )

    registered[[i]] <- .rdmlNormalizeFormatName(
      spec$name,
      "name"
    )
  }

  invisible(registered)
}
