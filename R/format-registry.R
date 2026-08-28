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


.rdmlNormalizeAliases <- function(x) {
  if (is.null(x) || !length(x)) {
    return(character())
  }

  if (!is.character(x) || anyNA(x)) {
    stop("`aliases` must be a character vector without NA", call. = FALSE)
  }

  x <- tolower(trimws(x))
  x <- x[nzchar(x)]

  unique(x)
}


.rdmlFormatApiVersions <- 1L


.rdmlNormalizeCapabilities <- function(x) {
  if (is.null(x) || !length(x)) {
    return(character())
  }

  if (!is.character(x) || anyNA(x)) {
    stop("`capabilities` must be a character vector without NA", call. = FALSE)
  }

  unique(x[nzchar(x)])
}


.rdmlFormatSpecs <- function() {
  keys <- ls(
    envir = .rdmlFormatRegistry,
    all.names = TRUE
  )

  if (!length(keys)) {
    return(list())
  }

  lapply(
    keys,
    function(key) {
      get(
        key,
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
    sniff = NULL,
    aliases = character(),
    priority = 0L,
    apiVersion = 1L,
    capabilities = character(),
    overwrite = FALSE,
    builtin = FALSE) {

  name <- .rdmlNormalizeFormatName(name, "name")
  extensions <- .rdmlNormalizeExtensions(extensions)
  aliases <- .rdmlNormalizeAliases(aliases)
  capabilities <- .rdmlNormalizeCapabilities(capabilities)
  apiVersion <- as.integer(apiVersion)

  if (
    length(apiVersion) != 1L ||
    is.na(apiVersion) ||
    !apiVersion %in% .rdmlFormatApiVersions
  ) {
    stop(
      "Unsupported format API version: ",
      apiVersion,
      ". Supported: ",
      paste(.rdmlFormatApiVersions, collapse = ", "),
      call. = FALSE
    )
  }

  checkmate::assertFlag(overwrite)
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
    !is.null(sniff) &&
    !is.function(sniff)
  ) {
    stop("`sniff` must be a function or NULL", call. = FALSE)
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

  priority <- as.integer(priority)

  if (
    length(priority) != 1L ||
    is.na(priority)
  ) {
    stop("`priority` must be one integer", call. = FALSE)
  }

  existsName <- exists(
    name,
    envir = .rdmlFormatRegistry,
    inherits = FALSE
  )

  if (existsName && !overwrite) {
    stop(
      "RDML format already registered: ",
      name,
      call. = FALSE
    )
  }

  # Names and aliases are explicit identifiers and therefore must remain
  # unambiguous.  Extensions may intentionally overlap and are resolved by
  # sniff()/priority.
  otherSpecs <- .rdmlFormatSpecs()

  if (existsName) {
    otherSpecs <- Filter(
      function(spec) !identical(spec$name, name),
      otherSpecs
    )
  }

  explicitTokens <- unique(c(name, aliases))

  for (spec in otherSpecs) {
    occupied <- unique(c(spec$name, spec$aliases))
    conflict <- intersect(explicitTokens, occupied)

    if (length(conflict)) {
      stop(
        "Format name/alias already registered: ",
        paste(conflict, collapse = ", "),
        call. = FALSE
      )
    }
  }

  spec <- list(
    name = name,
    extensions = extensions,
    aliases = aliases,
    reader = reader,
    writer = writer,
    sniff = sniff,
    priority = priority,
    apiVersion = apiVersion,
    capabilities = capabilities,
    builtin = builtin
  )

  assign(
    name,
    spec,
    envir = .rdmlFormatRegistry
  )

  invisible(spec)
}


#' Register a file-format reader and/or writer
#'
#' Extension collisions are allowed. On read, sniffer confidence and then
#' priority choose a handler. A reader returns `rdmlType` or `rdmlImportData`.
#'
#' @param name Unique format name.
#' @param extensions Extensions with or without a leading dot.
#' @param reader Reader function or `NULL`.
#' @param writer Writer function or `NULL`.
#' @param sniff Optional `function(fileName)` returning confidence 0..1.
#' @param aliases Explicit aliases accepted by `format=`.
#' @param priority Integer tie-break priority; larger values win.
#' @param apiVersion Plugin API version (currently 1).
#' @param capabilities Character vector of supported features.
#' @param overwrite Replace an existing format of the same name.
#' @return Registered format specification invisibly.
#' @seealso `rdmlLoadModule`, `rdmlFormats`, `rdmlRead`
#' @export
rdmlRegisterFormat <- function(
    name,
    extensions = character(),
    reader = NULL,
    writer = NULL,
    sniff = NULL,
    aliases = character(),
    priority = 0L,
    apiVersion = 1L,
    capabilities = character(),
    overwrite = FALSE) {

  .rdmlRegisterFormat(
    name = name,
    extensions = extensions,
    reader = reader,
    writer = writer,
    sniff = sniff,
    aliases = aliases,
    priority = priority,
    apiVersion = apiVersion,
    capabilities = capabilities,
    overwrite = overwrite,
    builtin = FALSE
  )
}


#' Unregister a file format
#'
#' @param name Registered format name.
#' @param force Allow removal of a built-in format.
#' @return `TRUE` invisibly when removed.
#' @seealso `rdmlRegisterFormat`, `rdmlFormats`
#' @export
rdmlUnregisterFormat <- function(name, force = FALSE) {
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

  invisible(TRUE)
}


#' List registered RDML file formats
#'
#' @return Data frame containing extensions, aliases, reader/writer/sniffer
#' availability, priority, API version, capabilities, and built-in status.
#' @seealso `rdmlRegisterFormat`, `rdmlDetectFormat`
#' @export
rdmlFormats <- function() {
  specs <- .rdmlFormatSpecs()

  if (!length(specs)) {
    return(
      data.frame(
        format = character(),
        extensions = character(),
        aliases = character(),
        read = logical(),
        write = logical(),
        sniff = logical(),
        priority = integer(),
        apiVersion = integer(),
        capabilities = character(),
        builtin = logical(),
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
          extensions = paste(spec$extensions, collapse = ", "),
          aliases = paste(spec$aliases, collapse = ", "),
          read = !is.null(spec$reader),
          write = !is.null(spec$writer),
          sniff = !is.null(spec$sniff),
          priority = spec$priority,
          apiVersion = spec$apiVersion,
          capabilities = paste(spec$capabilities, collapse = ", "),
          builtin = isTRUE(spec$builtin),
          stringsAsFactors = FALSE
        )
      }
    )
  )

  rownames(out) <- NULL
  out[order(out$format), , drop = FALSE]
}


.rdmlFormatSupports <- function(spec, operation) {
  switch(
    operation,
    read = !is.null(spec$reader),
    write = !is.null(spec$writer),
    stop("Unknown format operation: ", operation, call. = FALSE)
  )
}


.rdmlSniffScore <- function(spec, fileName) {
  if (is.null(spec$sniff)) {
    return(0)
  }

  score <- tryCatch(
    spec$sniff(fileName),
    error = function(e) 0
  )

  if (
    is.logical(score) &&
    length(score) == 1L &&
    !is.na(score)
  ) {
    score <- as.numeric(score)
  }

  if (
    !is.numeric(score) ||
    length(score) != 1L ||
    is.na(score) ||
    !is.finite(score)
  ) {
    return(0)
  }

  min(1, max(0, score))
}


.rdmlChooseCandidates <- function(
    candidates,
    fileName,
    operation,
    useSniff = TRUE) {

  if (!length(candidates)) {
    return(NULL)
  }

  if (length(candidates) == 1L) {
    return(candidates[[1L]])
  }

  if (
    identical(operation, "read") &&
    isTRUE(useSniff)
  ) {
    scores <- vapply(
      candidates,
      .rdmlSniffScore,
      numeric(1),
      fileName = fileName
    )

    bestScore <- max(scores)

    if (bestScore > 0) {
      best <- which(scores == bestScore)

      if (length(best) == 1L) {
        return(candidates[[best]])
      }

      candidates <- candidates[best]
    }
  }

  priorities <- vapply(
    candidates,
    function(spec) spec$priority,
    integer(1)
  )

  bestPriority <- max(priorities)
  best <- which(priorities == bestPriority)

  if (length(best) == 1L) {
    return(candidates[[best]])
  }

  stop(
    "File format is ambiguous between: ",
    paste(
      vapply(candidates, function(spec) spec$name, character(1)),
      collapse = ", "
    ),
    ". Specify `format=` explicitly.",
    call. = FALSE
  )
}


.rdmlResolveExplicitFormat <- function(format, operation) {
  token <- .rdmlNormalizeFormatName(format, "format")
  tokenNoDot <- sub("^\\.", "", token)

  specs <- Filter(
    function(spec) .rdmlFormatSupports(spec, operation),
    .rdmlFormatSpecs()
  )

  # Exact format name has highest precedence.
  exact <- Filter(
    function(spec) identical(spec$name, token),
    specs
  )

  if (length(exact) == 1L) {
    return(exact[[1L]])
  }

  alias <- Filter(
    function(spec) token %in% spec$aliases,
    specs
  )

  if (length(alias) == 1L) {
    return(alias[[1L]])
  }

  extension <- Filter(
    function(spec) tokenNoDot %in% spec$extensions,
    specs
  )

  if (length(extension) == 1L) {
    return(extension[[1L]])
  }

  if (length(alias) > 1L || length(extension) > 1L) {
    choices <- unique(
      c(
        vapply(alias, function(spec) spec$name, character(1)),
        vapply(extension, function(spec) spec$name, character(1))
      )
    )

    stop(
      "Explicit format '",
      format,
      "' is ambiguous between: ",
      paste(choices, collapse = ", "),
      ". Use a registered format name.",
      call. = FALSE
    )
  }

  stop(
    "Unsupported ",
    operation,
    " format: ",
    format,
    ". Registered formats: ",
    paste(
      vapply(specs, function(spec) spec$name, character(1)),
      collapse = ", "
    ),
    call. = FALSE
  )
}


.rdmlResolveAutoFormat <- function(fileName, operation) {
  specs <- Filter(
    function(spec) .rdmlFormatSupports(spec, operation),
    .rdmlFormatSpecs()
  )

  ext <- tolower(tools::file_ext(fileName))

  candidates <- if (nzchar(ext)) {
    Filter(
      function(spec) ext %in% spec$extensions,
      specs
    )
  } else {
    list()
  }

  # Extension-first dispatch.  When no extension matches on read, sniff every
  # readable format so extensionless or unusually named files can still work.
  if (!length(candidates) && identical(operation, "read")) {
    sniffable <- Filter(
      function(spec) !is.null(spec$sniff),
      specs
    )

    scores <- vapply(
      sniffable,
      .rdmlSniffScore,
      numeric(1),
      fileName = fileName
    )

    positive <- which(scores > 0)

    if (length(positive)) {
      candidates <- sniffable[positive]
    }
  }

  chosen <- .rdmlChooseCandidates(
    candidates,
    fileName = fileName,
    operation = operation,
    useSniff = TRUE
  )

  if (!is.null(chosen)) {
    return(chosen)
  }

  if (nzchar(ext)) {
    stop(
      "No registered ",
      operation,
      " format for extension .",
      ext,
      ". Use `rdmlFormats()` to list available handlers or specify `format=`.",
      call. = FALSE
    )
  }

  stop(
    "Could not detect a registered ",
    operation,
    " format for file: ",
    fileName,
    call. = FALSE
  )
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
#' @seealso `rdmlFormats`, `rdmlRead`, `rdmlWrite`
#' @export
rdmlDetectFormat <- function(
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


.rdmlFormatWriteLosses <- function(x, spec) {
  losses <- list()

  if (!("multiTm" %in% spec$capabilities)) {
    experiments <- .rdmlPropList(x, "experiment")

    for (experiment in experiments) {
      expId <- .rdmlIdChr(experiment$id)
      for (run in .rdmlPropList(experiment, "run")) {
        runId <- .rdmlIdChr(run$id)
        for (react in .rdmlPropList(run, "react")) {
          reactId <- .rdmlIdChr(react$id)
          for (dataObj in .rdmlPropList(react, "data")) {
            if (
              .rdmlPresent(dataObj$meltTemps) &&
              length(dataObj$meltTemps) > 1L
            ) {
              targetId <- .rdmlIdChr(dataObj$targetId)
              losses[[length(losses) + 1L]] <- rdmlLossRecord(
                code = "multipleTmUnsupported",
                message = paste0(
                  "Format '", spec$name,
                  "' cannot represent multiple Tm values; only meltTemp is written"
                ),
                path = paste0(
                  "experiment.", expId,
                  ".run.", runId,
                  ".react.", reactId,
                  ".data.", targetId
                ),
                details = list(
                  format = spec$name,
                  values = dataObj$meltTemps
                )
              )
            }
          }
        }
      }
    }
  }

  losses
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

  tree <- asXml(x, loss = "allow")

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

  tree <- asXml(x, loss = "allow")

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


# Built-in sniffers ---------------------------------------------------------

.rdmlReadTextHead <- function(
    fileName,
    bytes = 65536L) {

  size <- file.info(fileName)$size

  if (
    !length(size) ||
    is.na(size) ||
    size <= 0
  ) {
    return("")
  }

  raw <- readBin(
    fileName,
    what = "raw",
    n = min(
      as.double(bytes),
      size
    )
  )

  if (!length(raw)) {
    return("")
  }

  # The sniffers only search ASCII markers, so replacing undecodable bytes is
  # sufficient and avoids imposing a vendor encoding at registry level.
  rawToChar(raw, multiple = FALSE)
}


.rdmlSniffXml <- function(fileName) {
  txt <- tryCatch(
    .rdmlReadTextHead(
      fileName,
      bytes = 32768L
    ),
    error = function(e) ""
  )

  if (!nzchar(txt)) {
    return(0)
  }

  if (
    grepl(
      "<(?:[A-Za-z0-9_.-]+:)?rdml(?:\\s|>)",
      txt,
      perl = TRUE,
      ignore.case = TRUE
    )
  ) {
    return(1)
  }

  0
}


.rdmlSniffFqd <- function(fileName) {
  txt <- tryCatch(
    .rdmlReadTextHead(
      fileName,
      bytes = 131072L
    ),
    error = function(e) ""
  )

  if (!nzchar(txt)) {
    return(0)
  }

  if (
    length(
      strsplit(
        txt,
        "Quan\\.",
        perl = TRUE
      )[[1L]]
    ) >= 4L
  ) {
    return(1)
  }

  0
}


.rdmlSniffDtprime <- function(fileName) {
  raw <- tryCatch(
    readBin(
      fileName,
      what = "raw",
      n = min(
        file.info(fileName)$size,
        131072L
      )
    ),
    error = function(e) raw()
  )

  if (!length(raw)) {
    return(0)
  }

  # DTprime section names are ASCII even in CP1251 files.
  txt <- rawToChar(raw)

  score <- 0

  if (
    grepl(
      "$Information about tubes:$",
      txt,
      fixed = TRUE
    )
  ) {
    score <- score + 0.5
  }

  if (
    grepl(
      "$Results of optical measurements:$",
      txt,
      fixed = TRUE
    )
  ) {
    score <- score + 0.5
  }

  min(1, score)
}


.rdmlSniffRdes <- function(fileName) {
  raw <- tryCatch(
    readBin(
      fileName,
      what = "raw",
      n = min(
        file.info(fileName)$size,
        32768L
      )
    ),
    error = function(e) raw()
  )

  if (!length(raw)) {
    return(0)
  }

  text <- rawToChar(raw)

  if (!validUTF8(text)) {
    return(0)
  }

  firstLine <- strsplit(
    text,
    "\n",
    fixed = TRUE
  )[[1L]][[1L]]

  firstLine <- sub(
    "\r$",
    "",
    firstLine
  )

  fields <- strsplit(
    firstLine,
    "\t",
    fixed = TRUE
  )[[1L]]

  if (length(fields) < 8L) {
    return(0)
  }

  if (
    identical(
      fields[1:6],
      c(
        "Well",
        "Sample",
        "Sample Type",
        "Target",
        "Target Type",
        "Dye"
      )
    ) &&
    fields[[7L]] %in% c(
      "Cq",
      "Tm"
    )
  ) {
    return(1)
  }

  0
}


# Built-in format registration ---------------------------------------------

.rdmlRegisterBuiltinFormats <- function() {

  .rdmlRegisterFormat(
    name = "abi",
    extensions = "eds",
    aliases = "eds",
    reader = function(
        fileName,
        showProgress = TRUE,
        ...) {
      .rdmlImportAbi(
        fileName,
        showProgress
      )
    },
    capabilities = c("adp", "cq", "basicMetadata", "intermediate"),
    builtin = TRUE
  )

  .rdmlRegisterFormat(
    name = "rotorgene",
    extensions = "rex",
    aliases = c(
      "rotor-gene",
      "rex"
    ),
    reader = function(
        fileName,
        showProgress = TRUE,
        ...) {
      .rdmlImportRotorGene(
        fileName,
        showProgress
      )
    },
    capabilities = c("adp", "basicMetadata", "intermediate"),
    builtin = TRUE
  )

  .rdmlRegisterFormat(
    name = "excel",
    extensions = c(
      "xlsx",
      "xls"
    ),
    aliases = c(
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
    capabilities = c("adp", "mdp", "basicMetadata", "intermediate"),
    builtin = TRUE
  )

  .rdmlRegisterFormat(
    name = "csv",
    extensions = "csv",
    aliases = "csv",
    reader = function(
        fileName,
        showProgress = TRUE,
        ...) {
      .rdmlImportCsv(
        fileName,
        showProgress
      )
    },
    capabilities = c("adp", "mdp", "intermediate"),
    builtin = TRUE
  )

  .rdmlRegisterFormat(
    name = "dtprime",
    extensions = "r96",
    aliases = "r96",
    reader = function(
        fileName,
        showProgress = TRUE,
        ...) {
      .rdmlImportDtprime(
        fileName,
        showProgress
      )
    },
    sniff = .rdmlSniffDtprime,
    capabilities = c("adp", "basicMetadata", "intermediate"),
    builtin = TRUE
  )

  .rdmlRegisterFormat(
    name = "fqd",
    extensions = "txt",
    aliases = c(
      "fqd96",
      "txt"
    ),
    reader = function(
        fileName,
        showProgress = TRUE,
        ...) {
      .rdmlImportFqd(
        fileName,
        showProgress
      )
    },
    sniff = .rdmlSniffFqd,
    capabilities = c("adp", "cq", "basicMetadata", "intermediate"),
    builtin = TRUE
  )

  .rdmlRegisterFormat(
    name = "rdes",
    extensions = c(
      "tsv",
      "csv",
      "txt"
    ),
    aliases = c(
      "rdes",
      "tsv"
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
    sniff = .rdmlSniffRdes,
    priority = -10L,
    capabilities = c("adp", "mdp", "cq", "meltTemp", "multiTm", "basicMetadata", "intermediate"),
    builtin = TRUE
  )

  .rdmlRegisterFormat(
    name = "rdml-xml",
    extensions = "xml",
    aliases = "xml",
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
    sniff = .rdmlSniffXml,
    capabilities = c("adp", "mdp", "cq", "meltTemp", "fullMetadata"),
    builtin = TRUE
  )

  .rdmlRegisterFormat(
    name = "rdml",
    extensions = c(
      "rdml",
      "rdm"
    ),
    aliases = "rdm",
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
    capabilities = c("adp", "mdp", "cq", "meltTemp", "fullMetadata"),
    builtin = TRUE
  )

  # Roche LC96 archives are readable through the native RDML importer, but
  # RDML7 does not recreate Roche's additional vendor files on export.
  .rdmlRegisterFormat(
    name = "roche-lc96",
    extensions = "lc96p",
    aliases = c(
      "lc96",
      "lc96p"
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
    capabilities = c("adp", "mdp", "cq", "meltTemp", "fullMetadata"),
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
#' @seealso `rdmlWrite`, `rdmlFormats`, `rdmlValidate`
#' @export
rdmlRead <- function(
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
    result <- rdmlBuildImport(
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
#' @seealso `rdmlRead`, `rdmlFormats`, `rdmlLossRecord`
#' @export
rdmlWrite <- function(
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

  losses <- .rdmlFormatWriteLosses(
    x,
    spec
  )
  .rdmlSignalLosses(
    losses,
    loss = loss
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
#' @param version RDML version string.
#' @param ... Additional arguments forwarded to `rdmlBuildImport()`.
#' @return `rdmlType`.
#' @seealso `setFData`, `rdmlImportData`
#' @export
rdmlFromFData <- function(
    fdata,
    description,
    fdataType = "adp",
    publisher = NULL,
    serialNumber = "1",
    version = "1.2",
    ...) {

  checkmate::assertChoice(
    fdataType,
    c(
      "adp",
      "mdp"
    )
  )

  checkmate::assertString(version)

  importData <- rdmlImportData(
    series = list(
      rdmlImportSeries(
        fdataType = fdataType,
        fdata = data.table::as.data.table(fdata),
        description = data.table::as.data.table(description)
      )
    ),
    publisher = if (is.null(publisher)) NA_character_ else publisher,
    serialNumber = serialNumber,
    version = version,
    format = "fdata"
  )

  rdmlBuildImport(
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
#' and returns one or more specifications accepted by `rdmlRegisterFormat()`.
#'
#' @param path Module R file.
#' @param overwrite Allow replacement of an existing format.
#' @return Registered format names invisibly.
#' @seealso `rdmlRegisterFormat`, `rdmlFormats`
#' @export
rdmlLoadModule <- function(
    path,
    overwrite = FALSE) {

  checkmate::assertString(path)
  checkmate::assertFlag(overwrite)

  if (!file.exists(path)) {
    stop(
      "RDML module does not exist: ",
      path,
      call. = FALSE
    )
  }

  moduleEnv <- new.env(
    parent = environment(rdmlLoadModule)
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
    "writer",
    "sniff",
    "aliases",
    "priority",
    "apiVersion",
    "capabilities"
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
        paste(unknown, collapse = ", "),
        call. = FALSE
      )
    }

    spec$overwrite <- overwrite

    do.call(
      rdmlRegisterFormat,
      spec
    )

    registered[[i]] <- .rdmlNormalizeFormatName(
      spec$name,
      "name"
    )
  }

  invisible(registered)
}
