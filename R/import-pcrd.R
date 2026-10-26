# Bio-Rad CFX PCRD importer -------------------------------------------------

.pcrdAbort <- function(message, fileName = NULL) {
  if (!is.null(fileName)) message <- paste0(message, ": ", basename(fileName))
  stop(message, call. = FALSE)
}

.pcrdUInt16 <- function(x, offset) {
  sum(as.numeric(x[offset + seq_len(2L)]) * 256^(0:1))
}

.pcrdUInt32 <- function(x, offset) {
  sum(as.numeric(x[offset + seq_len(4L)]) * 256^(0:3))
}

.pcrdSigned <- function(x) {
  if (x >= 2^31) x - 2^32 else x
}

.pcrdUnsigned <- function(x) {
  x <- as.numeric(x)
  if (x < 0) x + 2^32 else x
}

.pcrdMultiply32 <- function(a, b) {
  aLow <- a %% 65536
  aHigh <- floor(a / 65536)
  bLow <- b %% 65536
  bHigh <- floor(b / 65536)
  (aLow * bLow + ((aLow * bHigh + aHigh * bLow) %% 65536) * 65536) %% 2^32
}

.pcrdCrcTable <- local({
  table <- NULL
  function() {
    if (!is.null(table)) return(table)
    table <<- vapply(0:255, function(i) {
      crc <- i
      for (j in seq_len(8L)) {
        crc <- if (bitwAnd(as.integer(crc), 1L)) {
          bitwXor(bitwShiftR(as.integer(crc), 1L), .pcrdSigned(0xedb88320))
        } else bitwShiftR(as.integer(crc), 1L)
      }
      .pcrdUnsigned(crc)
    }, numeric(1))
    table
  }
})

.pcrdCrcByte <- function(crc, byte) {
  index <- bitwAnd(bitwXor(.pcrdSigned(crc), as.integer(byte)), 255L) + 1L
  bitwXor(
    bitwShiftR(.pcrdSigned(crc), 8L),
    .pcrdSigned(.pcrdCrcTable()[[index]])
  ) |> .pcrdUnsigned()
}

.pcrdDecryptR <- function(data, password) {
  keys <- c(0x12345678, 0x23456789, 0x34567890)
  updateKeys <- function(value) {
    keys[[1L]] <<- .pcrdCrcByte(keys[[1L]], value)
    keys[[2L]] <<- (keys[[2L]] + bitwAnd(.pcrdSigned(keys[[1L]]), 255L)) %% 2^32
    keys[[2L]] <<- (.pcrdMultiply32(keys[[2L]], 134775813) + 1) %% 2^32
    keys[[3L]] <<- .pcrdCrcByte(keys[[3L]], bitwShiftR(.pcrdSigned(keys[[2L]]), 24L))
  }
  for (value in charToRaw(password)) updateKeys(as.integer(value))
  out <- raw(length(data))
  for (i in seq_along(data)) {
    temp <- bitwOr(as.integer(keys[[3L]] %% 65536), 2L)
    paired <- bitwXor(temp, 1L)
    mask <- floor((as.numeric(temp) * as.numeric(paired)) / 256) %% 256
    value <- bitwXor(as.integer(data[[i]]), as.integer(mask))
    out[[i]] <- as.raw(value)
    updateKeys(value)
  }
  out
}

.pcrdDecrypt <- function(data, password) {
  if (is.loaded("C_pcrd_decrypt", PACKAGE = "RDML7")) {
    return(.Call("C_pcrd_decrypt", data, password, PACKAGE = "RDML7"))
  }
  .pcrdDecryptR(data, password)
}

.pcrdExtractXml <- function(fileName) {
  bytes <- readBin(fileName, "raw", n = file.size(fileName))
  signature <- which(
    bytes == as.raw(80L) &
      c(bytes[-1L], as.raw(0L)) == as.raw(75L) &
      c(bytes[-c(1L, 2L)], as.raw(c(0L, 0L))) == as.raw(3L) &
      c(bytes[-c(1L, 2L, 3L)], as.raw(c(0L, 0L, 0L))) == as.raw(4L)
  )
  if (length(bytes) < 42L || !length(signature)) {
    .pcrdAbort("Unsupported Bio-Rad PCRD archive", fileName)
  }
  bytes <- bytes[signature[[1L]]:length(bytes)]
  flags <- .pcrdUInt16(bytes, 6L)
  method <- .pcrdUInt16(bytes, 8L)
  modificationTime <- .pcrdUInt16(bytes, 10L)
  crc <- .pcrdUInt32(bytes, 14L)
  compressedSize <- .pcrdUInt32(bytes, 18L)
  uncompressedSize <- .pcrdUInt32(bytes, 22L)
  nameSize <- .pcrdUInt16(bytes, 26L)
  extraSize <- .pcrdUInt16(bytes, 28L)
  start <- 30L + nameSize + extraSize
  if (!bitwAnd(flags, 1L) || method != 8L || start + compressedSize > length(bytes)) {
    .pcrdAbort("Unsupported PCRD ZIP encoding", fileName)
  }
  encrypted <- bytes[start + seq_len(compressedSize)]
  # password obtained here https://github.com/tomzzzhang/SHARP-processor/blob/master/PCRD_FORMAT.md
  decrypted <- .pcrdDecrypt(encrypted, "SecureCompressDecompressKeyiQ5V4Files!!##$$")
  checkByte <- if (bitwAnd(flags, 8L)) bitwShiftR(modificationTime, 8L) else bitwShiftR(.pcrdSigned(crc), 24L)
  if (length(decrypted) < 13L || as.integer(decrypted[[12L]]) != checkByte) {
    .pcrdAbort("PCRD decryption failed", fileName)
  }
  # Wrap the raw DEFLATE stream in a gzip container. ZIP and gzip use the same
  # DEFLATE payload and CRC32, so base R can decompress it without an add-on.
  gzip <- c(
    as.raw(c(31, 139, 8, 0, 0, 0, 0, 0, 0, 255)),
    decrypted[-seq_len(12L)],
    as.raw(floor(crc / 256^(0:3)) %% 256),
    as.raw(floor(uncompressedSize / 256^(0:3)) %% 256)
  )
  tryCatch(memDecompress(gzip, type = "gzip"), error = function(e) {
    .pcrdAbort("Invalid compressed XML in PCRD archive", fileName)
  })
}

.pcrdSampleType <- function(x) {
  switch(x, wcNTC = "ntc", wcPositiveControl = "pos", wcStandard = "std", "unkn")
}

.pcrdPosition <- function(index, columns) {
  paste0(intToUtf8(65L + index %/% columns), index %% columns + 1L)
}

.rdmlImportPcrd <- function(fileName, showProgress = TRUE) {
  xml <- tryCatch(xml2::read_xml(.pcrdExtractXml(fileName)), error = function(e) {
    .pcrdAbort(paste("Cannot parse PCRD XML:", conditionMessage(e)), fileName)
  })
  if (xml2::xml_name(xml2::xml_root(xml)) != "experimentalData2") {
    .pcrdAbort("Unsupported PCRD XML document", fileName)
  }

  plate <- xml2::xml_find_first(xml, "//plateSetup2")
  columns <- as.integer(xml2::xml_attr(plate, "columns"))
  layers <- xml2::xml_find_all(plate, "./dyeLayersList/dyeLayer")
  reads <- xml2::xml_find_all(xml, "//runData/plateReadDataVector/plateRead/PlateRead")
  cycles <- as.numeric(xml2::xml_text(xml2::xml_find_first(reads, "./Hdr/PlateReadDataHeader/Cycle")))
  if (!length(layers) || !length(reads) || is.na(columns)) {
    .pcrdAbort("PCRD plate setup or fluorescence data was not found", fileName)
  }
  values <- lapply(reads, function(node) {
    as.numeric(strsplit(xml2::xml_text(xml2::xml_find_first(node, "./Data/PAr")), ";", fixed = TRUE)[[1L]])
  })
  if (any(lengths(values) < 108L * 4L * 6L)) {
    .pcrdAbort("Unsupported PCRD fluorescence array", fileName)
  }
  values <- do.call(rbind, values)

  expId <- tools::file_path_sans_ext(basename(fileName))
  kvNodes <- xml2::xml_find_all(xml, "//protocolRunInfo/RunInfo/KeyValuePairs")
  kv <- setNames(
    xml2::xml_text(xml2::xml_find_first(kvNodes, "./Value")),
    xml2::xml_text(xml2::xml_find_first(kvNodes, "./Key"))
  )
  runId <- if (!is.null(kv[["RunStartTime"]])) kv[["RunStartTime"]] else "Bio-Rad CFX run"
  fluorescence <- list(cyc = cycles)
  descriptions <- list()

  for (layer in layers) {
    dye <- xml2::xml_attr(layer, "plateName")
    channel <- as.integer(xml2::xml_attr(xml2::xml_find_first(layer, "./fluor"), "channelPosition"))
    wells <- xml2::xml_find_all(layer, ".//wellSample[@wellLoadedFluor='True']")
    for (well in wells) {
      index <- as.integer(xml2::xml_attr(well, "plateIndex"))
      position <- .pcrdPosition(index, columns)
      quantity <- xml2::xml_attr(well, "sampleQuantity")
      condition <- xml2::xml_attr(well, "conditionName")
      sample <- if (!is.na(condition) && nzchar(condition)) condition else if (!is.na(quantity) && quantity != "NaN") quantity else paste("Well", position)
      target <- xml2::xml_attr(well, "geneName")
      if (is.na(target) || !nzchar(target)) target <- dye
      name <- make.unique(c(names(fluorescence), paste(position, target, dye, sep = "_")))[[length(fluorescence) + 1L]]
      arrayIndex <- channel * 108L * 4L + index * 4L + 1L
      fluorescence[[name]] <- values[, arrayIndex]
      descriptions[[length(descriptions) + 1L]] <- data.frame(
        fdataName = name, expId = expId, runId = runId,
        reactId = index + 1L, position = position, sample = sample,
        sampleType = .pcrdSampleType(xml2::xml_attr(well, "wellSampleType")),
        target = target, targetDyeId = dye, targetType = "toi",
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(descriptions)) .pcrdAbort("PCRD contains no assigned wells", fileName)
  fluorescence <- as.data.frame(
    fluorescence, check.names = FALSE, stringsAsFactors = FALSE
  )
  metadata <- list(
    expId = expId, runId = runId,
    operator = if (!is.null(kv[["Username"]])) kv[["Username"]] else "",
    startDate = if (!is.null(kv[["RunStartTime"]])) kv[["RunStartTime"]] else "",
    serialNumber = if (!is.null(kv[["BaseSerialNumber"]])) kv[["BaseSerialNumber"]] else "unknown",
    softwareVersion = xml2::xml_attr(xml2::xml_find_first(xml, "/experimentalData2/header"), "createdByClientAppVersion")
  )
  importData <- newRDMLImportData(
    series = list(newRDMLImportSeries(
      fdataType = "adp", fdata = data.table::as.data.table(fluorescence),
      description = data.table::as.data.table(do.call(rbind, descriptions))
    )), publisher = "Bio-Rad", serialNumber = metadata$serialNumber,
    format = "biorad-pcrd", metadata = metadata
  )
  buildRDMLImport(importData, loss = "allow")
}
