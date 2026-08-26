# XML serialization --------------------------------------------------------
# Single serializer for S7 RDML objects. Legacy XML serializer removed; asXml() is the single writer.

.rdmlXmlValue <- function(value, nodeName) {

  if (.rdmlIsMissing(value)) {
    return("")
  }

  # rdmlKeyedList is an S7 object AND a list-like wrapper.
  # It must be unwrapped before the generic S7 branch.
  if (S7::S7_inherits(value, rdmlKeyedList)) {
    value <- S7::S7_data(value)

    if (!length(value)) {
      return("")
    }

    return(
      paste0(
        vapply(
          value,
          function(item) {
            paste0(
              .rdmlXmlNode(item, nodeName),
              collapse = ""
            )
          },
          character(1)
        ),
        collapse = ""
      )
    )
  }

  # Ordinary list-valued RDML properties.
  if (is.list(value)) {
    if (!length(value)) {
      return("")
    }

    return(
      paste0(
        vapply(
          value,
          function(item) {
            paste0(
              .rdmlXmlNode(item, nodeName),
              collapse = ""
            )
          },
          character(1)
        ),
        collapse = ""
      )
    )
  }

  # Any remaining S7 object is serialized recursively.
  if (S7::S7_inherits(value)) {
    return(
      paste0(
        .rdmlXmlNode(value, nodeName),
        collapse = ""
      )
    )
  }

  # Atomic value.
  paste0(
    .rdmlXmlAtomic(nodeName, value),
    collapse = ""
  )
}

.rdmlCheckFPoints <- function(dt, required, type) {
  
  if (!is.data.frame(dt)) {
    stop(
      type,
      "@fpoints must be a data.frame/data.table",
      call. = FALSE
    )
  }
  
  missingColumns <- setdiff(
    required,
    names(dt)
  )
  
  if (length(missingColumns)) {
    stop(
      type,
      "@fpoints is missing column(s): ",
      paste(missingColumns, collapse = ", "),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


# XML serialization --------------------------------------------------------
# This serializer follows the current S7 schema rather than the old R6
# private fields. It also maps rdml7 internal property names back to RDML XML
# names (targetId -> tar for data and partitionData).

.rdmlXmlEscape <- function(x, attribute = FALSE) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  if (attribute) {
    x <- gsub('"', "&quot;", x, fixed = TRUE)
    x <- gsub("'", "&apos;", x, fixed = TRUE)
  }
  x
}

.rdmlXmlName <- function(object, property) {
  if (
    identical(property, "targetId") &&
    (
      S7::S7_inherits(object, dataType) ||
      S7::S7_inherits(object, partitionDataType)
    )
  ) {
    return("tar")
  }

  property
}

.rdmlXmlAtomic <- function(name, value) {
  if (.rdmlIsMissing(value)) return(character())
  text <- if (is.logical(value)) {
    ifelse(value, "true", "false")
  } else {
    .rdmlXmlEscape(value)
  }
  sprintf("<%s>%s</%s>", name, text, name)
}

.rdmlXmlNode <- function(x, nodeName) {
  if (.rdmlIsMissing(x)) return(character())

  # Enum values are element text.
  if (S7::S7_inherits(x, rdmlEnum)) {
    return(sprintf(
      "<%s>%s</%s>", nodeName, .rdmlXmlEscape(as.character(x)), nodeName
    ))
  }

  # References and ids are represented by an id attribute.
  if (S7::S7_inherits(x, idType)) {
    return(sprintf(
      '<%s id="%s"/>', nodeName, .rdmlXmlEscape(as.character(x), TRUE)
    ))
  }

  if (S7::S7_inherits(x, dpAmpCurveType)) {
    
    dt <- S7::prop(
      x,
      "fpoints"
    )
    
    if (
      is.null(dt) ||
      !nrow(dt)
    ) {
      return(character())
    }
    
    required <- c(
      "cyc",
      "fluor"
    )
    
    missingColumns <- setdiff(
      required,
      names(dt)
    )
    
    if (length(missingColumns)) {
      stop(
        "dpAmpCurveType@fpoints is missing column(s): ",
        paste(
          missingColumns,
          collapse = ", "
        ),
        call. = FALSE
      )
    }
    
    hasTmp <- "tmp" %in% names(dt)
    
    return(
      vapply(
        seq_len(nrow(dt)),
        function(i) {
          
          if (
            hasTmp &&
            !is.na(dt$tmp[[i]])
          ) {
            
            sprintf(
              paste0(
                "<adp>",
                "<cyc>%s</cyc>",
                "<tmp>%s</tmp>",
                "<fluor>%s</fluor>",
                "</adp>"
              ),
              .rdmlXmlEscape(dt$cyc[[i]]),
              .rdmlXmlEscape(dt$tmp[[i]]),
              .rdmlXmlEscape(dt$fluor[[i]])
            )
            
          } else {
            
            sprintf(
              paste0(
                "<adp>",
                "<cyc>%s</cyc>",
                "<fluor>%s</fluor>",
                "</adp>"
              ),
              .rdmlXmlEscape(dt$cyc[[i]]),
              .rdmlXmlEscape(dt$fluor[[i]])
            )
          }
        },
        character(1)
      )
    )
  }

  if (S7::S7_inherits(x, dpMeltingCurveType)) {
    
    dt <- S7::prop(
      x,
      "fpoints"
    )
    
    if (
      is.null(dt) ||
      !nrow(dt)
    ) {
      return(character())
    }
    
    required <- c(
      "tmp",
      "fluor"
    )
    
    missingColumns <- setdiff(
      required,
      names(dt)
    )
    
    if (length(missingColumns)) {
      stop(
        "dpMeltingCurveType@fpoints is missing column(s): ",
        paste(
          missingColumns,
          collapse = ", "
        ),
        call. = FALSE
      )
    }
    
    return(
      vapply(
        seq_len(nrow(dt)),
        function(i) {
          
          sprintf(
            paste0(
              "<mdp>",
              "<tmp>%s</tmp>",
              "<fluor>%s</fluor>",
              "</mdp>"
            ),
            .rdmlXmlEscape(dt$tmp[[i]]),
            .rdmlXmlEscape(dt$fluor[[i]])
          )
        },
        character(1)
      )
    )
  }

  # sampleType$type is represented as:
  # <type targetId="...">unkn</type>
  if (S7::S7_inherits(x, sampleTargetType)) {
    return(
      sprintf(
        '<%s targetId="%s">%s</%s>',
        nodeName,
        .rdmlXmlEscape(.rdmlIdChr(x$targetId), TRUE),
        .rdmlXmlEscape(.rdmlEnumChr(x$sampleType)),
        nodeName
      )
    )
  }

  # quantity has a targetId attribute plus value/unit children.
  if (S7::S7_inherits(x, quantityType)) {
    body <- c(
      .rdmlXmlAtomic("value", x$value),
      .rdmlXmlNode(x$unit, "unit")
    )
    return(sprintf(
      '<%s targetId="%s">%s</%s>',
      nodeName,
      .rdmlXmlEscape(.rdmlIdChr(x$targetId), TRUE),
      paste0(body, collapse = ""),
      nodeName
    ))
  }

  if (!S7::S7_inherits(x, rdmlBaseType)) {
    return(.rdmlXmlAtomic(nodeName, x))
  }

  properties <- S7::prop_names(x)
  attributes <- ""

  # All RDML entities whose first schema property is id use it as XML attr.
  if ("id" %in% properties && S7::S7_inherits(S7::prop(x, "id"), idType)) {
    attributes <- sprintf(
      ' id="%s"',
      .rdmlXmlEscape(.rdmlIdChr(S7::prop(x, "id")), TRUE)
    )
    properties <- setdiff(properties, "id")
  }

  children <- character()
  
  for (property in properties) {
    
    # version у корневого rdml — attribute
    if (
      S7::S7_inherits(x, rdmlType) &&
      identical(property, "version")
    ) {
      next
    }
    
    value <- S7::prop(
      x,
      property
    )
    
    if (.rdmlIsMissing(value)) {
      next
    }
    
    xmlName <- .rdmlXmlName(
      x,
      property
    )
    
    child <- .rdmlXmlValue(
      value,
      xmlName
    )
    
    if (length(child) && nzchar(child)) {
      children <- c(
        children,
        child
      )
    }
  }

  if (S7::S7_inherits(x, rdmlType)) {
    version <- x$version
    if (.rdmlIsMissing(version)) version <- "1.2"
    attributes <- paste0(
      ' xmlns="http://www.rdml.org" version="',
      .rdmlXmlEscape(version, TRUE),
      '"'
    )
  }

  sprintf(
    "<%s%s>%s</%s>",
    nodeName, attributes, paste0(children, collapse = ""), nodeName
  )
}

.rdmlOutputPath <- function(fileName) {
  checkmate::assertString(fileName)

  # A bare file name is explicitly placed in the current working directory.
  if (identical(dirname(fileName), ".")) {
    fileName <- file.path(getwd(), basename(fileName))
  } else {
    isAbsolute <- grepl(
      "^(?:[A-Za-z]:[/\\\\]|[/\\\\]{2}|/)",
      fileName
    )

    if (!isAbsolute) {
      fileName <- file.path(getwd(), fileName)
    }
  }

  outDir <- dirname(fileName)

  if (!dir.exists(outDir)) {
    stop(
      "Output directory does not exist: ",
      outDir,
      call. = FALSE
    )
  }

  file.path(
    normalizePath(
      outDir,
      winslash = "/",
      mustWork = TRUE
    ),
    basename(fileName)
  )
}


#' Serialize rdmlType as XML or a .rdml zip archive
#' @param x rdmlType object.
#' @param fileName Optional destination. If omitted, XML text is returned.
#' @return XML text invisibly when fileName is supplied; otherwise XML text.
S7::method(asXml, rdmlType) <- function(x, fileName) {

  tree <- .rdmlXmlNode(x, "rdml")

  if (missing(fileName)) {
    return(tree)
  }

  fileName <- .rdmlOutputPath(fileName)
  ext <- tolower(tools::file_ext(fileName))

  # Plain XML ----------------------------------------------------------
  if (identical(ext, "xml")) {
    writeLines(
      enc2utf8(tree),
      con = fileName,
      useBytes = TRUE
    )

    return(invisible(tree))
  }

  # RDML ZIP archive --------------------------------------------------
  tmpdir <- tempfile("rdml_")
  if (!dir.create(tmpdir)) {
    stop("Failed to create temporary RDML directory", call. = FALSE)
  }
  on.exit(unlink(tmpdir, recursive = TRUE, force = TRUE), add = TRUE)

  xmlFile <- file.path(tmpdir, "rdml_data.xml")
  writeLines(
    enc2utf8(tree),
    con = xmlFile,
    useBytes = TRUE
  )

  zipFile <- tempfile(fileext = ".zip")
  on.exit(unlink(zipFile, force = TRUE), add = TRUE)

  oldWd <- getwd()
  on.exit(setwd(oldWd), add = TRUE)

  setwd(tmpdir)
  zipStatus <- utils::zip(
    zipfile = zipFile,
    files = "rdml_data.xml"
  )
  setwd(oldWd)

  # utils::zip() commonly returns 0 on success; tolerate NULL for
  # implementations where no explicit status is returned.
  if (
    !file.exists(zipFile) ||
    (!is.null(zipStatus) && length(zipStatus) == 1L &&
       is.numeric(zipStatus) && zipStatus != 0)
  ) {
    stop("Failed to create RDML ZIP archive", call. = FALSE)
  }

  if (!file.copy(zipFile, fileName, overwrite = TRUE)) {
    stop(
      "Failed to write RDML file: ",
      fileName,
      call. = FALSE
    )
  }

  invisible(tree)
}
