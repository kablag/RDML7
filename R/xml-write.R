# XML serialization --------------------------------------------------------
# Single serializer for S7 RDML objects. Legacy XML serializer removed; AsXML() is the single writer.

.rdml_xml_value <- function(value, node_name) {

  if (.rdml_is_missing(value)) {
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
              .rdml_xml_node(item, node_name),
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
              .rdml_xml_node(item, node_name),
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
        .rdml_xml_node(value, node_name),
        collapse = ""
      )
    )
  }

  # Atomic value.
  paste0(
    .rdml_xml_atomic(node_name, value),
    collapse = ""
  )
}

.rdml_check_fpoints <- function(dt, required, type) {
  
  if (!is.data.frame(dt)) {
    stop(
      type,
      "@fpoints must be a data.frame/data.table",
      call. = FALSE
    )
  }
  
  missing_columns <- setdiff(
    required,
    names(dt)
  )
  
  if (length(missing_columns)) {
    stop(
      type,
      "@fpoints is missing column(s): ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


# XML serialization --------------------------------------------------------
# This serializer follows the current S7 schema rather than the old R6
# private fields. It also maps rdml7 internal property names back to RDML XML
# names (targetId -> tar for data and partitionData).

.rdml_xml_escape <- function(x, attribute = FALSE) {
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

.rdml_xml_name <- function(object, property) {
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

.rdml_xml_atomic <- function(name, value) {
  if (.rdml_is_missing(value)) return(character())
  text <- if (is.logical(value)) {
    ifelse(value, "true", "false")
  } else {
    .rdml_xml_escape(value)
  }
  sprintf("<%s>%s</%s>", name, text, name)
}

.rdml_xml_node <- function(x, node_name) {
  if (.rdml_is_missing(x)) return(character())

  # Enum values are element text.
  if (S7::S7_inherits(x, rdmlEnum)) {
    return(sprintf(
      "<%s>%s</%s>", node_name, .rdml_xml_escape(as.character(x)), node_name
    ))
  }

  # References and ids are represented by an id attribute.
  if (S7::S7_inherits(x, idType)) {
    return(sprintf(
      '<%s id="%s"/>', node_name, .rdml_xml_escape(as.character(x), TRUE)
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
    
    missing_columns <- setdiff(
      required,
      names(dt)
    )
    
    if (length(missing_columns)) {
      stop(
        "dpAmpCurveType@fpoints is missing column(s): ",
        paste(
          missing_columns,
          collapse = ", "
        ),
        call. = FALSE
      )
    }
    
    has_tmp <- "tmp" %in% names(dt)
    
    return(
      vapply(
        seq_len(nrow(dt)),
        function(i) {
          
          if (
            has_tmp &&
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
              .rdml_xml_escape(dt$cyc[[i]]),
              .rdml_xml_escape(dt$tmp[[i]]),
              .rdml_xml_escape(dt$fluor[[i]])
            )
            
          } else {
            
            sprintf(
              paste0(
                "<adp>",
                "<cyc>%s</cyc>",
                "<fluor>%s</fluor>",
                "</adp>"
              ),
              .rdml_xml_escape(dt$cyc[[i]]),
              .rdml_xml_escape(dt$fluor[[i]])
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
    
    missing_columns <- setdiff(
      required,
      names(dt)
    )
    
    if (length(missing_columns)) {
      stop(
        "dpMeltingCurveType@fpoints is missing column(s): ",
        paste(
          missing_columns,
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
            .rdml_xml_escape(dt$tmp[[i]]),
            .rdml_xml_escape(dt$fluor[[i]])
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
        node_name,
        .rdml_xml_escape(.rdml_id_chr(x$targetId), TRUE),
        .rdml_xml_escape(.rdml_enum_chr(x$sampleType)),
        node_name
      )
    )
  }

  # quantity has a targetId attribute plus value/unit children.
  if (S7::S7_inherits(x, quantityType)) {
    body <- c(
      .rdml_xml_atomic("value", x$value),
      .rdml_xml_node(x$unit, "unit")
    )
    return(sprintf(
      '<%s targetId="%s">%s</%s>',
      node_name,
      .rdml_xml_escape(.rdml_id_chr(x$targetId), TRUE),
      paste0(body, collapse = ""),
      node_name
    ))
  }

  if (!S7::S7_inherits(x, rdmlBaseType)) {
    return(.rdml_xml_atomic(node_name, x))
  }

  properties <- S7::prop_names(x)
  attributes <- ""

  # All RDML entities whose first schema property is id use it as XML attr.
  if ("id" %in% properties && S7::S7_inherits(S7::prop(x, "id"), idType)) {
    attributes <- sprintf(
      ' id="%s"',
      .rdml_xml_escape(.rdml_id_chr(S7::prop(x, "id")), TRUE)
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
    
    if (.rdml_is_missing(value)) {
      next
    }
    
    xml_name <- .rdml_xml_name(
      x,
      property
    )
    
    child <- .rdml_xml_value(
      value,
      xml_name
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
    if (.rdml_is_missing(version)) version <- "1.2"
    attributes <- paste0(
      ' xmlns="http://www.rdml.org" version="',
      .rdml_xml_escape(version, TRUE),
      '"'
    )
  }

  sprintf(
    "<%s%s>%s</%s>",
    node_name, attributes, paste0(children, collapse = ""), node_name
  )
}

.rdml_output_path <- function(file.name) {
  checkmate::assertString(file.name)

  # A bare file name is explicitly placed in the current working directory.
  if (identical(dirname(file.name), ".")) {
    file.name <- file.path(getwd(), basename(file.name))
  } else {
    is_absolute <- grepl(
      "^(?:[A-Za-z]:[/\\\\]|[/\\\\]{2}|/)",
      file.name
    )

    if (!is_absolute) {
      file.name <- file.path(getwd(), file.name)
    }
  }

  out_dir <- dirname(file.name)

  if (!dir.exists(out_dir)) {
    stop(
      "Output directory does not exist: ",
      out_dir,
      call. = FALSE
    )
  }

  file.path(
    normalizePath(
      out_dir,
      winslash = "/",
      mustWork = TRUE
    ),
    basename(file.name)
  )
}


#' Serialize rdmlType as XML or a .rdml zip archive
#' @param x rdmlType object.
#' @param file.name Optional destination. If omitted, XML text is returned.
#' @return XML text invisibly when file.name is supplied; otherwise XML text.
S7::method(AsXML, rdmlType) <- function(x, file.name) {

  tree <- .rdml_xml_node(x, "rdml")

  if (missing(file.name)) {
    return(tree)
  }

  file.name <- .rdml_output_path(file.name)
  ext <- tolower(tools::file_ext(file.name))

  # Plain XML ----------------------------------------------------------
  if (identical(ext, "xml")) {
    writeLines(
      enc2utf8(tree),
      con = file.name,
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

  xml_file <- file.path(tmpdir, "rdml_data.xml")
  writeLines(
    enc2utf8(tree),
    con = xml_file,
    useBytes = TRUE
  )

  zip_file <- tempfile(fileext = ".zip")
  on.exit(unlink(zip_file, force = TRUE), add = TRUE)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  setwd(tmpdir)
  zip_status <- utils::zip(
    zipfile = zip_file,
    files = "rdml_data.xml"
  )
  setwd(old_wd)

  # utils::zip() commonly returns 0 on success; tolerate NULL for
  # implementations where no explicit status is returned.
  if (
    !file.exists(zip_file) ||
    (!is.null(zip_status) && length(zip_status) == 1L &&
       is.numeric(zip_status) && zip_status != 0)
  ) {
    stop("Failed to create RDML ZIP archive", call. = FALSE)
  }

  if (!file.copy(zip_file, file.name, overwrite = TRUE)) {
    stop(
      "Failed to write RDML file: ",
      file.name,
      call. = FALSE
    )
  }

  invisible(tree)
}
