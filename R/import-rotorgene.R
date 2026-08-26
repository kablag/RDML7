# fromRotorGene importer -----------------------------------------------------------

.rdml_import_rotorgene <- function(filename, show.progress = TRUE) {
  fromRotorGene <- function() {
    dat <- xml2::read_xml(filename)
    rdml.env$ns <- xml2::xml_ns(dat)

    sample_nodes <- xml2::xml_find_all(
      dat,
      "/Experiment/Samples/Page/Sample[Name]"
    )

    if (!length(sample_nodes)) {
      stop("No samples found in Rotor-Gene .rex file", call. = FALSE)
    }

    description <- do.call(
      rbind,
      lapply(seq_along(sample_nodes), function(i) {
        el <- sample_nodes[[i]]
        type_raw <- getTextValue(el, "Type")
        type_map <- c("5" = "pos", "3" = "ntc", "1" = "std")
        sample_type <- unname(type_map[as.character(type_raw)])
        if (!length(sample_type) || is.na(sample_type)) sample_type <- "unkn"

        quantity <- .rdml_as_numeric(getTextValue(el, "GivenConc"))
        if (!length(quantity)) quantity <- NA_real_ else quantity <- quantity[[1L]]

        data.frame(
          fdata.name = getTextValue(el, "ID"),
          exp.id = "exp1",
          run.id = "run1",
          react.id = base::as.numeric(getTextValue(el, "TubePosition")),
          sample = getTextValue(el, "Name"),
          sample.type = sample_type,
          quantity = quantity,
          target = NA_character_,
          target.dyeId = NA_character_,
          stringsAsFactors = FALSE
        )
      })
    )

    groups <- xml2::xml_find_all(dat, "/Experiment/Samples/Groups/Group")
    for (i in seq_along(groups)) {
      group <- groups[[i]]
      group_name <- getTextValue(group, "Name")
      tube_nodes <- xml2::xml_find_all(group, ".//Tube")
      ids <- xml2::xml_text(tube_nodes)
      description$target[description$fdata.name %in% ids] <- group_name
    }

    original.targets <- description$target
    original.targets[is.na(original.targets) | !nzchar(original.targets)] <- "unkn"

    channels <- xml2::xml_find_all(dat, "/Experiment/RawChannels/RawChannel")
    if (!length(channels)) {
      stop("No raw channels found in Rotor-Gene .rex file", call. = FALSE)
    }

    x <- .rdml_new_import("RotorGene", "1")

    for (i in seq_along(channels)) {
      rawChannel <- channels[[i]]
      dye_id <- getTextValue(rawChannel, "Name")
      description$target.dyeId <- dye_id
      description$target <- paste(original.targets, dye_id, sep = "#")

      readings <- getTextVector(
        rawChannel,
        sprintf("Name[text()='%s']/../Reading", dye_id)
      )
      if (!length(readings)) {
        readings <- xml2::xml_text(xml2::xml_find_all(rawChannel, ".//Reading"))
      }

      ridx <- base::as.integer(description$react.id)
      selected <- readings[ridx]
      curves <- lapply(selected, function(z) {
        if (is.na(z) || !nzchar(z)) return(numeric())
        .rdml_as_numeric(.split_ws(z))
      })

      lens <- lengths(curves)
      if (!length(lens) || any(lens == 0L)) next
      if (length(unique(lens)) != 1L) {
        stop("Rotor-Gene fluorescence curves have unequal lengths", call. = FALSE)
      }

      mat <- do.call(cbind, curves)
      fdata <- data.frame(cyc = seq_len(nrow(mat)), check.names = FALSE)
      for (j in seq_len(ncol(mat))) {
        fdata[[as.character(description$fdata.name[[j]])]] <- mat[, j]
      }

      x <- .rdml_set_fdata_import(x, fdata, description, "adp")
    }

    x
  }

  fromRotorGene()
}
