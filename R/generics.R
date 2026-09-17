# Public S7 generics --------------------------------------------------------

#' Build a metadata table from an RDML object
#'
#' Creates one metadata row for every target-specific `dataType` stored in an
#' RDML object. This table can be inspected directly or supplied as `request`
#' to [getFData()].
#'
#' @param x An [rdmlType] object.
#' @param ... Named expressions defining additional columns. Expressions are
#'   evaluated once per `dataType`; see **Custom columns** below.
#'
#' @section Arguments for the `rdmlType` method:
#' \describe{
#'   \item{`columns`}{Character vector selecting built-in columns, or `NULL`
#'   for all built-in columns. Available values are `expId`, `runId`,
#'   `reactId`, `position`, `sample`, `target`, `targetDyeId`, `sampleType`,
#'   `adp`, and `mdp`.}
#'   \item{`namePattern`}{Character template or expression used to create the
#'   unique `fdataName` for each curve. Character templates use `\{field\}`
#'   placeholders, for example `"\{position\}_\{target\}"`.}
#'   \item{`includeHidden`}{Logical. Include experiments whose identifiers
#'   start with `.`? Defaults to `FALSE`.}
#'   \item{`treatNullAsNa`}{Logical. Convert `NULL` results from custom
#'   expressions to `NA`? Defaults to `FALSE`.}
#'   \item{`default`, `addColumns`}{Deprecated compatibility arguments for
#'   named lists of base and additional expressions. New code should use
#'   `columns` and named expressions in `...`.}
#' }
#'
#' @section Custom columns:
#' Expressions in `...` can use the scalar values `expId`, `runId`, `reactId`,
#' `position`, `sample`, `target`, `targetDyeId`, `sampleType`, `adp`, and
#' `mdp`. They can also inspect the current `experiment`, `run`, `react`, and
#' `data` objects and the top-level `samples`, `targets`, `dateMade`,
#' `dateUpdated`, `id`, `experimenter`, `documentation`, `dye`, and
#' `thermalCyclingConditions` collections.
#'
#' Named custom columns may be referenced by `namePattern` placeholders.
#'
#' @return A keyed [data.table::data.table] with `fdataName` followed by the
#'   selected built-in and custom columns. Positions use canonical plate
#'   labels such as `A01`.
#' @examples
#' rdmlFile <- system.file("extdata", "lc96_bACTXY.rdml", package = "RDML7")
#' rdml <- readRDML(rdmlFile, showProgress = FALSE)
#'
#' meta <- asTable(
#'   rdml,
#'   columns = c("position", "sample", "target", "adp"),
#'   cq = data$cq,
#'   namePattern = "{position}_{target}"
#' )
#' head(meta)
#' @seealso `getFData()`, `setFData()`
#' @export
asTable <- S7::new_generic("asTable", "x")

#' Extract fluorescence data
#'
#' Extracts amplification or melting curves either from one [dataType] object
#' or from a selection of curves in an [rdmlType] object.
#'
#' @param x A [dataType] containing one target-specific result, or an
#'   [rdmlType] containing complete experiments.
#' @param ... Reserved for method-specific extensions.
#'
#' @section Method arguments:
#' \describe{
#'   \item{`dpType`}{Character scalar accepted by both methods: `"adp"` for
#'   amplification data or `"mdp"` for melting data. Defaults to `"adp"`.}
#'   \item{`request`}{For an `rdmlType`, a data frame normally produced by
#'   [asTable()]. It must contain `fdataName`, `expId`, `runId`, `reactId`, and
#'   `target`. Additional columns are retained in long output. If omitted,
#'   `asTable(x)` selects every stored target result.}
#'   \item{`longTable`}{For an `rdmlType`, return one row per measured point
#'   with request metadata repeated (`TRUE`), or a wide table with one
#'   fluorescence column per `fdataName` (`FALSE`, the default).}
#'   \item{`includeMissing`}{In long form, retain requested metadata rows that
#'   have no curve of the selected `dpType`, filling point columns with `NA`?
#'   Defaults to `FALSE`. It has no effect on wide output.}
#' }
#'
#' @section Output columns:
#' For a single `dataType`, amplification output has `cyc`, optional `tmp`, and
#' `fluor`; melting output has `tmp` and `fluor`. A missing curve returns an
#' empty table with the appropriate coordinate and `fluor` columns.
#'
#' For an `rdmlType`, wide amplification output begins with `cyc` and may
#' include `tmp` when temperature is unambiguous for every cycle. Wide melting
#' output begins with `tmp`. Remaining columns are named by `fdataName` in the
#' order requested. Long output joins the request columns to `cyc`/`tmp` and
#' `fluor`.
#'
#' Duplicate `fdataName` values are replaced by unique names constructed from
#' `expId`, `runId`, `reactId`, and `target`, with a warning.
#'
#' @return A [data.table::data.table]. See **Output columns** for the shape
#'   returned by each method and argument combination.
#' @examples
#' rdmlFile <- system.file("extdata", "lc96_bACTXY.rdml", package = "RDML7")
#' rdml <- readRDML(rdmlFile, showProgress = FALSE)
#' meta <- asTable(
#'   rdml,
#'   columns = c(
#'     "expId", "runId", "reactId", "position", "sample", "target", "adp"
#'   )
#' )
#'
#' # All amplification curves as one wide table.
#' amplification <- getFData(rdml, dpType = "adp")
#'
#' # Selected curves with metadata repeated for every cycle.
#' selected <- getFData(
#'   rdml,
#'   request = meta[seq_len(min(2L, nrow(meta))), ],
#'   dpType = "adp",
#'   longTable = TRUE
#' )
#'
#' # Keep metadata even when melting data are absent.
#' melting <- getFData(
#'   rdml,
#'   request = meta,
#'   dpType = "mdp",
#'   longTable = TRUE,
#'   includeMissing = TRUE
#' )
#' @seealso `asTable()`, `setFData()`
#' @export
getFData <- S7::new_generic("getFData", "x")

#' Add or replace fluorescence data
#'
#' Adds wide fluorescence data and its metadata to an [rdmlType]. Missing
#' experiments, runs, reactions, samples, targets, and dyes are created when
#' enough metadata are supplied.
#'
#' @param x An [rdmlType] object to modify.
#' @param ... Reserved for method-specific extensions.
#'
#' @section Arguments for the `rdmlType` method:
#' \describe{
#'   \item{`fdata`}{A matrix, data frame, or [data.table::data.table]. The first
#'   column contains the coordinate (`cyc` for amplification or `tmp` for
#'   melting); every other fluorescence column is matched to a row of
#'   `description` through `fdataName`. For amplification data, an optional
#'   column named `tmp` is stored as per-cycle temperature unless explicitly
#'   described as a fluorescence series.}
#'   \item{`description`}{A metadata table with one row per fluorescence
#'   column. Required columns are `fdataName`, `expId`, `runId`, `reactId`, and
#'   `target`. `sample` is required when creating a new reaction, and
#'   `targetDyeId` is required when creating a new target. Recognized optional
#'   columns include `sampleType`, `targetType`, `cq`, `meltTemp`, and
#'   `quantity`.}
#'   \item{`fdataType`}{Character scalar: `"adp"` for amplification data or
#'   `"mdp"` for melting data. Defaults to `"adp"`.}
#'   \item{`conflict`}{How to handle metadata that disagree with existing RDML
#'   objects: `"error"` (default) stops, `"keep"` retains existing metadata,
#'   and `"replace"` updates it. Fluorescence curves supplied in `fdata` are
#'   written for their selected `fdataType`.}
#' }
#'
#' @details
#' New runs receive a conventional 96-well format, or a 384-well format when
#' numeric reaction identifiers exceed 96. Each fluorescence column must have
#' exactly one matching description row; undescribed columns are skipped with
#' a warning and duplicate descriptions are an error.
#'
#' RDML objects use value semantics: this function does not modify `x` in
#' place. Assign the returned value.
#'
#' @return The updated [rdmlType] object.
#' @examples
#' rdmlFile <- system.file("extdata", "lc96_bACTXY.rdml", package = "RDML7")
#' source <- readRDML(rdmlFile, showProgress = FALSE)
#' metadata <- asTable(source)
#' metadata <- metadata[seq_len(min(2L, nrow(metadata))), ]
#' curves <- getFData(
#'   source,
#'   request = metadata,
#'   dpType = "adp"
#' )
#'
#' copied <- setFData(
#'   rdmlType(),
#'   fdata = curves,
#'   description = metadata,
#'   fdataType = "adp"
#' )
#' asTable(copied)
#' @seealso `buildRDMLFromFData()`, `asTable()`, `getFData()`
#' @export
setFData <- S7::new_generic("setFData", "x")

#' Represent RDML structure as a dendrogram
#'
#' Groups stored data hierarchically by experiment, run, target, sample type,
#' and curve type (`adp` or `mdp`). Leaf labels contain the number of matching
#' reactions.
#'
#' @param x An [rdmlType] object.
#' @param ... Reserved for method-specific extensions.
#'
#' @section Arguments for the `rdmlType` method:
#' \describe{
#'   \item{`plotDendrogram`}{Logical. Plot the dendrogram immediately? Defaults
#'   to `TRUE`. Use `FALSE` to build the object without opening a graphics
#'   device.}
#' }
#'
#' @return A base R `dendrogram` object. An empty RDML object returns an empty
#'   dendrogram; plotting it produces a warning.
#' @examples
#' rdmlFile <- system.file(
#'   "extdata", "BioRad_qPCR_melt.rdml", package = "RDML7"
#' )
#' rdml <- readRDML(rdmlFile, showProgress = FALSE)
#' tree <- asDendrogram(rdml, plotDendrogram = FALSE)
#' tree
#' @seealso `summary()`
#' @export
asDendrogram <- S7::new_generic("asDendrogram", "x")

#' Serialize an RDML object to XML
#'
#' Serializes standard RDML properties. Package-only extension fields are kept
#' in memory but are not emitted as non-standard XML elements.
#'
#' @param x An [rdmlType] object.
#' @param ... Arguments passed to the method.
#'
#' @section Arguments for the `rdmlType` method:
#' \describe{
#'   \item{`fileName`}{Optional output path. If omitted, return the complete
#'   XML document as a character scalar. A `.xml` path writes plain XML; other
#'   extensions, normally `.rdml`, write a ZIP-based RDML archive.}
#'   \item{`loss`}{Policy for package data that standard RDML XML cannot
#'   represent: `"warn"` (default), `"error"`, or `"allow"`.}
#' }
#'
#' @return Without `fileName`, a character scalar containing XML. With
#'   `fileName`, the serialized XML is returned invisibly after the file is
#'   written.
#' @examples
#' rdmlFile <- system.file("extdata", "lc96_bACTXY.rdml", package = "RDML7")
#' rdml <- readRDML(rdmlFile, showProgress = FALSE)
#'
#' xml <- asXML(rdml, loss = "allow")
#' substr(xml, 1L, 80L)
#'
#' output <- tempfile(fileext = ".rdml")
#' invisible(asXML(rdml, fileName = output, loss = "allow"))
#' file.exists(output)
#' unlink(output)
#' @seealso `writeRDML()`
#' @export
asXML <- S7::new_generic("asXML", "x")
