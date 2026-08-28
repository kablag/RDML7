# RDML schema type reference ------------------------------------------------

#' Metadata schema types
#'
#' S7 classes representing reusable metadata elements in an RDML document.
#'
#' \describe{
#'   \item{`experimenterType`}{Experimenter/contact metadata: `id`,
#'   `firstName`, `lastName`, optional `email`, `labName`, and `labAddress`.}
#'   \item{`documentationType`}{Reusable documentation identified by `id` and
#'   optional free text.}
#'   \item{`dyeChemistryType`}{Enumeration describing fluorescence chemistry.}
#'   \item{`dyeType`}{Dye metadata: `id`, optional `description`, and optional
#'   `dyeChemistry`.}
#'   \item{`xRefType`}{External cross-reference with optional `name` and `id`.}
#'   \item{`annotationType`}{Annotation `property` / `value` pair.}
#'   \item{`rdmlIdType`}{Top-level publisher identifier containing
#'   `publisher`, `serialNumber`, and optional `MD5Hash`.}
#' }
#'
#' @seealso `idType`, `idReferenceType`, `rdmlType`
#' @name rdml-metadata-types
#' @aliases experimenterType documentationType dyeChemistryType dyeType xRefType annotationType rdmlIdType
NULL


#' Sample and target schema types
#'
#' S7 classes used to describe samples, quantities, targets, oligonucleotides,
#' assays, and cDNA synthesis metadata.
#'
#' @section Enumerations:
#' \describe{
#'   \item{`sampleTypeType`}{`unkn`, `ntc`, `nac`, `std`, `ntp`, `nrt`,
#'   `pos`, or `opt`.}
#'   \item{`quantityUnitType`}{`cop`, `fold`, `dil`, `ng`, `nMol`, or
#'   `other`.}
#'   \item{`primingMethodType`}{`oligo-dt`, `random`, `target-specific`,
#'   `oligo-dt and random`, or `other`.}
#'   \item{`nucleotideType`}{`DNA`, `genomic DNA`, `cDNA`, or `RNA`.}
#'   \item{`targetTypeType`}{`ref` or `toi`.}
#' }
#'
#' @section Classes:
#' \describe{
#'   \item{`sampleTargetType`}{Associates `targetId` with a sample type.}
#'   \item{`quantityType`}{Target-specific quantity: `targetId`, `value`,
#'   and `unit`.}
#'   \item{`cdnaSynthesisMethodType`}{Enzyme, priming method, DNase treatment,
#'   and optional thermal-program reference.}
#'   \item{`templateQuantityType`}{Template concentration and nucleotide type.}
#'   \item{`sampleType`}{Sample metadata, target-specific type/quantity,
#'   annotations, documentation, and cDNA synthesis information.}
#'   \item{`oligoType`}{Oligonucleotide sequence and optional terminal tags.}
#'   \item{`sequencesType`}{Primers, probes, and optional amplicon sequence.}
#'   \item{`commercialAssayType`}{Commercial assay company and order number.}
#'   \item{`targetType`}{Target type, dye reference, amplification efficiency,
#'   optional sequences, and commercial assay metadata.}
#' }
#'
#' @name rdml-sample-target-types
#' @aliases sampleTypeType sampleTargetType quantityUnitType quantityType primingMethodType cdnaSynthesisMethodType nucleotideType templateQuantityType sampleType oligoType sequencesType commercialAssayType targetTypeType targetType
NULL


#' Thermal-cycling schema types
#'
#' S7 types used to represent PCR and melting thermal programs.
#'
#' `measureType` accepts `real time` or `meltcurve`.
#'
#' \describe{
#'   \item{`temperatureBaseType`}{Common duration, measurement, ramp, and
#'   change fields.}
#'   \item{`temperatureType`}{Fixed-temperature step.}
#'   \item{`gradientType`}{Gradient step with high/low temperatures.}
#'   \item{`loopType`}{Loop control with `goto` and `repeat`.}
#'   \item{`pauseType`}{Pause at a temperature.}
#'   \item{`lidOpenType`}{Marker step indicating an open lid.}
#'   \item{`stepType`}{Numbered step containing one thermal action.}
#'   \item{`thermalCyclingConditionsType`}{Named thermal program containing
#'   `stepType` objects.}
#' }
#'
#' @name rdml-thermal-types
#' @aliases measureType temperatureBaseType temperatureType gradientType loopType pauseType lidOpenType stepType thermalCyclingConditionsType
NULL


#' Experimental data and run schema types
#'
#' S7 classes for fluorescence curves, reactions, runs, experiments, plate
#' layouts, digital-PCR partitions, and the top-level RDML document.
#'
#' @section Curve and result classes:
#' \describe{
#'   \item{`dpAmpCurveType`}{Amplification `fpoints`: `cyc`, optional `tmp`,
#'   and `fluor`.}
#'   \item{`dpMeltingCurveType`}{Melting `fpoints`: `tmp` and `fluor`.}
#'   \item{`dataType`}{Target-specific results and curves. `meltTemps` is a
#'   package extension used to retain multiple RDES Tm values; standard RDML
#'   XML still contains the single `meltTemp` element.}
#' }
#'
#' @section Hierarchy and layout:
#' \describe{
#'   \item{`partitionDataType`, `partitionsType`}{Digital-PCR partition data.}
#'   \item{`reactType`}{Reaction with sample reference and target-keyed data.}
#'   \item{`dataCollectionSoftwareType`}{Software name/version.}
#'   \item{`labelFormatType`}{Plate label enumeration.}
#'   \item{`pcrFormatType`}{Plate dimensions and label formats.}
#'   \item{`cqDetectionMethodType`}{Cq-detection method enumeration.}
#'   \item{`runType`}{Instrument run and reactions.}
#'   \item{`experimentType`}{Experiment and runs.}
#'   \item{`rdmlType`}{Top-level RDML document.}
#' }
#'
#' @seealso `rdmlRead`, `asTable`, `getFData`, `rdmlValidate`
#' @name rdml-experimental-types
#' @aliases dpAmpCurveType dpMeltingCurveType dataType partitionDataType partitionsType reactType dataCollectionSoftwareType labelFormatType pcrFormatType cqDetectionMethodType runType experimentType rdmlType
NULL
