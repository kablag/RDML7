# experimenterType --------------------------------------------------------


#' experimenterType S7 class
#'
#' Contact details of an experimenter associated with an RDML document or thermal cycling conditions. Inherits from `rdmlBaseType`.
#'
#' @format An S7 class.
#'
#' @section Properties:
#' \describe{
#'   \item{`id`}{`idType`. Unique experimenter identifier.}
#'   \item{`firstName`}{`character(1)`. First name.}
#'   \item{`lastName`}{`character(1)`. Last name.}
#'   \item{`email`}{`character(1)` or `NA`. E-mail address.}
#'   \item{`labName`}{`character(1)` or `NA`. Laboratory or organization name.}
#'   \item{`labAddress`}{`character(1)` or `NA`. Laboratory postal address.}
#' }
#'
#' @seealso `rdmlType`, `thermalCyclingConditionsType`
#' @export

experimenterType <- S7::new_class(
  "experimenterType",
  parent = rdmlBaseType,
  properties = list(
    id = classId,
    firstName = classCharacterNonemptySingle,
    lastName = classCharacterNonemptySingle,
    email = classCharacterNaNonemptySingle,
    labName = classCharacterNaNonemptySingle,
    labAddress = classCharacterNaNonemptySingle
  )
)


# documentationType -------------------------------------------------------

#' documentationType S7 class
#'
#' Reusable documentation text. Use this type when the same description or note is referenced by several samples, targets, runs, or experiments. Inherits from `rdmlBaseType`.
#'
#' @format An S7 class.
#'
#' @section Properties:
#' \describe{
#'   \item{`id`}{`idType`. Unique documentation identifier.}
#'   \item{`text`}{`character(1)` or `NA`. Documentation text.}
#' }
#'
#' @seealso `rdmlType`, `idReferenceType`
#' @export

documentationType <- S7::new_class(
  "documentationType",
  parent = rdmlBaseType,
  properties = list(
    id = classId,
    text = classCharacterNaNonemptySingle
  )
)




# dyeChemistryType --------------------------------------------------------


#' dyeChemistryType S7 enumeration
#'
#' Fluorescence chemistry used by a dye or detection system.
#'
#' @format An S7 class.
#'
#' @section Allowed values:
#' \describe{
#'   \item{`non-saturating DNA binding dye`}{Allowed value.}
#'   \item{`saturating DNA binding dye`}{Allowed value.}
#'   \item{`hybridization probe`}{Allowed value.}
#'   \item{`hydrolysis probe`}{Allowed value.}
#'   \item{`labelled forward primer`}{Allowed value.}
#'   \item{`labelled reverse primer`}{Allowed value.}
#'   \item{`DNA-zyme probe`}{Allowed value.}
#' }
#'
#' @seealso `dyeType`
#' @export

dyeChemistryType <- 
  .newEnumClass(
    "dyeChemistryType",
    c("non-saturating DNA binding dye", 
      "saturating DNA binding dye",
      "hybridization probe",
      "hydrolysis probe", 
      "labelled forward primer", 
      "labelled reverse primer",
      "DNA-zyme probe")
  )


# dyeType -----------------------------------------------------------------

#' dyeType S7 class
#'
#' Description of a fluorescent dye or reporter used to detect a target. Inherits from `rdmlBaseType`.
#'
#' @format An S7 class.
#'
#' @section Properties:
#' \describe{
#'   \item{`id`}{`idType`. Unique dye identifier.}
#'   \item{`description`}{`character(1)` or `NA`. Human-readable dye description.}
#'   \item{`dyeChemistry`}{`dyeChemistryType` or `NA`. Detection chemistry.}
#' }
#'
#' @seealso `dyeChemistryType`, `targetType`
#' @export

dyeType <- S7::new_class(
  "dyeType",
  parent = rdmlBaseType,
  properties = list(
    id = classId,
    description = classCharacterNaNonemptySingle,
    dyeChemistry = S7::new_property(
      S7::class_any,
      validator = function(value) {
        if (.isSingleNa(value)) {
          return(NULL)
        }

        if (S7::S7_inherits(value, dyeChemistryType)) {
          return(NULL)
        }

        "must be NA or a single dyeChemistryType"
      },
      default = NA
    )
  )
)


# xRefType ----------------------------------------------------------------


#' xRefType S7 class
#'
#' External database or ontology cross-reference. Inherits from `rdmlBaseType`.
#'
#' @format An S7 class.
#'
#' @section Properties:
#' \describe{
#'   \item{`name`}{`character(1)` or `NA`. Name of the external reference system.}
#'   \item{`id`}{`character(1)` or `NA`. Identifier in that reference system.}
#' }
#'
#' @seealso `sampleType`, `targetType`
#' @export

xRefType <- S7::new_class(
  "xRefType",
  parent = rdmlBaseType,
  properties = list(
    name = classCharacterNaNonemptySingle,
    id = classCharacterNaNonemptySingle
  )
)


# annotationType ----------------------------------------------------------


#' annotationType S7 class
#'
#' Free-form property/value annotation attached to a sample. Inherits from `rdmlBaseType`.
#'
#' @format An S7 class.
#'
#' @section Properties:
#' \describe{
#'   \item{`property`}{`character(1)` or `NA`. Annotation property name.}
#'   \item{`value`}{`character(1)` or `NA`. Annotation value.}
#' }
#'
#' @seealso `sampleType`
#' @export

annotationType <- S7::new_class(
  "annotationType",
  parent = rdmlBaseType,
  properties = list(
    property = classCharacterNaNonemptySingle,
    value = classCharacterNaNonemptySingle
  )
)

# sampleTypeType ----------------------------------------------------------

#' sampleTypeType S7 enumeration
#'
#' Role of a sample for a particular target.
#'
#' @format An S7 class.
#'
#' @section Allowed values:
#' \describe{
#'   \item{`unkn`}{unknown sample.}
#'   \item{`ntc`}{no-template control.}
#'   \item{`nac`}{no-amplification control.}
#'   \item{`std`}{standard.}
#'   \item{`ntp`}{no-target-positive control.}
#'   \item{`nrt`}{no-reverse-transcription control.}
#'   \item{`pos`}{positive control.}
#'   \item{`opt`}{optical calibrator.}
#' }
#'
#' @seealso `sampleTargetType`, `sampleType`
#' @export

sampleTypeType <- .newEnumClass(
  "sampleTypeType",
  c(
    "unkn", "ntc", "nac",
    "std", "ntp", "nrt",
    "pos", "opt"
  )
)

#' sampleTargetType S7 class
#'
#' Sample role, optionally restricted to a target. If `targetId` is `NA`, the role applies to all targets unless a target-specific role overrides it. Inherits from `rdmlBaseType`.
#'
#' @format An S7 class.
#'
#' @section Properties:
#' \describe{
#'   \item{`targetId`}{`idReferenceType` or `NA`. Optional target reference; `NA` means that the sample role is target-independent.}
#'   \item{`sampleType`}{`sampleTypeType`. Role of the sample for that target.}
#' }
#'
#' @seealso `sampleTypeType`, `sampleType`, `targetType`
#' @export

sampleTargetType <- S7::new_class(
  "sampleTargetType",
  parent = rdmlBaseType,
  properties = list(
    targetId = .testClassNa("idReferenceType"),
    sampleType = S7::new_property(
      sampleTypeType,
      validator = function(value) {
        if (length(value) != 1L) {
          "must be a single sampleTypeType"
        }
      }
    )
  )
)

#' quantityUnitType S7 enumeration
#'
#' Unit used for a target-specific sample quantity.
#'
#' @format An S7 class.
#'
#' @section Allowed values:
#' \describe{
#'   \item{`cop`}{copies.}
#'   \item{`fold`}{fold value.}
#'   \item{`dil`}{dilution.}
#'   \item{`ng`}{nanograms.}
#'   \item{`nMol`}{nanomoles.}
#'   \item{`other`}{another unit.}
#' }
#'
#' @seealso `quantityType`
#' @export

quantityUnitType <- .newEnumClass(
  "quantityUnitType",
  c("cop", "fold", "dil", "ng", "nMol", "other")
)

classQuantityUnitTypeNonemptySingle <- S7::new_property(
  quantityUnitType,
  validator = function(value) {
    if (length(value) != 1L) {
      "must be a single quantityUnitType"
    }
  }
)

#' quantityType S7 class
#'
#' Target-specific quantity assigned to a sample. Inherits from `rdmlBaseType`.
#'
#' @format An S7 class.
#'
#' @section Properties:
#' \describe{
#'   \item{`targetId`}{`idReferenceType`. Reference to the target.}
#'   \item{`value`}{`numeric(1)`. Quantity value.}
#'   \item{`unit`}{`quantityUnitType`. Unit of the quantity.}
#' }
#'
#' @seealso `sampleType`, `quantityUnitType`
#' @export

quantityType <- S7::new_class(
  "quantityType",
  parent = rdmlBaseType,
  properties = list(
    targetId = .testClass("idReferenceType"),
    value = classNumberSingle,
    unit = classQuantityUnitTypeNonemptySingle
  )
)

#' primingMethodType S7 enumeration
#'
#' Priming strategy used for cDNA synthesis.
#'
#' @format An S7 class.
#'
#' @section Allowed values:
#' \describe{
#'   \item{`oligo-dt`}{Allowed value.}
#'   \item{`random`}{Allowed value.}
#'   \item{`target-specific`}{Allowed value.}
#'   \item{`oligo-dt and random`}{Allowed value.}
#'   \item{`other`}{Allowed value.}
#' }
#'
#' @seealso `cdnaSynthesisMethodType`
#' @export

primingMethodType <- .newEnumClass(
  "primingMethodType",
  c(
    "oligo-dt",
    "random",
    "target-specific",
    "oligo-dt and random",
    "other"
  )
)

#' cdnaSynthesisMethodType S7 class
#'
#' Description of reverse-transcription/cDNA-synthesis conditions. Inherits from `rdmlBaseType`.
#'
#' @format An S7 class.
#'
#' @section Properties:
#' \describe{
#'   \item{`enzyme`}{`character(1)` or `NA`. Reverse-transcriptase/enzyme description.}
#'   \item{`primingMethod`}{`primingMethodType` or `NA`. Priming strategy.}
#'   \item{`dnaseTreatment`}{`logical(1)` or `NA`. Whether DNase treatment was performed.}
#'   \item{`thermalCyclingConditions`}{`idReferenceType` or `NA`. Reference to the thermal program used for cDNA synthesis.}
#' }
#'
#' @seealso `primingMethodType`, `thermalCyclingConditionsType`, `sampleType`
#' @export

cdnaSynthesisMethodType <- S7::new_class(
  "cdnaSynthesisMethodType",
  parent = rdmlBaseType,
  properties = list(
    enzyme = classCharacterNaNonemptySingle,
    primingMethod = .testClassNa("primingMethodType"),
    dnaseTreatment = classFlagNa,
    thermalCyclingConditions = .testClassNa("idReferenceType")
  )
)

#' nucleotideType S7 enumeration
#'
#' Nucleic-acid type of the template material.
#'
#' @format An S7 class.
#'
#' @section Allowed values:
#' \describe{
#'   \item{`DNA`}{Allowed value.}
#'   \item{`genomic DNA`}{Allowed value.}
#'   \item{`cDNA`}{Allowed value.}
#'   \item{`RNA`}{Allowed value.}
#' }
#'
#' @seealso `templateQuantityType`
#' @export

nucleotideType <- .newEnumClass(
  "nucleotideType",
  c("DNA", "genomic DNA", "cDNA", "RNA")
)

#' templateQuantityType S7 class
#'
#' Total template concentration together with the template nucleotide type. Inherits from `rdmlBaseType`.
#'
#' @format An S7 class.
#'
#' @section Properties:
#' \describe{
#'   \item{`conc`}{`numeric(1)`. Template concentration.}
#'   \item{`nucleotide`}{`nucleotideType`. Nucleic-acid type.}
#' }
#'
#' @seealso `nucleotideType`, `sampleType`
#' @export

templateQuantityType <- S7::new_class(
  "templateQuantityType",
  parent = rdmlBaseType,
  properties = list(
    conc = classNumberSingle,
    nucleotide = S7::new_property(
      nucleotideType,
      validator = function(value) {
        if (length(value) != 1L) {
          "must be a single nucleotideType"
        }
      }
    )
  )
)

#' sampleType S7 class
#'
#' A qPCR sample/template solution. Dilutions with different concentrations are represented as different samples; technical replicates normally reference the same sample id, while biological replicates normally use different sample ids. Inherits from `rdmlBaseType`.
#'
#' @format An S7 class.
#'
#' @section Properties:
#' \describe{
#'   \item{`id`}{`idType`. Unique sample identifier.}
#'   \item{`description`}{`character(1)` or `NA`. Sample description.}
#'   \item{`documentation`}{List of `idReferenceType` objects or `NA`. References to reusable documentation.}
#'   \item{`xRef`}{List of `xRefType` objects or `NA`. External references.}
#'   \item{`annotation`}{List of `annotationType` objects or `NA`. Sample annotations.}
#'   \item{`type`}{List of `sampleTargetType` objects or `NA`. Roles may be target-independent or target-specific.}
#'   \item{`interRunCalibrator`}{`logical(1)` or `NA`. Marks an inter-run calibrator.}
#'   \item{`quantity`}{Target-keyed list of `quantityType` objects or `NA`. Target-specific quantities.}
#'   \item{`calibratorSample`}{`logical(1)` or `NA`. Marks a calibrator sample.}
#'   \item{`cdnaSynthesisMethod`}{`cdnaSynthesisMethodType` or `NA`. cDNA synthesis metadata.}
#'   \item{`templateQuantity`}{`templateQuantityType` or `NA`. Template concentration/type.}
#' }
#'
#' @seealso `sampleTargetType`, `quantityType`, `reactType`, `rdmlType`
#' @export

sampleType <- S7::new_class(
  "sampleType",
  parent = rdmlBaseType,
  properties = list(
    id = classId,
    description = classCharacterNaNonemptySingle,
    documentation = .testClassNaList("idReferenceType"),
    xRef = .testClassNaList("xRefType"),
    annotation = .testClassNaList("annotationType"),
    type = .testClassNaList("sampleTargetType"),
    interRunCalibrator = classFlagNa,
    quantity = .testClassNaKeyedList(
      "quantityType",
      key = "targetId"
    ),
    calibratorSample = classFlagNa,
    cdnaSynthesisMethod = .testClassNa("cdnaSynthesisMethodType"),
    templateQuantity = .testClassNa("templateQuantityType")
  )
)

#' oligoType S7 class
#'
#' Oligonucleotide sequence used for a primer, probe, or amplicon definition. Inherits from `rdmlBaseType`.
#'
#' @format An S7 class.
#'
#' @section Properties:
#' \describe{
#'   \item{`threePrimeTag`}{`character(1)` or `NA`. Optional 3-prime modification/tag.}
#'   \item{`fivePrimeTag`}{`character(1)` or `NA`. Optional 5-prime modification/tag.}
#'   \item{`sequence`}{`character(1)`. Nucleotide sequence.}
#' }
#'
#' @seealso `sequencesType`
#' @export

oligoType <- S7::new_class(
  "oligoType",
  parent = rdmlBaseType,
  properties = list(
    threePrimeTag = classCharacterNaNonemptySingle,
    fivePrimeTag = classCharacterNaNonemptySingle,
    sequence = classCharacterNonemptySingle
  )
)

#' sequencesType S7 class
#'
#' Primer, probe, and amplicon sequences associated with a target. Inherits from `rdmlBaseType`.
#'
#' @format An S7 class.
#'
#' @section Properties:
#' \describe{
#'   \item{`forwardPrimer`}{`oligoType` or `NA`. Forward primer.}
#'   \item{`reversePrimer`}{`oligoType` or `NA`. Reverse primer.}
#'   \item{`probe1`}{`oligoType` or `NA`. First probe.}
#'   \item{`probe2`}{`oligoType` or `NA`. Second probe.}
#'   \item{`amplicon`}{`oligoType` or `NA`. Amplicon sequence.}
#' }
#'
#' @seealso `oligoType`, `targetType`
#' @export

sequencesType <- S7::new_class(
  "sequencesType",
  parent = rdmlBaseType,
  properties = list(
    forwardPrimer = .testClassNa("oligoType"), 
    reversePrimer = .testClassNa("oligoType"),
    probe1 = .testClassNa("oligoType"),
    probe2 = .testClassNa("oligoType"),
    amplicon = .testClassNa("oligoType")
  )
)


#' commercialAssayType S7 class
#'
#' Commercial assay identification. Inherits from `rdmlBaseType`.
#'
#' @format An S7 class.
#'
#' @section Properties:
#' \describe{
#'   \item{`company`}{`character(1)`. Manufacturer/company.}
#'   \item{`orderNumber`}{`character(1)`. Catalogue or order number.}
#' }
#'
#' @seealso `targetType`
#' @export

commercialAssayType <- S7::new_class(
  "commercialAssayType",
  parent = rdmlBaseType,
  properties = list(
    company = classCharacterNonemptySingle,
    orderNumber = classCharacterNonemptySingle
  )
)

#' targetTypeType S7 enumeration
#'
#' Functional role of a target.
#'
#' @format An S7 class.
#'
#' @section Allowed values:
#' \describe{
#'   \item{`ref`}{reference target.}
#'   \item{`toi`}{target of interest.}
#' }
#'
#' @seealso `targetType`
#' @export

targetTypeType <-  .newEnumClass(
  "targetTypeType",
  c("ref", "toi")
)

#' targetType S7 class
#'
#' Description of a qPCR target, including dye, efficiency, sequence, and assay metadata. Inherits from `rdmlBaseType`.
#'
#' @format An S7 class.
#'
#' @section Properties:
#' \describe{
#'   \item{`id`}{`idType`. Unique target identifier.}
#'   \item{`description`}{`character(1)` or `NA`. Target description.}
#'   \item{`documentation`}{List of `idReferenceType` objects or `NA`. Documentation references.}
#'   \item{`xRef`}{List of `xRefType` objects or `NA`. External references.}
#'   \item{`type`}{`targetTypeType`. Reference target or target of interest.}
#'   \item{`amplificationEfficiencyMethod`}{`character(1)` or `NA`. Method used to determine amplification efficiency.}
#'   \item{`amplificationEfficiency`}{`numeric(1)` or `NA`. Amplification efficiency.}
#'   \item{`amplificationEfficiencySE`}{`numeric(1)` or `NA`. Standard error of amplification efficiency.}
#'   \item{`meltingTemperature`}{`numeric(1)` or `NA`. Expected target melting temperature.}
#'   \item{`detectionLimit`}{`numeric(1)` or `NA`. Detection limit.}
#'   \item{`dyeId`}{`idReferenceType`. Reference to the detection dye.}
#'   \item{`sequences`}{`sequencesType` or `NA`. Primers/probes/amplicon.}
#'   \item{`commercialAssay`}{`commercialAssayType` or `NA`. Commercial assay metadata.}
#' }
#'
#' @seealso `targetTypeType`, `dyeType`, `dataType`, `rdmlType`
#' @export

targetType <- S7::new_class(
  "targetType",
  parent = rdmlBaseType,
  properties = list(
    id = classId,
    description = classCharacterNaNonemptySingle,
    documentation = .testClassNaList("idReferenceType"),
    xRef = .testClassNaList("xRefType"),
    type = .testClass("targetTypeType"),
    amplificationEfficiencyMethod = classCharacterNaNonemptySingle,
    amplificationEfficiency = classNumberNaSingle,
    amplificationEfficiencySE = classNumberNaSingle,
    meltingTemperature = classNumberNaSingle,
    detectionLimit = classNumberNaSingle,
    dyeId = .testClass("idReferenceType"),
    sequences = .testClassNa("sequencesType"),
    commercialAssay = .testClassNa("commercialAssayType")
  )
)


# thermalCyclingConditions ------------------------------------------------

#' measureType S7 enumeration
#'
#' Fluorescence measurement mode used in a thermal-cycling step.
#'
#' @format An S7 class.
#'
#' @section Allowed values:
#' \describe{
#'   \item{`real time`}{Allowed value.}
#'   \item{`meltcurve`}{Allowed value.}
#' }
#'
#' @seealso `temperatureBaseType`
#' @export

measureType <-  .newEnumClass(
  "measureType",
  c("real time", "meltcurve")
)


#' temperatureBaseType S7 class
#'
#' Base class for thermal steps with duration, measurement, ramp, and per-cycle changes. Inherits from `rdmlBaseType`.
#'
#' @format An S7 class.
#'
#' @section Properties:
#' \describe{
#'   \item{`duration`}{Positive integer. Step duration.}
#'   \item{`temperatureChange`}{`numeric(1)` or `NA`. Temperature increment/decrement per cycle.}
#'   \item{`durationChange`}{`integer(1)` or `NA`. Duration increment/decrement per cycle.}
#'   \item{`measure`}{`measureType` or `NA`. Fluorescence measurement mode.}
#'   \item{`ramp`}{`numeric(1)` or `NA`. Ramp rate.}
#' }
#'
#' @seealso `temperatureType`, `gradientType`, `measureType`
#' @export

temperatureBaseType <- S7::new_class(
  "temperatureBaseType",
  parent = rdmlBaseType,
  properties = list(
    duration = classPositiveIntegerSingle,
    temperatureChange = classNumberNaSingle,
    durationChange = classIntegerNaSingle,
    measure = .testClassNa("measureType"),
    ramp = classNumberNaSingle
  )
)

#' temperatureType S7 class
#'
#' Fixed-temperature thermal-cycling step. Inherits from `temperatureBaseType`.
#'
#' @format An S7 class.
#'
#' @section Properties:
#' \describe{
#'   \item{`temperature`}{`numeric(1)`. Step temperature.}
#' }
#'
#' @seealso `temperatureBaseType`, `stepType`
#' @export

temperatureType <- S7::new_class(
  "temperatureType",
  parent = temperatureBaseType,
  properties = list(
    temperature = classNumberSingle
  )
)

#' gradientType S7 class
#'
#' Temperature-gradient step. Inherits from `temperatureBaseType`.
#'
#' @format An S7 class.
#'
#' @section Properties:
#' \describe{
#'   \item{`highTemperature`}{`numeric(1)`. High end of the gradient.}
#'   \item{`lowTemperature`}{`numeric(1)`. Low end of the gradient.}
#' }
#'
#' @seealso `temperatureBaseType`, `stepType`
#' @export

gradientType <- S7::new_class(
  "gradientType",
  parent = temperatureBaseType,
  properties = list(
    highTemperature = classNumberSingle,
    lowTemperature = classNumberSingle
  )
)

#' loopType S7 class
#'
#' Loop instruction in a thermal-cycling program. Inherits from `rdmlBaseType`.
#'
#' @format An S7 class.
#'
#' @section Properties:
#' \describe{
#'   \item{`goto`}{Positive integer. Number of the step to return to.}
#'   \item{`repeat`}{Positive integer. Number of loop repetitions.}
#' }
#'
#' @seealso `stepType`, `thermalCyclingConditionsType`
#' @export

loopType <- S7::new_class(
  "loopType",
  parent = rdmlBaseType,
  properties = list(
    goto = classPositiveIntegerSingle,
    "repeat" = classPositiveIntegerSingle
  )
)

#' pauseType S7 class
#'
#' Pause instruction at a specified temperature. Inherits from `rdmlBaseType`.
#'
#' @format An S7 class.
#'
#' @section Properties:
#' \describe{
#'   \item{`temperature`}{`numeric(1)`. Pause temperature.}
#' }
#'
#' @seealso `stepType`
#' @export

pauseType <- S7::new_class(
  "pauseType",
  parent = rdmlBaseType,
  properties = list(
    temperature = classNumberSingle
  )
)

#' lidOpenType S7 class
#'
#' Marker step indicating that the instrument lid is opened. This class has no additional properties. Inherits from `rdmlBaseType`.
#'
#' @format An S7 class.
#'
#' This class does not define additional properties.
#'
#' @seealso `stepType`
#' @export

lidOpenType <- S7::new_class(
  "lidOpenType",
  parent = rdmlBaseType,
  properties = list(
  )
)

#' stepType S7 class
#'
#' One numbered step of a thermal-cycling program. Exactly one of the action properties normally describes the operation performed by the step. Inherits from `rdmlBaseType`.
#'
#' @format An S7 class.
#'
#' @section Properties:
#' \describe{
#'   \item{`nr`}{Positive integer. Step number.}
#'   \item{`description`}{`character(1)` or `NA`. Step description.}
#'   \item{`temperature`}{`temperatureType` or `NA`. Fixed-temperature action.}
#'   \item{`gradient`}{`gradientType` or `NA`. Gradient action.}
#'   \item{`loop`}{`loopType` or `NA`. Loop action.}
#'   \item{`pause`}{`pauseType` or `NA`. Pause action.}
#'   \item{`lidOpen`}{`lidOpenType` or `NA`. Lid-open action.}
#' }
#'
#' @seealso `thermalCyclingConditionsType`, `temperatureType`, `gradientType`, `loopType`
#' @export

stepType <- S7::new_class(
  "stepType",
  parent = rdmlBaseType,
  properties = list(
    nr = classPositiveIntegerSingle,
    description = classCharacterNaNonemptySingle,
    temperature = .testClassNa("temperatureType"),
    gradient = .testClassNa("gradientType"),
    loop = .testClassNa("loopType"),
    pause = .testClassNa("pauseType"),
    lidOpen = .testClassNa("lidOpenType")
  )
)

#' thermalCyclingConditionsType S7 class
#'
#' A reusable thermal-cycling program referenced by runs or cDNA-synthesis metadata. Inherits from `rdmlBaseType`.
#'
#' @format An S7 class.
#'
#' @section Properties:
#' \describe{
#'   \item{`id`}{`idType`. Unique program identifier.}
#'   \item{`description`}{`character(1)` or `NA`. Program description.}
#'   \item{`documentation`}{List of `idReferenceType` objects or `NA`. Documentation references.}
#'   \item{`lidTemperature`}{`numeric(1)` or `NA`. Heated-lid temperature.}
#'   \item{`experimenter`}{List of `idReferenceType` objects or `NA`. Experimenter references.}
#'   \item{`step`}{List of `stepType` objects. Ordered program steps.}
#' }
#'
#' @seealso `stepType`, `runType`, `cdnaSynthesisMethodType`
#' @export

thermalCyclingConditionsType <- S7::new_class(
  "thermalCyclingConditionsType",
  parent = rdmlBaseType,
  properties = list(
    id = classId,
    description = classCharacterNaNonemptySingle,
    documentation = .testClassNaList("idReferenceType"),
    lidTemperature = classNumberNaSingle,
    experimenter = .testClassNaList("idReferenceType"),
    step = .testClassList("stepType")
  )
)


# experiment --------------------------------------------------------------

#' dpAmpCurveType S7 class
#'
#' Amplification fluorescence curve. Inherits from `rdmlBaseType`.
#'
#' @format An S7 class.
#'
#' @section Properties:
#' \describe{
#'   \item{`fpoints`}{`data.table` containing `cyc` and `fluor`, with optional per-cycle `tmp`.}
#' }
#'
#' @seealso `dataType`, `getFData`, `setFData`
#' @export

dpAmpCurveType <- S7::new_class(
  "dpAmpCurveType",
  parent = rdmlBaseType,
  validator = function(self) {
    if (!checkmate::testSetEqual(colnames(self@fpoints), c("cyc", "fluor")) &&
        !checkmate::testSetEqual(colnames(self@fpoints), c("cyc", "tmp", "fluor"))) {
      "@fpoints must have columns: cyc, tmp (optional), fluor"
    }
  },
  properties = list(
    fpoints = classDataTable
  )
)

#' dpMeltingCurveType S7 class
#'
#' Melting fluorescence curve. Inherits from `rdmlBaseType`.
#'
#' @format An S7 class.
#'
#' @section Properties:
#' \describe{
#'   \item{`fpoints`}{`data.table` containing `tmp` and `fluor`.}
#' }
#'
#' @seealso `dataType`, `getFData`, `setFData`
#' @export

dpMeltingCurveType <- S7::new_class(
  "dpMeltingCurveType",
  parent = rdmlBaseType,
  validator = function(self) {
    if (!identical(
      colnames(self@fpoints),
      c("tmp", "fluor")
    )) {
      "@fpoints must have columns: tmp, fluor"
    }
  },
  properties = list(
    fpoints = classDataTable
  )
)

# dataType ----------------------------------------------------------------


#' dataType S7 class
#'
#' Target-specific qPCR result data for one reaction. It stores calculated results and optional amplification and melting fluorescence curves. Inherits from `rdmlBaseType`.
#'
#' @format An S7 class.
#'
#' @section Properties:
#' \describe{
#'   \item{`targetId`}{`idReferenceType`. Reference to the target.}
#'   \item{`cq`}{`numeric(1)` or `NA`. Quantification cycle (Cq).}
#'   \item{`N0`}{`numeric(1)` or `NA`. Estimated initial template quantity.}
#'   \item{`ampEffMet`}{`character(1)` or `NA`. Amplification-efficiency method.}
#'   \item{`ampEff`}{`numeric(1)` or `NA`. Amplification efficiency.}
#'   \item{`ampEffSE`}{`numeric(1)` or `NA`. Standard error of amplification efficiency.}
#'   \item{`corrF`}{`numeric(1)` or `NA`. Fluorescence correction factor.}
#'   \item{`corrP`}{`numeric(1)` or `NA`. Correction parameter.}
#'   \item{`meltTemp`}{`numeric(1)` or `NA`. Primary melting temperature.}
#'   \item{`excl`}{`character(1)` or `NA`. Exclusion information.}
#'   \item{`note`}{`character(1)` or `NA`. Free-text note.}
#'   \item{`adp`}{`dpAmpCurveType` or `NA`. Amplification curve.}
#'   \item{`mdp`}{`dpMeltingCurveType` or `NA`. Melting curve.}
#'   \item{`endPt`}{`numeric(1)` or `NA`. End-point fluorescence/result.}
#'   \item{`bgFluor`}{`numeric(1)` or `NA`. Background fluorescence.}
#'   \item{`bgFluorSlp`}{`numeric(1)` or `NA`. Background-fluorescence slope.}
#'   \item{`quantFluor`}{`numeric(1)` or `NA`. Quantification fluorescence.}
#' }
#'
#' @seealso `reactType`, `targetType`, `dpAmpCurveType`, `dpMeltingCurveType`
#' @export

dataType <- 
  S7::new_class("dataType",
            parent = rdmlBaseType,
            properties = list(
              targetId = .testClass("idReferenceType"),
              cq = classNumberNaSingle,
              N0 = classNumberNaSingle,
              ampEffMet = classCharacterNaNonemptySingle,
              ampEff = classNumberNaSingle,
              ampEffSE = classNumberNaSingle,
              corrF = classNumberNaSingle,
              corrP = classNumberNaSingle,
              meltTemp = classNumberNaSingle,
              # Package extension: RDES can represent multiple measured Tm
              # values while the RDML schema has a single meltTemp element.
              # XML serializers intentionally do not emit meltTemps.
              meltTemps = classNumberNaVector,
              excl = classCharacterNaNonemptySingle,
              note = classCharacterNaNonemptySingle,
              adp = .testClassNa("dpAmpCurveType"),
              mdp = .testClassNa("dpMeltingCurveType"),
              endPt = classNumberNaSingle,
              bgFluor = classNumberNaSingle,
              bgFluorSlp = classNumberNaSingle,
              quantFluor = classNumberNaSingle
            ))


# partitionDataType -------------------------------------------------------

#' partitionDataType S7 class
#'
#' Target-specific digital-PCR partition result data. Inherits from `rdmlBaseType`.
#'
#' @format An S7 class.
#'
#' @section Properties:
#' \describe{
#'   \item{`targetId`}{`idReferenceType`. Reference to the target.}
#'   \item{`excluded`}{`character(1)` or `NA`. Exclusion information.}
#'   \item{`note`}{`character(1)` or `NA`. Note.}
#'   \item{`pos`}{Positive integer. Positive partitions.}
#'   \item{`neg`}{Positive integer. Negative partitions.}
#'   \item{`undef`}{Positive integer or `NA`. Undefined partitions.}
#'   \item{`excl`}{Positive integer or `NA`. Excluded partitions.}
#'   \item{`conc`}{`numeric(1)` or `NA`. Calculated concentration.}
#' }
#'
#' @seealso `partitionsType`, `targetType`
#' @export

partitionDataType <- 
  S7::new_class("partitionDataType",
            parent = rdmlBaseType,
            properties = list(
              targetId = .testClass("idReferenceType"),
              excluded = classCharacterNaNonemptySingle,
              note = classCharacterNaNonemptySingle,
              pos = classPositiveIntegerSingle,
              neg = classPositiveIntegerSingle,
              undef = classPositiveIntegerNaSingle,
              excl = classPositiveIntegerNaSingle,
              conc = classNumberNaSingle
            ))


# partitionsType ----------------------------------------------------------

#' partitionsType S7 class
#'
#' Digital-PCR partition metadata and target-specific partition results. Inherits from `rdmlBaseType`.
#'
#' @format An S7 class.
#'
#' @section Properties:
#' \describe{
#'   \item{`volume`}{`numeric(1)`. Partition volume.}
#'   \item{`endPtTable`}{`character(1)` or `NA`. Optional endpoint-table representation/reference.}
#'   \item{`data`}{Target-keyed list of `partitionDataType` objects or `NA`.}
#' }
#'
#' @seealso `partitionDataType`, `reactType`
#' @export

partitionsType <- S7::new_class(
  "partitionsType",
  parent = rdmlBaseType,
  properties = list(
    volume = classNumberSingle,
    endPtTable = classCharacterNaNonemptySingle,
    
    data = .testClassNaKeyedList(
      "partitionDataType",
      key = "targetId"
    )
  )
)


# reactType ---------------------------------------------------------------


#' reactType S7 class
#'
#' One reaction/well within a run. It references a sample and stores target-specific qPCR or digital-PCR results. Inherits from `rdmlBaseType`.
#'
#' @format An S7 class.
#'
#' @section Properties:
#' \describe{
#'   \item{`id`}{`idType`. Reaction identifier or well position.}
#'   \item{`sample`}{`idReferenceType`. Reference to the sample.}
#'   \item{`data`}{Target-keyed list of `dataType` objects or `NA`.}
#'   \item{`partitions`}{List of `partitionsType` objects or `NA`.}
#' }
#'
#' @seealso `runType`, `sampleType`, `dataType`, `partitionsType`
#' @export

reactType <- S7::new_class(
  "reactType",
  parent = rdmlBaseType,
  properties = list(
    id = classId,
    sample = .testClass("idReferenceType"),
    
    data = .testClassNaKeyedList(
      "dataType",
      key = "targetId"
    ),
    
    partitions = .testClassNaList("partitionsType")
  )
)



# dataCollectionSoftwareType ----------------------------------------------

#' dataCollectionSoftwareType S7 class
#'
#' Software used by the instrument to collect qPCR data. Inherits from `rdmlBaseType`.
#'
#' @format An S7 class.
#'
#' @section Properties:
#' \describe{
#'   \item{`name`}{`character(1)`. Software name.}
#'   \item{`version`}{`character(1)`. Software version.}
#' }
#'
#' @seealso `runType`
#' @export

dataCollectionSoftwareType <- 
  S7::new_class("dataCollectionSoftwareType",
            parent = rdmlBaseType,
            properties = list(
              name = classCharacterNonemptySingle,
              version = classCharacterNonemptySingle
            ))



# labelFormatType ---------------------------------------------------------

#' labelFormatType S7 enumeration
#'
#' Labeling convention used for plate rows or columns.
#'
#' @format An S7 class.
#'
#' @section Allowed values:
#' \describe{
#'   \item{`ABC`}{Allowed value.}
#'   \item{`123`}{Allowed value.}
#'   \item{`A1a1`}{Allowed value.}
#' }
#'
#' @seealso `pcrFormatType`
#' @export

labelFormatType <- 
  .newEnumClass(
    "labelFormatType",
    c("ABC", 
      "123",
      "A1a1")
  )

# pcrFormatType -----------------------------------------------------------

#' pcrFormatType S7 class
#'
#' PCR plate or reaction-layout dimensions and labeling conventions. Inherits from `rdmlBaseType`.
#'
#' @format An S7 class.
#'
#' @section Properties:
#' \describe{
#'   \item{`rows`}{Positive integer. Number of rows.}
#'   \item{`columns`}{Positive integer. Number of columns.}
#'   \item{`rowLabel`}{`labelFormatType`. Row-label convention.}
#'   \item{`columnLabel`}{`labelFormatType`. Column-label convention.}
#' }
#'
#' @seealso `labelFormatType`, `runType`
#' @export

pcrFormatType <- 
  S7::new_class("pcrFormatType",
            parent = rdmlBaseType,
            properties = list(
              rows = classPositiveIntegerSingle,
              columns = classPositiveIntegerSingle,
              rowLabel = .testClass("labelFormatType"),
              columnLabel = .testClass("labelFormatType")
            ))

# cqDetectionMethodType ---------------------------------------------------

#' cqDetectionMethodType S7 enumeration
#'
#' Method used to determine Cq values.
#'
#' @format An S7 class.
#'
#' @section Allowed values:
#' \describe{
#'   \item{`automated threshold and baseline settings`}{Allowed value.}
#'   \item{`manual threshold and baseline settings`}{Allowed value.}
#'   \item{`second derivative maximum`}{Allowed value.}
#'   \item{`other`}{Allowed value.}
#' }
#'
#' @seealso `runType`, `dataType`
#' @export

cqDetectionMethodType <-
  .newEnumClass("cqDetectionMethodType",
                 c("automated threshold and baseline settings",
                   "manual threshold and baseline settings",
                   "second derivative maximum",
                   "other")
  )

# runType -----------------------------------------------------------------

#' runType S7 class
#'
#' One qPCR instrument run within an experiment. A run contains instrument/acquisition metadata and reactions. Inherits from `rdmlBaseType`.
#'
#' @format An S7 class.
#'
#' @section Properties:
#' \describe{
#'   \item{`id`}{`idType`. Unique run identifier within the experiment.}
#'   \item{`description`}{`character(1)` or `NA`. Run description.}
#'   \item{`documentation`}{List of `idReferenceType` objects or `NA`. Documentation references.}
#'   \item{`experimenter`}{List of `idReferenceType` objects or `NA`. Experimenter references.}
#'   \item{`instrument`}{`character(1)` or `NA`. Instrument description.}
#'   \item{`dataCollectionSoftware`}{`dataCollectionSoftwareType` or `NA`. Acquisition software.}
#'   \item{`backgroundDeterminationMethod`}{`character(1)` or `NA`. Background determination method.}
#'   \item{`cqDetectionMethod`}{`cqDetectionMethodType` or `NA`. Cq calculation method.}
#'   \item{`thermalCyclingConditions`}{`idReferenceType` or `NA`. Reference to thermal cycling conditions.}
#'   \item{`pcrFormat`}{`pcrFormatType` or `NA`. Plate/layout format.}
#'   \item{`runDate`}{Date-time value or `NA`. Date/time of the run.}
#'   \item{`react`}{Reaction-id-keyed list of `reactType` objects or `NA`.}
#' }
#'
#' @seealso `experimentType`, `reactType`, `pcrFormatType`, `thermalCyclingConditionsType`
#' @export

runType <- S7::new_class(
  "runType",
  parent = rdmlBaseType,
  properties = list(
    id = classId,
    description = classCharacterNaNonemptySingle,
    documentation = .testClassNaList("idReferenceType"),
    experimenter = .testClassNaList("idReferenceType"),
    instrument = classCharacterNaNonemptySingle,
    dataCollectionSoftware = .testClassNa("dataCollectionSoftwareType"),
    backgroundDeterminationMethod = classCharacterNaNonemptySingle,
    cqDetectionMethod = .testClassNa("cqDetectionMethodType"),
    thermalCyclingConditions = .testClassNa("idReferenceType"),
    pcrFormat = .testClassNa("pcrFormatType"),
    runDate = classDateTimeNa,
    
    react = .testClassNaKeyedList(
      "reactType",
      key = "id"
    )
  )
)


# experimentType ----------------------------------------------------------

#' experimentType S7 class
#'
#' A qPCR experiment. One experiment may contain several instrument runs. Inherits from `rdmlBaseType`.
#'
#' @format An S7 class.
#'
#' @section Properties:
#' \describe{
#'   \item{`id`}{`idType`. Unique experiment identifier.}
#'   \item{`description`}{`character(1)` or `NA`. Experiment description.}
#'   \item{`documentation`}{List of `idReferenceType` objects or `NA`. Documentation references.}
#'   \item{`run`}{Run-id-keyed list of `runType` objects or `NA`.}
#' }
#'
#' @seealso `runType`, `rdmlType`
#' @export

experimentType <- S7::new_class(
  "experimentType",
  parent = rdmlBaseType,
  properties = list(
    id = classId,
    description = classCharacterNaNonemptySingle,
    documentation = .testClassNaList("idReferenceType"),
    
    run = .testClassNaKeyedList(
      "runType",
      key = "id"
    )
  )
)


# rdmlIdType -------------------------------------------------------------

#' rdmlIdType S7 class
#'
#' Publisher identifier for an RDML document. Inherits from `rdmlBaseType`.
#'
#' @format An S7 class.
#'
#' @section Properties:
#' \describe{
#'   \item{`publisher`}{`character(1)`. Publisher/producer identifier.}
#'   \item{`serialNumber`}{`character(1)`. Serial number assigned by the publisher.}
#'   \item{`MD5Hash`}{`character(1)` or `NA`. Optional MD5 hash.}
#' }
#'
#' @seealso `rdmlType`
#' @export

rdmlIdType <- 
  S7::new_class("rdmlIdType",
            parent = rdmlBaseType,
            properties = list(
              publisher = classCharacterNonemptySingle,
              serialNumber = classCharacterNonemptySingle,
              MD5Hash = classCharacterNaNonemptySingle
            ))




# rdmlType ----------------------------------------------------------------

#' rdmlType S7 class
#'
#' Top-level container representing one RDML document. Its structure mirrors the RDML schema and stores reusable metadata together with experiments and runs. Inherits from `rdmlBaseType`.
#'
#' @format An S7 class.
#'
#' @section Properties:
#' \describe{
#'   \item{`version`}{`character(1)`. RDML schema version represented by this object. Always `"1.3"` and read-only.}
#'   \item{`dateMade`}{Date-time value or `NA`. Document creation date.}
#'   \item{`dateUpdated`}{Date-time value or `NA`. Last update date.}
#'   \item{`id`}{List of `rdmlIdType` objects or `NA`. Publisher identifiers.}
#'   \item{`experimenter`}{Experimenter-id-keyed list of `experimenterType` objects or `NA`.}
#'   \item{`documentation`}{Documentation-id-keyed list of `documentationType` objects or `NA`.}
#'   \item{`dye`}{Dye-id-keyed list of `dyeType` objects or `NA`.}
#'   \item{`sample`}{Sample-id-keyed list of `sampleType` objects or `NA`.}
#'   \item{`target`}{Target-id-keyed list of `targetType` objects or `NA`.}
#'   \item{`thermalCyclingConditions`}{Id-keyed list of `thermalCyclingConditionsType` objects or `NA`.}
#'   \item{`experiment`}{Experiment-id-keyed list of `experimentType` objects or `NA`.}
#' }
#'
#' @seealso `rdmlRead`, `rdmlWrite`, `asTable`, `getFData`, `rdmlValidate`
#' @export

rdmlType <- S7::new_class(
  "rdmlType",
  parent = rdmlBaseType,
  
  properties = list(
    version = S7::new_property(
      S7::class_character,
      getter = function(self) .rdmlSchemaVersion
    ),
    dateMade = classDateTimeNa,
    dateUpdated = classDateTimeNa,
    id = .testClassNaList("rdmlIdType"),
    experimenter = .testClassNaIdList("experimenterType"),
    documentation = .testClassNaIdList("documentationType"),
    dye = .testClassNaIdList("dyeType"),
    sample = .testClassNaIdList("sampleType"),
    target = .testClassNaIdList("targetType"),
    thermalCyclingConditions =
      .testClassNaIdList(
        "thermalCyclingConditionsType"
      ),
    experiment = .testClassNaIdList("experimentType")
  )
)
