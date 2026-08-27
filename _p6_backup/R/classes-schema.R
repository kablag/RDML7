# experimenterType --------------------------------------------------------


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

#' @export
sampleTypeType <- .newEnumClass(
  "sampleTypeType",
  c(
    "unkn", "ntc", "nac",
    "std", "ntp", "nrt",
    "pos", "opt"
  )
)

#' @export
sampleTargetType <- S7::new_class(
  "sampleTargetType",
  parent = rdmlBaseType,
  properties = list(
    targetId = .testClass("idReferenceType"),
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

#' @export
nucleotideType <- .newEnumClass(
  "nucleotideType",
  c("DNA", "genomic DNA", "cDNA", "RNA")
)

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
    type = .testClassNaKeyedList(
      "sampleTargetType",
      key = "targetId"
    ),
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


#' @export
commercialAssayType <- S7::new_class(
  "commercialAssayType",
  parent = rdmlBaseType,
  properties = list(
    company = classCharacterNonemptySingle,
    orderNumber = classCharacterNonemptySingle
  )
)

#' @export
targetTypeType <-  .newEnumClass(
  "targetTypeType",
  c("ref", "toi")
)

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

#' @export
measureType <-  .newEnumClass(
  "measureType",
  c("real time", "meltcurve")
)


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

#' @export
temperatureType <- S7::new_class(
  "temperatureType",
  parent = temperatureBaseType,
  properties = list(
    temperature = classNumberSingle
  )
)

#' @export
gradientType <- S7::new_class(
  "gradientType",
  parent = temperatureBaseType,
  properties = list(
    highTemperature = classNumberSingle,
    lowTemperature = classNumberSingle
  )
)

#' @export
loopType <- S7::new_class(
  "loopType",
  parent = rdmlBaseType,
  properties = list(
    goto = classPositiveIntegerSingle,
    "repeat" = classPositiveIntegerSingle
  )
)

#' @export
pauseType <- S7::new_class(
  "pauseType",
  parent = rdmlBaseType,
  properties = list(
    temperature = classNumberSingle
  )
)

#' @export
lidOpenType <- S7::new_class(
  "lidOpenType",
  parent = rdmlBaseType,
  properties = list(
  )
)

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

#' @export
dataCollectionSoftwareType <- 
  S7::new_class("dataCollectionSoftwareType",
            parent = rdmlBaseType,
            properties = list(
              name = classCharacterNonemptySingle,
              version = classCharacterNonemptySingle
            ))



# labelFormatType ---------------------------------------------------------

#' @export
labelFormatType <- 
  .newEnumClass(
    "labelFormatType",
    c("ABC", 
      "123",
      "A1a1")
  )

# pcrFormatType -----------------------------------------------------------

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

#' @export
cqDetectionMethodType <-
  .newEnumClass("cqDetectionMethodType",
                 c("automated threshold and baseline settings",
                   "manual threshold and baseline settings",
                   "second derivative maximum",
                   "other")
  )

# runType -----------------------------------------------------------------

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

#' @export
rdmlType <- S7::new_class(
  "rdmlType",
  parent = rdmlBaseType,
  
  properties = list(
    version = S7::class_character,
    dateMade = classDateTimeNa,
    dateUpdated = classDateTimeNa,
    
    id =
      .testClassNaList("rdmlIdType"),
    
    experimenter =
      .testClassNaIdList("experimenterType"),
    
    documentation =
      .testClassNaIdList("documentationType"),
    
    dye =
      .testClassNaIdList("dyeType"),
    
    sample =
      .testClassNaIdList("sampleType"),
    
    target =
      .testClassNaIdList("targetType"),
    
    thermalCyclingConditions =
      .testClassNaIdList(
        "thermalCyclingConditionsType"
      ),
    
    experiment =
      .testClassNaIdList("experimentType")
  )
)
