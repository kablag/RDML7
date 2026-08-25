# experimenterType --------------------------------------------------------


#' @export
experimenterType <- S7::new_class(
  "experimenterType",
  parent = rdmlBaseType,
  properties = list(
    id = class_id,
    firstName = class_character_nonempty_single,
    lastName = class_character_nonempty_single,
    email = class_character_na_nonempty_single,
    labName = class_character_na_nonempty_single,
    labAddress = class_character_na_nonempty_single
  )
)


# documentationType -------------------------------------------------------

#' @export
documentationType <- S7::new_class(
  "documentationType",
  parent = rdmlBaseType,
  properties = list(
    id = class_id,
    text = class_character_na_nonempty_single
  )
)




# dyeChemistryType --------------------------------------------------------


#' @export
dyeChemistryType <- 
  new_enum_class(
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
    id = class_id,
    description = class_character_na_nonempty_single,
    dyeChemistry = S7::new_property(
      S7::class_any,
      validator = function(value) {
        if (.is_single_na(value)) {
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
    name = class_character_na_nonempty_single,
    id = class_character_na_nonempty_single
  )
)


# annotationType ----------------------------------------------------------


#' @export
annotationType <- S7::new_class(
  "annotationType",
  parent = rdmlBaseType,
  properties = list(
    property = class_character_na_nonempty_single,
    value = class_character_na_nonempty_single
  )
)

# sampleTypeType ----------------------------------------------------------

#' @export
sampleTypeType <- new_enum_class(
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
    targetId = test_class("idReferenceType"),
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
quantityUnitType <- new_enum_class(
  "quantityUnitType",
  c("cop", "fold", "dil", "ng", "nMol", "other")
)

class_quantityUnitType_nonempty_single <- S7::new_property(
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
    targetId = test_class("idReferenceType"),
    value = class_number_single,
    unit = class_quantityUnitType_nonempty_single
  )
)

#' @export
primingMethodType <- new_enum_class(
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
    enzyme = class_character_na_nonempty_single,
    primingMethod = test_class_na("primingMethodType"),
    dnaseTreatment = class_flag_na,
    thermalCyclingConditions = test_class_na("idReferenceType")
  )
)

#' @export
nucleotideType <- new_enum_class(
  "nucleotideType",
  c("DNA", "genomic DNA", "cDNA", "RNA")
)

#' @export
templateQuantityType <- S7::new_class(
  "templateQuantityType",
  parent = rdmlBaseType,
  properties = list(
    conc = class_number_single,
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
    id = class_id,
    description = class_character_na_nonempty_single,
    documentation = test_class_na_list("idReferenceType"),
    xRef = test_class_na_list("xRefType"),
    annotation = test_class_na_list("annotationType"),
    type = test_class_na_keyed_list(
      "sampleTargetType",
      key = "targetId"
    ),
    interRunCalibrator = class_flag_na,
    quantity = test_class_na_keyed_list(
      "quantityType",
      key = "targetId"
    ),
    calibratorSample = class_flag_na,
    cdnaSynthesisMethod = test_class_na("cdnaSynthesisMethodType"),
    templateQuantity = test_class_na("templateQuantityType")
  )
)

#' @export
oligoType <- S7::new_class(
  "oligoType",
  parent = rdmlBaseType,
  properties = list(
    threePrimeTag = class_character_na_nonempty_single,
    fivePrimeTag = class_character_na_nonempty_single,
    sequence = class_character_nonempty_single
  )
)

#' @export
sequencesType <- S7::new_class(
  "sequencesType",
  parent = rdmlBaseType,
  properties = list(
    forwardPrimer = test_class_na("oligoType"), 
    reversePrimer = test_class_na("oligoType"),
    probe1 = test_class_na("oligoType"),
    probe2 = test_class_na("oligoType"),
    amplicon = test_class_na("oligoType")
  )
)


#' @export
commercialAssayType <- S7::new_class(
  "commercialAssayType",
  parent = rdmlBaseType,
  properties = list(
    company = class_character_nonempty_single,
    orderNumber = class_character_nonempty_single
  )
)

#' @export
targetTypeType <-  new_enum_class(
  "targetTypeType",
  c("ref", "toi")
)

#' @export
targetType <- S7::new_class(
  "targetType",
  parent = rdmlBaseType,
  properties = list(
    id = class_id,
    description = class_character_na_nonempty_single,
    documentation = test_class_na_list("idReferenceType"),
    xRef = test_class_na_list("xRefType"),
    type = test_class("targetTypeType"),
    amplificationEfficiencyMethod = class_character_na_nonempty_single,
    amplificationEfficiency = class_number_na_single,
    amplificationEfficiencySE = class_number_na_single,
    meltingTemperature = class_number_na_single,
    detectionLimit = class_number_na_single,
    dyeId = test_class("idReferenceType"),
    sequences = test_class_na("sequencesType"),
    commercialAssay = test_class_na("commercialAssayType")
  )
)


# thermalCyclingConditions ------------------------------------------------

#' @export
measureType <-  new_enum_class(
  "measureType",
  c("real time", "meltcurve")
)


#' @export
temperatureBaseType <- S7::new_class(
  "temperatureBaseType",
  parent = rdmlBaseType,
  properties = list(
    duration = class_positive_integer_single,
    temperatureChange = class_number_na_single,
    durationChange = class_integer_na_single,
    measure = test_class_na("measureType"),
    ramp = class_number_na_single
  )
)

#' @export
temperatureType <- S7::new_class(
  "temperatureType",
  parent = temperatureBaseType,
  properties = list(
    temperature = class_number_single
  )
)

#' @export
gradientType <- S7::new_class(
  "gradientType",
  parent = temperatureBaseType,
  properties = list(
    highTemperature = class_number_single,
    lowTemperature = class_number_single
  )
)

#' @export
loopType <- S7::new_class(
  "loopType",
  parent = rdmlBaseType,
  properties = list(
    goto = class_positive_integer_single,
    "repeat" = class_positive_integer_single
  )
)

#' @export
pauseType <- S7::new_class(
  "pauseType",
  parent = rdmlBaseType,
  properties = list(
    temperature = class_number_single
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
    nr = class_positive_integer_single,
    description = class_character_na_nonempty_single,
    temperature = test_class_na("temperatureType"),
    gradient = test_class_na("gradientType"),
    loop = test_class_na("loopType"),
    pause = test_class_na("pauseType"),
    lidOpen = test_class_na("lidOpenType")
  )
)

#' @export
thermalCyclingConditionsType <- S7::new_class(
  "thermalCyclingConditionsType",
  parent = rdmlBaseType,
  properties = list(
    id = class_id,
    description = class_character_na_nonempty_single,
    documentation = test_class_na_list("idReferenceType"),
    lidTemperature = class_number_na_single,
    experimenter = test_class_na_list("idReferenceType"),
    step = test_class_list("stepType")
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
    fpoints = class_datatable
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
    fpoints = class_datatable
  )
)

# dataType ----------------------------------------------------------------


#' @export
dataType <- 
  S7::new_class("dataType",
            parent = rdmlBaseType,
            properties = list(
              targetId = test_class("idReferenceType"),
              cq = class_number_na_single,
              N0 = class_number_na_single,
              ampEffMet = class_character_na_nonempty_single,
              ampEff = class_number_na_single,
              ampEffSE = class_number_na_single,
              corrF = class_number_na_single,
              corrP = class_number_na_single,
              meltTemp = class_number_na_single,
              excl = class_character_na_nonempty_single,
              note = class_character_na_nonempty_single,
              adp = test_class_na("dpAmpCurveType"),
              mdp = test_class_na("dpMeltingCurveType"),
              endPt = class_number_na_single,
              bgFluor = class_number_na_single,
              bgFluorSlp = class_number_na_single,
              quantFluor = class_number_na_single
            ))


# partitionDataType -------------------------------------------------------

#' @export
partitionDataType <- 
  S7::new_class("partitionDataType",
            parent = rdmlBaseType,
            properties = list(
              targetId = test_class("idReferenceType"),
              excluded = class_character_na_nonempty_single,
              note = class_character_na_nonempty_single,
              pos = class_positive_integer_single,
              neg = class_positive_integer_single,
              undef = class_positive_integer_na_single,
              excl = class_positive_integer_na_single,
              conc = class_number_na_single
            ))


# partitionsType ----------------------------------------------------------

#' @export
partitionsType <- S7::new_class(
  "partitionsType",
  parent = rdmlBaseType,
  properties = list(
    volume = class_number_single,
    endPtTable = class_character_na_nonempty_single,
    
    data = test_class_na_keyed_list(
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
    id = class_id,
    sample = test_class("idReferenceType"),
    
    data = test_class_na_keyed_list(
      "dataType",
      key = "targetId"
    ),
    
    partitions = test_class_na_list("partitionsType")
  )
)



# dataCollectionSoftwareType ----------------------------------------------

#' @export
dataCollectionSoftwareType <- 
  S7::new_class("dataCollectionSoftwareType",
            parent = rdmlBaseType,
            properties = list(
              name = class_character_nonempty_single,
              version = class_character_nonempty_single
            ))



# labelFormatType ---------------------------------------------------------

#' @export
labelFormatType <- 
  new_enum_class(
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
              rows = class_positive_integer_single,
              columns = class_positive_integer_single,
              rowLabel = test_class("labelFormatType"),
              columnLabel = test_class("labelFormatType")
            ))

# cqDetectionMethodType ---------------------------------------------------

#' @export
cqDetectionMethodType <-
  new_enum_class("cqDetectionMethodType",
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
    id = class_id,
    description = class_character_na_nonempty_single,
    documentation = test_class_na_list("idReferenceType"),
    experimenter = test_class_na_list("idReferenceType"),
    instrument = class_character_na_nonempty_single,
    dataCollectionSoftware = test_class_na("dataCollectionSoftwareType"),
    backgroundDeterminationMethod = class_character_na_nonempty_single,
    cqDetectionMethod = test_class_na("cqDetectionMethodType"),
    thermalCyclingConditions = test_class_na("idReferenceType"),
    pcrFormat = test_class_na("pcrFormatType"),
    runDate = class_datetime_na,
    
    react = test_class_na_keyed_list(
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
    id = class_id,
    description = class_character_na_nonempty_single,
    documentation = test_class_na_list("idReferenceType"),
    
    run = test_class_na_keyed_list(
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
              publisher = class_character_nonempty_single,
              serialNumber = class_character_nonempty_single,
              MD5Hash = class_character_na_nonempty_single
            ))




# rdmlType ----------------------------------------------------------------

#' @export
rdmlType <- S7::new_class(
  "rdmlType",
  parent = rdmlBaseType,
  
  properties = list(
    version = S7::class_character,
    dateMade = class_datetime_na,
    dateUpdated = class_datetime_na,
    
    id =
      test_class_na_list("rdmlIdType"),
    
    experimenter =
      test_class_na_id_list("experimenterType"),
    
    documentation =
      test_class_na_id_list("documentationType"),
    
    dye =
      test_class_na_id_list("dyeType"),
    
    sample =
      test_class_na_id_list("sampleType"),
    
    target =
      test_class_na_id_list("targetType"),
    
    thermalCyclingConditions =
      test_class_na_id_list(
        "thermalCyclingConditionsType"
      ),
    
    experiment =
      test_class_na_id_list("experimentType")
  )
)
