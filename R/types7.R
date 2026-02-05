library(S7)
library(checkmate)

asXMLnodes <- new_generic("asXMLnodes", "x")


class_datetime_na <- new_property(
  class_any,
  validator = function(value) {
    if (!is.na(value) && (is.na(ymd_hms(dateTime, quiet = TRUE)) ||
                          is.na(ymd(dateTime, quiet = TRUE))))
      "must be a NA or pass lubridate ymd_hms() / ymd() conversion"
  }, default = as.character(NA)
)

class_character_na_nonempty_single <- new_property(
  class_any,
  validator = function(value) {
    # if (is.na(value)) return()
    # value <- 
    if (#!identical(value, character(0)) 
      # !checkString(value, na.ok = TRUE) && 
      !testString(value, #gsub(" ", "", value),
                  min.chars = 1,
                  pattern = "[^[:space:]]",
                  na.ok = TRUE)
    )
      "must be a NA or a single non-empty string"
  }, default = as.character(NA))

class_character_nonempty_single <- new_property(
  class_character,
  validator = function(value) {
    value <- gsub(" ", "", value)
    if (length(value) != 1 || is.na(value) || value == "")
      "must be a single non-empty string"
  })

class_flag_na <- new_property(
  class_any,
  validator = function(value) {
    # if (is.na(value)) return()
    # value <- 
    if (#!identical(value, character(0)) 
      # !checkString(value, na.ok = TRUE) && 
      !testFlag(value, na.ok = TRUE)
    )
      "must be a NA or a flag"
  }, default = as.logical(NA))

class_flag <- new_property(
  class_logical,
  validator = function(value) {
    if (length(value) != 1)
      "must be a flag"
  })

class_datatable <- new_property(
  class_data.frame,
  # validator = function(value) {
  #   if ((value) != 1)
  #     "must be a single data.frame or data.table"
  # },
  setter = function(self, value) {
    if (!is.data.table(value)) {
      value <- as.data.table(value)
    }
    self@fpoints <- value
    self
  })

class_number_na_single <- new_property(
  class_any,
  validator = function(value) {
    # if (is.na(value)) return()
    # value <- 
    if (!testNumber(value, na.ok = TRUE))
      "must be a NA or a number"
  }, default = as.character(NA))

class_number_single <- new_property(
  class_numeric,
  validator = function(value) {
    if (length(value) != 1 || is.na(value))
      "must be a number"
  })

class_positive_integer_single <- new_property(
  class_integer,
  validator = function(value) {
    if (!testInt(value, lower = 0))
      "must be a positive integer"
  }
)

class_integer_na_single <- new_property(
  class_any,
  validator = function(value) {
    # if (is.na(value)) return()
    # value <- 
    if (!testInt(value, na.ok = TRUE))
      "must be a NA or a integer"
  }, default = as.integer(NA))

class_positive_integer_na_single <- new_property(
  class_any,
  validator = function(value) {
    # if (is.na(value)) return()
    # value <- 
    if (!testInt(value, lower = 0, na.ok = TRUE))
      "must be a NA or a positive integer"
  }, default = as.integer(NA))

test_class <- function(className) {
  new_property(
    class_any,
    validator = function(value) {
      if (testClass(value, className) &&
          length(value) == 1)
        NULL
      else
        paste("must be a", className)
    },
    default = NA
  )
}

test_class_na <- function(className) {
  new_property(
    class_any,
    validator = function(value) {
      if (is.na(value) || 
          (testClass(value, className) &&
           length(value) == 1))
        NULL
      else
        paste("must be a NA or", className)
    },
    default = NA
  )
}

test_class_list <- function(className) {
  new_property(
    class_any,
    validator = function(value) {
      if (
        is.list(value) &&
        all(sapply(value, \(x) testClass(x, className)))
      )
        NULL
      else
        paste("must be a list of", className)
    },
    default = NA
  )
}

test_class_na_list <- function(className) {
  new_property(
    class_any,
    validator = function(value) {
      if (is.na(value) || (
        is.list(value) &&
        all(sapply(value, \(x) testClass(x, className))))
      )
        NULL
      else
        paste("must be NA or a list of", className)
    },
    default = NA
  )
}

rdmlEnum <- new_class(
  "rdmlEnum",
  properties = list(
    value = class_character,
    variants = class_character
  ),
  validator = function(self) { 
    if (length(self@value) != 1L) {
      "enum value's are length 1"
    } else if (!(self@value %in% self@variants)) {
      sprintf("enum value must be one of possible variants: %s",
              paste(self@variants, collapse = ", "))
    }
  }, 
  abstract = TRUE
)
print.rdmlEnum <- function(x, ...) {
  cat(class(x)[1], "::", x@value, sep = "")
  invisible(x)
}
as.character.rdmlEnum <- function(x) {
  x@value
}

method(asXMLnodes, rdmlEnum) <- function(x, nodeName) {
  assertString(nodeName)
  subnodesNames <- names(props(x))
  sprintf("<%s>%s</%s>",
          nodeName, x@value, nodeName)
}

new_enum_class <- function(enum_class, variants) {
  new_class(
    enum_class,
    parent = rdmlEnum,
    properties = list(
      value = class_character,
      variants = new_property(class_character, default = variants)
    ),
    constructor = function(value = "") {
      new_object(S7_object(), value = value, variants = variants)
    }
  )
}

idType <- new_class(
  "idType",
  properties = list(
    id = class_character
  ),
  validator = function(self) { 
    if (length(self@id) != 1L) {
      "id value's are length 1"
    }
  }
)
print.idType <- function(x, ...) {
  cat(x@id)
  invisible(x)
}
as.character.idType <- function(x) {
  as.character(x@id)
}
method(asXMLnodes, idType) <- function(x, nodeName) {
  assertString(nodeName)
  # nodeName <- deparse(substitute(x))
  sprintf("<%s id=%s/>",
          nodeName, x@id)
}
class_id <- new_property(
  validator = function(value) {
    if (!(testClass(value, "idType")))
      "must be a idType"
  }
)

idReferenceType <- new_class(
  "idReferenceType",
  parent = idType
)

rdmlBaseType <- new_class(
  "rdmlBaseType",
  abstract = TRUE)


method(asXMLnodes, rdmlBaseType) <- function(x,
                                             nodeName
                                             #, attribute = ""
) {
  assertString(nodeName)
  # assertString(attribute)
  # nodeName <- class(x)[1]
  subnodesNames <- names(props(x))
  sprintf("<%s%s>%s</%s>",
          nodeName, #node name
          #attribute
          {
            if ((subnodesNames[1] == "id")) {
              attribute <- sprintf(" %s=%s", 
                                   subnodesNames[1],
                                   prop(x, subnodesNames[1])@id)
              subnodesNames <- subnodesNames[-1]
              attribute
            } else {
              ""
            }
          },
          # value
          {
            subnodes <- 
              sapply(
                subnodesNames,
                function(subnodeName) {
                  # subnodeName <- gsub("^\\.(.*)$",
                  #                      "\\1", name)
                  switch(
                    typeof(prop(x, subnodeName)),
                    closure = NULL,
                    list =
                      sapply(prop(x, subnodeName),
                             function(sublist)
                               asXMLnodes(sublist, subnodeName)) |> 
                      # .[!sapply(., is.null)] |>
                      paste0(collapse = "\n")
                    ,
                    S4 = {
                      asXMLnodes(
                        prop(x, subnodeName),
                        subnodeName
                      )
                    },
                    {
                      if (!testClass(prop(x, subnodeName),
                                     "rdmlBaseType") && (
                                       is.null(prop(x, subnodeName)) ||
                                       is.na(prop(x, subnodeName))
                                     ))
                      {
                        NULL
                      } else {
                        sprintf("<%s>%s</%s>\n",
                                subnodeName,
                                switch(
                                  typeof(prop(x, subnodeName)),
                                  logical =
                                    ifelse(prop(x, subnodeName),
                                           "true",
                                           "false"
                                    ),
                                  prop(x, subnodeName)
                                ),
                                subnodeName
                        )
                      }
                    })
                })
            # browser()
            subnodes <- unlist(subnodes)
            paste0(subnodes, collapse = "")
          },
          nodeName)
}

method(names, rdmlBaseType) <- function(x) {
  prop_names(x)
}
`$.rdmlBaseType` <- function(x, name) {
  if (typeof(x) %in% c("list", "environment")) {
    NextMethod()
  } else {
    prop(x, name)
  }
}
`$<-.rdmlBaseType` <- function(x, name, value) {
  prop(x, name) <- value
  x
}

experimenterType <- new_class(
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

documentationType <- new_class(
  "documentationType",
  parent = rdmlBaseType,
  properties = list(
    id = class_id,
    text = class_character_na_nonempty_single
  )
)

dyeChemistryType <- 
  new_enum_class(
    "DyeChemistryType",
    c("non-saturating DNA binding dye", 
      "saturating DNA binding dye",
      "hybridization probe",
      "hydrolysis probe", 
      "labelled forward primer", 
      "labelled reverse primer",
      "DNA-zyme probe")
  )

dyeType <- new_class(
  "dyeType",
  parent = rdmlBaseType,
  properties = list(
    id = class_id,
    description = class_character_na_nonempty_single,
    dyeChemistry = 
      new_property(
        class_any,
        validator = function(value) {
          if (
            !(testClass(value, 
                        "dyeChemistryType") ||
              is.na(value))
          )
            "must be a NA or a single DyeChemistryType"
        },
        default = NA
      )
  )
)

xRefType <- new_class(
  "xRefType",
  parent = rdmlBaseType,
  properties = list(
    name = class_character_na_nonempty_single,
    id = class_character_na_nonempty_single
  )
)

annotationType <- new_class(
  "annotationType",
  parent = rdmlBaseType,
  properties = list(
    property = class_character_na_nonempty_single,
    value = class_character_na_nonempty_single
  )
)

sampleTypeType <- 
  new_enum_class(
    "sampleTypeType",
    c("unkn", "ntc", "nac",
      "std", "ntp", "nrt",
      "pos", "opt")
  )

sampleTargetType <- new_class(
  "sampleTargetType",
  parent = rdmlBaseType,
  properties = list(
    targetId = class_id,
    sampleType = 
      new_property(
        sampleTypeType,
        validator = function(value) {
          if (length(value) != 1)
            "must be a single sampleTypeType"
        }
      )
  )
)

quantityUnitType <- 
  new_enum_class(
    "quantityUnitType",
    c("cop", "fold", "dil",
      "ng", "nMol", "other")
  )

class_quantityUnitType_nonempty_single <- 
  new_property(
    quantityUnitType,
    validator = function(value) {
      if (length(value) != 1)
        "must be a single quantityUnitType"
    }
  )

quantityType <- new_class(
  "quantityType",
  parent = rdmlBaseType,
  properties = list(
    targetId = class_id,
    value = class_numeric,
    unit = class_quantityUnitType_nonempty_single
  )
)

# !!!!!!!!!!!
cdnaSynthesisMethodType <- new_class(
  "cdnaSynthesisMethodType",
  parent = rdmlBaseType,
  properties = list(
    enzyme = class_character_na_nonempty_single,
    primingMethod = class_number_na_single,
    dnaseTreatment = class_logical,
    thermalCyclingConditions = test_class_na("isReferenceType")
  )
)

nucleotideType <- 
  new_enum_class(
    "nucleotideType",
    c("DNA", "genomic DNA", "cDNA", "RNA")
  )

templateQuantityType <- new_class(
  "templateQuantityType",
  parent = rdmlBaseType,
  properties = list(
    conc = class_number_single,
    nucleotide = new_property(
      nucleotideType,
      validator = function(value) {
        if (length(value) != 1)
          "must be a single nucleotideType"
      }
    )
  )
)


sampleType <- new_class(
  "sampleType",
  parent = rdmlBaseType,
  properties = list(
    id = class_id,
    description = class_character_na_nonempty_single,
    documentation = test_class_na_list("idReferenceType"),
    xRef = test_class_na_list("xRefType"),
    type = test_class_na_list("sampleTargetType"),
    interRunCalibrator = class_flag_na,
    quantity = test_class_na_list("quantityType"),
    calibratorSample = class_flag_na,
    cdnaSynthesisMethod = test_class_na("cdnaSynthesisMethodType"),
    templateQuantity = test_class_na("templateQuantityType")
  )
)

oligoType <- new_class(
  "oligoType",
  parent = rdmlBaseType,
  properties = list(
    threePrimeTag = class_character_na_nonempty_single,
    fivePrimeTag = class_character_na_nonempty_single,
    sequence = class_character_nonempty_single
  )
)

sequencesType <- new_class(
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


commercialAssayType <- new_class(
  "commercialAssayType",
  parent = rdmlBaseType,
  properties = list(
    company = class_character_nonempty_single,
    orderNumber = class_character_nonempty_single
  )
)

targetTypeType <-  new_enum_class(
  "targetTypeType",
  c("ref", "toi")
)

targetType <- new_class(
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

measureType <-  new_enum_class(
  "measureType",
  c("real time", "meltcurve")
)


temperatureBaseType <- new_class(
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

temperatureType <- new_class(
  "temperatureBaseType",
  parent = rdmlBaseType,
  properties = list(
    temperature = class_number_single
  )
)

gradientType <- new_class(
  "gradientType",
  parent = temperatureBaseType,
  properties = list(
    highTemperature = class_number_single,
    lowTemperature = class_number_single
  )
)

loopType <- new_class(
  "loopType",
  parent = rdmlBaseType,
  properties = list(
    goto = class_positive_integer_single,
    "repeat" = class_positive_integer_single
  )
)

pauseType <- new_class(
  "pauseType",
  parent = rdmlBaseType,
  properties = list(
    temperature = class_number_single
  )
)

lidOpenType <- new_class(
  "lidOpenType",
  parent = rdmlBaseType,
  properties = list(
  )
)

stepType <- new_class(
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

thermalCyclingConditionsType <- new_class(
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

dpAmpCurveType <- new_class(
  "dpAmpCurveType",
  parent = rdmlBaseType,
  validator = function(self) {
    if (!testSetEqual(colnames(self@fpoints), c("cyc", "fluor")) &&
        !testSetEqual(colnames(self@fpoints), c("cyc", "tmp", "fluor"))) {
      "@fpoints must have columns: cyc, tmp (optional), fluor"
    }
  },
  properties = list(
    fpoints = class_datatable
  )
)

method(asXMLnodes, dpAmpCurveType) <- function(x, nodeName) {
  dt <- x@fpoints
  if (ncol(dt) == 2)
    sprintf("<adp><cyc>%f</cyc><fluor>%f</fluor></adp>", dt$cyc, dt$fluor)
  else
    sprintf("<adp><cyc>%f</cyc><tmp>%f</tmp><fluor>%f</fluor></adp>", 
            dt$cyc,
            dt$tmp,
            dt$fluor)
}

dpMeltingCurveType <- new_class(
  "dpMeltingCurveType",
  parent = rdmlBaseType,
  validator = function(self) {
    if (colnames(self@fpoints) != c("tmp", "fluor")) {
      "@fpoints must have columns: tmp, fluor"
    }
  },
  properties = list(
    fpoints = class_datatable
  )
)

method(asXMLnodes, dpMeltingCurveType) <- function(x, nodeName) {
  dt <- x@fpoints
  sprintf("<mdp><tmp>%f</tmp><fluor>%f</fluor></mdp>", dt$tmp, dt$fluor)
}


# dataType ----------------------------------------------------------------


dataType <- 
  new_class("dataType",
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

partitionDataType <- 
  new_class("partitionDataType",
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

partitionsType <- 
  new_class("partitionsType",
            parent = rdmlBaseType,
            properties = list(
              volume = class_number_single,
              endPtTable = class_character_na_nonempty_single,
              data = test_class_list("partitionDataType")
            ))



# reactType ---------------------------------------------------------------


reactType <- new_class("reactType", parent = rdmlBaseType, properties = list(
  id = class_id,
  sample = test_class("idReferenceType"),
  data = test_class_na_list("dataType"),
  patitions = test_class_na_list("partitionsType")
))



# dataCollectionSoftwareType ----------------------------------------------

dataCollectionSoftwareType <- 
  new_class("dataCollectionSoftwareType",
            parent = rdmlBaseType,
            properties = list(
              name = class_character_nonempty_single,
              version = class_character_nonempty_single
            ))



# labelFormatType ---------------------------------------------------------

labelFormatType <- 
  new_enum_class(
    "labelFormatType",
    c("ABC", 
      "123",
      "A1a1")
  )

# pcrFormatType -----------------------------------------------------------

pcrFormatType <- 
  new_class("pcrFormatType",
            parent = rdmlBaseType,
            properties = list(
              rows = class_positive_integer_single,
              columns = class_positive_integer_single,
              rowLabel = test_class("labelFormatType"),
              columnLabel = test_class("labelFormatType")
            ))

# cqDetectionMethodType ---------------------------------------------------

cqDetectionMethodType <-
  new_enum_class("cqDetectionMethodType",
                 c("automated threshold and baseline settings",
                   "manual threshold and baseline settings",
                   "second derivative maximum",
                   "other")
  )

# runType -----------------------------------------------------------------

runType <- 
  new_class(
    "runType",
    parent = rdmlBaseType,
    properties = list(
      id = class_id,
      description = class_character_na_nonempty_single,
      documentation = test_class_na_list("idReferenceType"),
      experimenter = test_class_na_list("idReferenceType"),
      instrument = class_character_na_nonempty_single,
      dataCollectionSoftware = test_class_na("dataCollectionSoftwareType"),
      backgroundDetermenationMethod = class_character_na_nonempty_single,
      cqDetectionMethod = test_class_na("cqDetectionMethodType"),
      thermalCyclingConditions = test_class_na("idReferenceType"),
      pcrFormat = test_class_na("pcrFormatType"),
      runDate = class_datetime_na,
      react = test_class_na_list("reactType")
    ))


# experimentType ----------------------------------------------------------

experimentType <- 
  new_class("experimentType",
            parent = rdmlBaseType,
            properties = list(
              id = class_id,
              description = class_character_na_nonempty_single,
              documentation = test_class_na_list("idReferenceType"),
              run = test_class_na_list("runType")
            ))



# rdmlIdType -------------------------------------------------------------

rdmlIdType <- 
  new_class("rdmlIdType",
            parent = rdmlBaseType,
            properties = list(
              publisher = class_character_nonempty_single,
              serialNumber = class_character_nonempty_single,
              MD5Hash = class_character_na_nonempty_single
            ))




# rdmlType ----------------------------------------------------------------

rdmlType <- 
  new_class("rdmlType", 
            parent = rdmlBaseType, 
            properties = list(
              version = class_character,
              dateMade = class_datetime_na,
              dateUpadted = class_datetime_na,
              id = test_class_na_list("rdmlIdType"),
              experimenter = test_class_na_list("experimenterType"),
              documentation = test_class_na_list("documentationType"),
              dye = test_class_na_list("dyeType"),
              sample = test_class_na_list("sampleType"),
              target = test_class_na_list("targetType"),
              thermalCyclingConditions = test_class_na_list("thermalCyclingConditionsType"),
              experiment = test_class_na_list("experimentType")
            ))


