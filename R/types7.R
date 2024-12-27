library(S7)
library(checkmate)

asXMLnodes <- new_generic("asXMLnodes", "x")

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

class_number_na_single <- new_property(
  class_any,
  validator = function(value) {
    # if (is.na(value)) return()
    # value <- 
    if (!testNumber(value,na.ok = TRUE))
      "must be a NA or a number"
  }, default = as.character(NA))

class_number_single <- new_property(
  class_numeric,
  validator = function(value) {
    if (length(value) != 1 || is.na(value))
      "must be a number"
  })

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
        paste("must be a list of", className)
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
    constructor = function(value) {
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
                    if (!testClass(x, "rdmlBaseType") &&
                        (
                          is.null(prop(x, subnodeName)) ||
                          is.na(prop(x, subnodeName)))
                    ) {
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
              }) |>
              #### (vals ~ vals[!sapply(vals, is.null)]) |>
              paste0(collapse = "")
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


cdnaSynthesisMethodType <- new_class(
  "cdnaSynthesisMethodType",
  parent = rdmlBaseType,
  properties = list(
    enzyme = class_character_na_nonempty_single,
    primingMethod = class_number_na_single,
    dnaseTreatment = class_logical,
    thermalCyclingConditions = class_id
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
    documentation = test_class_na_list("idType"),
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
    documentation = class_id,
    xRef = new_property(
      class_list,
      validator = function(value) {
        if (!all(sapply(value, \(x) testClass(x, "xRefType"))))
          "must be a list of xRefType"
      }
    ),
    type = new_property(
      targetTypeType
    ),
    amplificationEfficiencyMethod = class_character_na_nonempty_single,
    amplificationEfficiency = class_number_na_single,
    amplificationEfficiencySE = class_number_na_single,
    meltingTemperature = class_number_na_single,
    detectionLimit = class_number_na_single,
    dyeId = new_property(
      idType
    )
  )
)