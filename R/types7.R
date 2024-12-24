library(S7)
library(checkmate)

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


asXMLnodes <- new_generic("asXMLnodes", "x")

method(asXMLnodes, rdmlEnum) <- function(x
                                         # , nodeName
                                         ) {
  # assertString(nodeName)
  nodeName <- class(x)[1]
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


rdmlBaseType <- new_class(
  "rdmlBaseType",
  abstract = TRUE)


method(asXMLnodes, rdmlBaseType) <- function(x
                                             # , 
                                             # nodeName,
                                             # attribute = ""
                                             ) {
  # assertString(nodeName)
  # assertString(attribute)
  nodeName <- class(x)[1]
  subnodesNames <- names(props(x))
  sprintf("<%s%s>%s</%s>",
          nodeName, #node name
          #attribute
          {
            if (subnodesNames[1] == "id" ||
                subnodesNames[1] == "targetId") {
              attribute <- sprintf(" %s=%s", 
                                   subnodesNames[1],
                                   prop(x, subnodesNames[1]))
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
                    asXMLnodes(prop(x, subnodeName)
                               # , subnodeName
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

experimenterType <- new_class(
  "experimenterType",
  parent = rdmlBaseType,
  properties = list(
    id = class_character_nonempty_single,
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
    id = class_character_nonempty_single,
    text = class_character_na_nonempty_single
  )
)

DyeChemistryType <- 
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
    id = class_character_nonempty_single,
    description = class_character_na_nonempty_single,
    dyeChemistry = 
      new_property(
        class_any,
        validator = function(value) {
          if (
            !(testClass(value, 
                        "DyeChemistryType") ||
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
