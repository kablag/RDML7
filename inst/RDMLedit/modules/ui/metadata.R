rdml7MetadataMenu <- function() {
      navbarMenu(
        "Metadata",
  
        tabPanel(
          "RDML ID",
          fluidRow(
            column(
              4,
              selectizeInput(
                "idSlct",
                "ID record",
                choices = character(),
                options = list(create = TRUE)
              ),
              textInput(
                "idPublisherText",
                "Publisher"
              ),
              textInput(
                "idSerialNumberText",
                "Serial number"
              ),
              textInput(
                "idMD5HashText",
                "MD5 hash"
              ),
              actionButton(
                "saveIDBtn",
                "Save"
              ),
              actionButton(
                "removeIDBtn",
                "Remove"
              )
            ),
            column(
              8,
              verbatimTextOutput("idPreview")
            )
          )
        ),
  
        tabPanel(
          "Experimenter",
          fluidRow(
            column(
              4,
              selectizeInput(
                "experimenterSlct",
                "Experimenter",
                choices = character(),
                options = list(create = TRUE)
              ),
              textInput(
                "experimenterIdText",
                "ID"
              ),
              textInput(
                "experimenterFirstNameText",
                "First name"
              ),
              textInput(
                "experimenterLastNameText",
                "Last name"
              ),
              textInput(
                "experimenterEmailText",
                "Email"
              ),
              textInput(
                "experimenterLabNameText",
                "Lab name"
              ),
              textInput(
                "experimenterLabAddressText",
                "Lab address"
              ),
              actionButton(
                "saveExperimenterBtn",
                "Save"
              ),
              actionButton(
                "removeExperimenterBtn",
                "Remove"
              )
            ),
            column(
              8,
              verbatimTextOutput("experimenterPreview")
            )
          )
        ),
  
        tabPanel(
          "Documentation",
          fluidRow(
            column(
              4,
              selectizeInput(
                "documentationSlct",
                "Documentation",
                choices = character(),
                options = list(create = TRUE)
              ),
              textInput(
                "documentationIdText",
                "ID"
              ),
              textAreaInput(
                "documentationTextText",
                "Text",
                rows = 8
              ),
              actionButton(
                "saveDocumentationBtn",
                "Save"
              ),
              actionButton(
                "removeDocumentationBtn",
                "Remove"
              )
            ),
            column(
              8,
              verbatimTextOutput("documentationPreview")
            )
          )
        ),
  
        tabPanel(
          "Dye",
          fluidRow(
            column(
              4,
              selectizeInput(
                "dyeSlct",
                "Dye",
                choices = character(),
                options = list(create = TRUE)
              ),
              textInput(
                "dyeIdText",
                "ID"
              ),
              textInput(
                "dyeDescriptionText",
                "Description"
              ),
              actionButton(
                "saveDyeBtn",
                "Save"
              ),
              actionButton(
                "removeDyeBtn",
                "Remove"
              )
            ),
            column(
              8,
              verbatimTextOutput("dyePreview")
            )
          )
        ),
  
        tabPanel(
          "Sample",
          fluidRow(
            column(
              4,
              selectizeInput(
                "sampleSlct",
                "Sample",
                choices = character(),
                options = list(create = TRUE)
              ),
              textInput(
                "sampleIdText",
                "ID"
              ),
              textInput(
                "sampleDescriptionText",
                "Description"
              ),
              selectInput(
                "sampleTypeSlct",
                "Type",
                choices = c(
                  "",
                  "unkn",
                  "ntc",
                  "nac",
                  "std",
                  "ntp",
                  "nrt",
                  "pos",
                  "opt"
                )
              ),
              checkboxInput(
                "sampleInterRunCalibratorChk",
                "Inter-run calibrator",
                FALSE
              ),
              checkboxInput(
                "sampleCalibratorSampleChk",
                "Calibrator sample",
                FALSE
              ),
              actionButton(
                "saveSampleBtn",
                "Save"
              ),
              actionButton(
                "removeSampleBtn",
                "Remove"
              )
            ),
            column(
              8,
              p(
                "The RDML7 port edits common sample properties here. ",
                "Existing nested properties (documentation, xRef, annotations, ",
                "quantity, cDNA synthesis, template quantity) are preserved."
              ),
              verbatimTextOutput("samplePreview")
            )
          )
        ),
  
        tabPanel(
          "Target",
          fluidRow(
            column(
              4,
              selectizeInput(
                "targetSlct",
                "Target",
                choices = character(),
                options = list(create = TRUE)
              ),
              textInput(
                "targetIdText",
                "ID"
              ),
              textInput(
                "targetDescriptionText",
                "Description"
              ),
              selectInput(
                "targetTypeSlct",
                "Type",
                choices = c(
                  "",
                  "toi",
                  "ref"
                )
              ),
              selectInput(
                "targetDyeIdSlct",
                "Dye",
                choices = character()
              ),
              actionButton(
                "saveTargetBtn",
                "Save"
              ),
              actionButton(
                "removeTargetBtn",
                "Remove"
              )
            ),
            column(
              8,
              p(
                "Existing target sequence/xRef/documentation and assay properties ",
                "are preserved when common properties are edited."
              ),
              verbatimTextOutput("targetPreview")
            )
          )
        ),
  
        rdml7ExperimentPanel(),
  
        tabPanel(
          "Object tree",
          p(
            "Read-only S7 structure view. This replaces the old editor's ",
            "dependence on R6 private fields."
          ),
          verbatimTextOutput("rdmlObjectTree")
        )
      )
}
