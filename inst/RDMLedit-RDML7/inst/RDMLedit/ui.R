library(shiny)
library(shinythemes)

source("ui-full.R", local = TRUE)

shinyUI(
  navbarPage(
    title = "RDML7 Editor",
    theme = shinytheme("cerulean"),

    tabPanel(
      "Files",
      fluidRow(
        column(
          4,
          wellPanel(
            fileInput(
              "rdmlFiles",
              "Upload RDML / supported instrument files",
              multiple = TRUE
            ),
            selectizeInput(
              "rdmlFileSlct",
              "Open document",
              choices = character(),
              options = list(create = TRUE)
            ),
            fluidRow(
              column(
                6,
                actionButton(
                  "newRDMLBtn",
                  "New RDML"
                )
              ),
              column(
                6,
                actionButton(
                  "removeRDMLBtn",
                  "Remove"
                )
              )
            ),
            actionButton(
              "createSubversionRDMLBtn",
              "Create copy"
            )
          ),
          wellPanel(
            selectInput(
              "mergeRdmlsSlct",
              "Merge with",
              multiple = TRUE,
              choices = character()
            ),
            actionButton(
              "mergeBtn",
              "Merge"
            )
          )
        ),
        column(
          8,
          fluidRow(
            column(
              6,
              textInput(
                "dateMadeText",
                "Date made"
              )
            ),
            column(
              6,
              textInput(
                "dateUpdatedText",
                "Date updated"
              )
            )
          ),
          actionButton(
            "updateTopLevelBtn",
            "Apply metadata"
          ),
          hr(),
          h4("Structure"),
          plotOutput(
            "dendroRDMLplot",
            height = "500px"
          )
        )
      )
    ),

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
    ),

    rdml7QpcrPanel(),

    rdml7MeltingPanel(),

    tabPanel(
      "Store",
      wellPanel(
        downloadButton(
          "downloadRDML",
          "Store RDML"
        ),
        tags$p(
          "RDML7 writes the currently selected in-memory object."
        )
      )
    ),

    tabPanel(
      "Help",
      includeMarkdown("md/help.md")
    ),

    footer = wellPanel(
      h4("Log"),
      actionButton(
        "clearLogBtn",
        "Clear"
      ),
      verbatimTextOutput("logText")
    )
  )
)
