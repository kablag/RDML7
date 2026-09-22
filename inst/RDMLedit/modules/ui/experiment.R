rdml7ExperimentPanel <- function() {
  tabPanel(
    "Experiment / Run / React / Data",

    fluidRow(
      column(
        3,
        wellPanel(
          h4("Experiment"),
          selectizeInput(
            "experimentSlct",
            "Select experiment",
            choices = character(),
            options = list(
              create = TRUE
            )
          ),
          textInput(
            "experimentIdText",
            "ID"
          ),
          textInput(
            "experimentDescriptionText",
            "Description"
          ),
          selectInput(
            "experimentDocumentationSlct",
            "Documentation",
            choices = character(),
            multiple = TRUE
          ),
          actionButton(
            "saveExperimentBtn",
            "Save experiment"
          ),
          actionButton(
            "removeExperimentBtn",
            "Remove experiment"
          )
        )
      ),

      column(
        3,
        wellPanel(
          h4("Run"),
          selectizeInput(
            "runSlct",
            "Select run",
            choices = character(),
            options = list(
              create = TRUE
            )
          ),
          textInput(
            "runIdText",
            "ID"
          ),
          textInput(
            "runDescriptionText",
            "Description"
          ),
          selectInput(
            "runDocumentationSlct",
            "Documentation",
            choices = character(),
            multiple = TRUE
          ),
          selectInput(
            "runExperimenterSlct",
            "Experimenter",
            choices = character(),
            multiple = TRUE
          ),
          textInput(
            "runInstrumentText",
            "Instrument"
          ),
          textInput(
            "runDataCollectionSoftwareNameText",
            "Data collection software"
          ),
          textInput(
            "runDataCollectionSoftwareVersionText",
            "Software version"
          ),
          textInput(
            "runBackgroundDeterminationMethodText",
            "Background determination method"
          ),
          textInput(
            "runCqDetectionMethodText",
            "Cq detection method"
          ),
          selectInput(
            "runTccSlct",
            "Thermal cycling conditions",
            choices = character()
          ),
          fluidRow(
            column(
              6,
              numericInput(
                "runRowsText",
                "Rows",
                value = 8,
                step = 1
              )
            ),
            column(
              6,
              numericInput(
                "runColumnsText",
                "Columns",
                value = 12,
                step = 1
              )
            )
          ),
          fluidRow(
            column(
              6,
              selectInput(
                "runRowLabelSlct",
                "Row label",
                choices = c(
                  "ABC",
                  "123",
                  "A1a1"
                )
              )
            ),
            column(
              6,
              selectInput(
                "runColumnLabelSlct",
                "Column label",
                choices = c(
                  "123",
                  "ABC",
                  "A1a1"
                )
              )
            )
          ),
          textInput(
            "runDateText",
            "Run date"
          ),
          actionButton(
            "saveRunBtn",
            "Save run"
          ),
          actionButton(
            "removeRunBtn",
            "Remove run"
          )
        )
      ),

      column(
        3,
        wellPanel(
          h4("React"),
          selectizeInput(
            "reactSlct",
            "Select react",
            choices = character(),
            options = list(
              create = TRUE
            )
          ),
          textInput(
            "reactIdText",
            "ID / position"
          ),
          selectInput(
            "reactSampleSlct",
            "Sample",
            choices = character()
          ),
          actionButton(
            "saveReactBtn",
            "Save react"
          ),
          actionButton(
            "removeReactBtn",
            "Remove react"
          )
        )
      ),

      column(
        3,
        wellPanel(
          h4("Data"),
          selectizeInput(
            "dataSlct",
            "Select data",
            choices = character(),
            options = list(
              create = TRUE
            )
          ),
          selectInput(
            "dataTarSlct",
            "Target",
            choices = character()
          ),
          textInput(
            "dataCqText",
            "Cq"
          ),
          textInput(
            "dataMeltTempText",
            "Melting temperature(s)",
            placeholder = "87.8;82.2"
          ),
          textInput(
            "dataExclText",
            "Excluded"
          ),
          textInput(
            "dataEndPtText",
            "End point"
          ),
          textInput(
            "dataBgFluorText",
            "Background fluorescence"
          ),
          textInput(
            "dataBgFluorSlpText",
            "Background fluorescence slope"
          ),
          textInput(
            "dataQuantFluorText",
            "Quantification fluorescence"
          ),
          actionButton(
            "saveDataBtn",
            "Save data"
          ),
          actionButton(
            "removeDataBtn",
            "Remove data"
          )
        )
      )
    ),

    verbatimTextOutput(
      "experimentPathPreview"
    )
  )
}


