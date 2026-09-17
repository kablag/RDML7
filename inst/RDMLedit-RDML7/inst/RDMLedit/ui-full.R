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


rdml7QpcrPanel <- function() {
  tabPanel(
    "qPCR",
    value = "adp",

    fluidRow(
      column(
        4,
        checkboxInput(
          "preprocessqPCR",
          "Preprocess",
          FALSE
        )
      ),
      column(
        4,
        selectInput(
          "smoothqPCRmethod",
          "Smoothing method",
          choices = c(
            "None" = "none",
            "LOWESS" = "lowess",
            "Moving average" = "mova",
            "Savitzky-Golay (recommended)" = "savgol",
            "Cubic spline smooth (recommended)" = "smooth",
            "Standard cubic spline smooth" = "spline",
            "Friedman's SuperSmoother" = "supsmu",
            "Whittaker 1" = "whit1",
            "Whittaker 2" = "whit2"
          ),
          selected = "savgol"
        )
      ),
      column(
        4,
        selectInput(
          "normqPCRmethod",
          "Normalization method",
          choices = c(
            "None" = "none",
            "Min-max" = "minm",
            "Maximum normalization" = "max",
            "Quantile normalization" = "luqn",
            "z-score" = "zscore"
          ),
          selected = "none"
        )
      )
    ),

    fluidRow(
      column(
        4,
        wellPanel(
          fluidRow(
            column(
              4,
              selectInput(
                "cqMethod",
                "Cq method",
                choices = c(
                  "None" = "none",
                  "Threshold" = "th",
                  "SDM" = "sdm"
                )
              )
            ),
            conditionalPanel(
              condition = "input.cqMethod == 'th'",
              column(
                4,
                checkboxInput(
                  "autoThLevel",
                  "Auto threshold",
                  TRUE
                )
              ),
              conditionalPanel(
                condition = "input.autoThLevel == false",
                column(
                  4,
                  uiOutput(
                    "thLevelsUI"
                  )
                )
              )
            )
          )
        )
      ),

      column(
        4,
        wellPanel(
          selectInput(
            "hookMethod",
            "Hook detection method",
            choices = c(
              "None" = "none",
              "hookreg" = "hookreg",
              "hookregNL" = "hookregNL",
              "Both" = "both"
            )
          )
        )
      ),

      column(
        4,
        actionButton(
          "recalcQpcrBtn",
          "Recalc",
          class = "btn-primary",
          width = "100%"
        )
      )
    ),

    wellPanel(
      fluidRow(
        column(
          2,
          selectInput(
            "colorqPCRby",
            "Color by",
            choices = c(
              "None" = "none",
              "Experiment" = "expId",
              "Run" = "runId",
              "Target" = "target",
              "Dye" = "targetDyeId",
              "Sample" = "sample",
              "Sample type" = "sampleType",
              "Position" = "position",
              "Hook" = "hook"
            )
          )
        ),
        column(
          2,
          selectInput(
            "shapeqPCRby",
            "Line type by",
            choices = c(
              "None" = "none",
              "Experiment" = "expId",
              "Run" = "runId",
              "Target" = "target",
              "Dye" = "targetDyeId",
              "Sample" = "sample",
              "Sample type" = "sampleType",
              "Position" = "position",
              "Hook" = "hook"
            )
          )
        ),
        column(
          2,
          selectInput(
            "showTargetsadp",
            "Show targets",
            multiple = TRUE,
            choices = character()
          )
        ),
        column(
          2,
          selectInput(
            "showCq",
            "Show Cq",
            choices = c(
              "None" = "none",
              "Yes" = "yes",
              "Mean for replicates" = "mean"
            )
          )
        ),
        column(
          2,
          checkboxInput(
            "logScale",
            "Log scale",
            FALSE
          )
        ),
        column(
          2,
          actionButton(
            "restoreRawAdpBtn",
            "Restore raw curves"
          )
        )
      ),

      fluidRow(
        column(
          6,
          plotly::plotlyOutput(
            "qPCRPlot",
            height = "520px"
          )
        ),
        column(
          6,
          wellPanel(
            fluidRow(
              column(
                4,
                selectInput(
                  "showqPCRExperiment",
                  "Experiment",
                  choices = character()
                )
              ),
              column(
                4,
                selectInput(
                  "showqPCRRun",
                  "Run",
                  choices = character()
                )
              )
            ),
            uiOutput(
              "qPCRPlateUI"
            ),
            tags$small(
              "With shinyMolBio installed this is the original interactive PCR plate. ",
              "Otherwise a multiple position selector is shown."
            )
          )
        )
      )
    ),

    DT::DTOutput(
      "qPCRDt"
    )
  )
}


rdml7MeltingPanel <- function() {
  tabPanel(
    "Melting Curves",
    value = "mdp",

    fluidRow(
      column(
        2,
        checkboxInput(
          "preprocessMelting",
          "Preprocess",
          FALSE
        )
      ),
      conditionalPanel(
        condition = "input.preprocessMelting == true",
        column(
          2,
          checkboxInput(
            "bgAdjMelting",
            "Adjust background signal",
            FALSE
          )
        ),
        conditionalPanel(
          condition = "input.bgAdjMelting == true",
          column(
            2,
            sliderInput(
              "bgRangeMelting",
              "Temperature background range (°C)",
              min = 0,
              max = 100,
              value = c(
                50,
                55
              ),
              step = 1
            )
          )
        ),
        column(
          2,
          checkboxInput(
            "minMaxMelting",
            "Min-max normalization",
            FALSE
          )
        ),
        column(
          2,
          sliderInput(
            "dfFactMelting",
            "Factor to smooth curve",
            min = 0.6,
            max = 1.1,
            value = 0.95,
            step = 0.05
          )
        )
      ),
      column(
        2,
        actionButton(
          "restoreRawMdpBtn",
          "Restore raw curves"
        )
      )
    ),

    wellPanel(
      fluidRow(
        column(
          2,
          selectInput(
            "colorMeltingBy",
            "Color by",
            choices = c(
              "None" = "none",
              "Experiment" = "expId",
              "Run" = "runId",
              "Target" = "target",
              "Dye" = "targetDyeId",
              "Sample" = "sample",
              "Sample type" = "sampleType",
              "Position" = "position"
            )
          )
        ),
        column(
          2,
          selectInput(
            "shapeMeltingBy",
            "Line type by",
            choices = c(
              "None" = "none",
              "Experiment" = "expId",
              "Run" = "runId",
              "Target" = "target",
              "Dye" = "targetDyeId",
              "Sample" = "sample",
              "Sample type" = "sampleType",
              "Position" = "position"
            )
          )
        ),
        column(
          2,
          selectInput(
            "showTargetsmdp",
            "Show targets",
            choices = character(),
            multiple = TRUE
          )
        )
      ),

      fluidRow(
        column(
          6,
          plotly::plotlyOutput(
            "meltingPlot",
            height = "620px"
          )
        ),
        column(
          6,
          wellPanel(
            fluidRow(
              column(
                4,
                selectInput(
                  "showMeltingExperiment",
                  "Experiment",
                  choices = character()
                )
              ),
              column(
                4,
                selectInput(
                  "showMeltingRun",
                  "Run",
                  choices = character()
                )
              )
            ),
            uiOutput(
              "meltingPlateUI"
            ),
            tags$small(
              "With shinyMolBio installed this is the original interactive PCR plate. ",
              "Otherwise a multiple position selector is shown."
            )
          )
        )
      )
    ),

    DT::DTOutput(
      "meltingDt"
    )
  )
}
