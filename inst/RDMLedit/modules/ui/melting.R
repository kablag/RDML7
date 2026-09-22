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
              "Temperature background range (В°C)",
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
