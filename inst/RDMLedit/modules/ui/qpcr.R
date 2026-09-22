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


