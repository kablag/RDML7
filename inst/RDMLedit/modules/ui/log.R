rdml7LogFooter <- function() {
      footer = wellPanel(
        h4("Log"),
        actionButton(
          "clearLogBtn",
          "Clear"
        ),
        verbatimTextOutput("logText")
      )
}
