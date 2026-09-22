rdml7FilesPanel <- function() {
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
      )
}
