library(shiny)
library(shinythemes)

for (module in c(
  "files.R",
  "metadata.R",
  "experiment.R",
  "qpcr.R",
  "melting.R",
  "store.R",
  "help.R",
  "log.R"
)) {
  source(file.path("modules", "ui", module), local = TRUE)
}

shinyUI(
  navbarPage(
    title = "RDML7 Editor",
    theme = shinytheme("cerulean"),

    rdml7FilesPanel(),
    rdml7MetadataMenu(),
    rdml7QpcrPanel(),
    rdml7MeltingPanel(),
    rdml7StorePanel(),
    rdml7HelpPanel(),

    footer = rdml7LogFooter()
  )
)
