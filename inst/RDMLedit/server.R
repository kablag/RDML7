library(shiny)

source(
  "helpers.R",
  local = TRUE
)

source(
  "analysis-helpers.R",
  local = TRUE
)

if (!requireNamespace("RDML7", quietly = TRUE)) {
  stop(
    "RDML7 must be installed to run RDML7 Editor.",
    call. = FALSE
  )
}

if (!requireNamespace("S7", quietly = TRUE)) {
  stop(
    "S7 must be installed to run RDML7 Editor.",
    call. = FALSE
  )
}

shinyServer(function(input, output, session) {

  values <- reactiveValues(
    RDMLs = list(),
    rdml = NULL,
    log = character(),
    rawCurves = list(),
    hookResults = list(),
    thresholds = list()
  )

  updLog <- function(message) {
    message <- sprintf(
      "[%s] %s",
      format(
        Sys.time(),
        "%H:%M:%S"
      ),
      message
    )

    cat(
      message,
      "\n"
    )

    isolate({
      values$log <- c(
        values$log,
        message
      )
    })
  }

  runSafe <- function(expr) {
    tryCatch(
      expr,
      error = function(e) {
        updLog(
          conditionMessage(e)
        )
        NULL
      }
    )
  }

  activeName <- reactive({
    input$rdmlFileSlct
  })

  commitActive <- function() {
    name <- isolate(
      input$rdmlFileSlct
    )

    if (
      !is.null(values$rdml) &&
      !is.null(name) &&
      nzchar(name)
    ) {
      values$RDMLs[[name]] <- editor_clone(
        values$rdml
      )
    }

    invisible(NULL)
  }

  refreshFileSelectors <- function(selected = NULL) {
    choices <- names(
      values$RDMLs
    )

    if (
      is.null(selected) &&
      length(choices)
    ) {
      selected <- tail(
        choices,
        1L
      )
    }

    updateSelectizeInput(
      session,
      "rdmlFileSlct",
      choices = choices,
      selected = selected,
      server = TRUE
    )

    current <- isolate(
      input$rdmlFileSlct
    )

    updateSelectInput(
      session,
      "mergeRdmlsSlct",
      choices = setdiff(
        choices,
        current
      )
    )
  }

  refreshMetadataSelectors <- function() {
    if (is.null(values$rdml)) {
      return(invisible(NULL))
    }

    updateSelectizeInput(
      session,
      "idSlct",
      choices = editor_collection_names(
        values$rdml,
        "id"
      ),
      server = TRUE
    )

    updateSelectizeInput(
      session,
      "experimenterSlct",
      choices = editor_collection_names(
        values$rdml,
        "experimenter"
      ),
      server = TRUE
    )

    updateSelectizeInput(
      session,
      "documentationSlct",
      choices = editor_collection_names(
        values$rdml,
        "documentation"
      ),
      server = TRUE
    )

    dyes <- editor_collection_names(
      values$rdml,
      "dye"
    )

    updateSelectizeInput(
      session,
      "dyeSlct",
      choices = dyes,
      server = TRUE
    )

    updateSelectInput(
      session,
      "targetDyeIdSlct",
      choices = c(
        "",
        dyes
      )
    )

    updateSelectizeInput(
      session,
      "sampleSlct",
      choices = editor_collection_names(
        values$rdml,
        "sample"
      ),
      server = TRUE
    )

    updateSelectizeInput(
      session,
      "targetSlct",
      choices = editor_collection_names(
        values$rdml,
        "target"
      ),
      server = TRUE
    )

    invisible(NULL)
  }

  loadActive <- function(name) {
    if (
      is.null(name) ||
      !nzchar(name) ||
      !(name %in% names(values$RDMLs))
    ) {
      return(invisible(NULL))
    }

    values$rdml <- editor_clone(
      values$RDMLs[[name]]
    )

    updateTextInput(
      session,
      "dateMadeText",
      value = editor_display(
        values$rdml$dateMade
      )
    )

    updateTextInput(
      session,
      "dateUpdatedText",
      value = editor_display(
        values$rdml$dateUpdated
      )
    )

    refreshMetadataSelectors()
    invisible(NULL)
  }



  serverModules <- c(
    "files.R", "rdml-id.R", "experimenter.R", "documentation.R",
    "dye.R", "sample.R", "target.R", "experiment.R", "object-tree.R",
    "analysis-shared.R", "qpcr.R", "melting.R", "store.R", "log.R"
  )
  for (module in serverModules) {
    source(file.path("modules", "server", module), local = TRUE)
  }

  rdml7FilesServer(environment())
  rdml7RdmlIdServer(environment())
  rdml7ExperimenterServer(environment())
  rdml7DocumentationServer(environment())
  rdml7DyeServer(environment())
  rdml7SampleServer(environment())
  rdml7TargetServer(environment())
  rdml7ExperimentServer(environment())
  rdml7ObjectTreeServer(environment())
  rdml7AnalysisSharedServer(environment())
  rdml7QpcrServer(environment())
  rdml7MeltingServer(environment())
  rdml7StoreServer(environment())
  rdml7LogServer(environment())
})
