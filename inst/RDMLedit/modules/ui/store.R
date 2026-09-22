rdml7StorePanel <- function() {
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
      )
}
