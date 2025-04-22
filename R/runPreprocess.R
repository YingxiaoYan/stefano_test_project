# ------------------------------------------------------------------------------------------- #
# A Shiny module to run a preprocessing script allowing some selections
# ------------------------------------------------------------------------------------------- #

#' A Shiny UI module to run a preprocessing script allowing some selections
#'
#' @param uiId A string that identifies the UI module.
#' 
#' @returns A list of UI elements
#' @export
runPreprocessUI <- function(uiId) {
  ns <- shiny::NS(uiId)
  shiny::tagList(
    shiny::radioButtons(
      inputId = ns("weight"),
      label = "Weighting method",
      selected = "lowestR2",
      choiceValues = list("lowestR2", "1", "1/x", "1/x2"),
      choiceNames = list(
        shiny::HTML(paste0("Lowest R", shiny::tags$sup("2"))),
        "1",
        "1/x",
        shiny::HTML(paste0("1/x", shiny::tags$sup("2")))
      ),
    ),
    shiny::actionButton(ns("run_button"), label = "Preprocess data"),
    shiny::verbatimTextOutput(ns("script_output")),
    shiny::br(),
  )
}

#' A Shiny server module 
#'
#' @param serverId A string that identifies the server module.
#' @export
runPreprocessServer <- function(serverId) {
  shiny::moduleServer(serverId, function(input, output, session) {
    
    shiny::observeEvent(input[["run_button"]], {
      param_weight <- input[["weight"]]
      output[["script_output"]] <- shiny::renderText("")
      out <- tryCatch(
        withr::with_dir(
          new = "..",     # Every script is written to be executed at the root of the project
          utils::capture.output(source("code/scripts/preprocess.R", local = TRUE))
        ),
        warning = function(w) w,
        error = function(e) e
      )
      output[["script_output"]] <- shiny::renderText(paste(out, collapse = "\n"))
    })
  })
}

#' A Shiny app for module testing purposes
runPreprocessApp <- function() {
  ui <- shiny::fluidPage(
    runPreprocessUI("proc1"),
  )
  server <- function(input, output, session) {
    runPreprocessServer("proc1")
  }
  shiny::shinyApp(ui = ui, server = server)
}

# Module test on `code/` where `app.R` is located
# shiny::runApp(runPreprocessApp())
